-module(mod_2048).

-behaviour(gen_mod).
-behaviour(gen_server).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_muc_room.hrl").

-define(NS_2048, <<"urn:xmpp:2048">>).
-define(NS_2048_VOTE, <<"urn:xmpp:2048#vote">>).
-define(NS_2048_VOTES, <<"urn:xmpp:2048#votes">>).
-define(NS_2048_MOVE, <<"urn:xmpp:2048#move">>).
-define(NS_2048_BOARD, <<"urn:xmpp:2048#board">>).
-define(NS_2048_PLAYERS_SINCE_START, <<"urn:xmpp:2048#players-since-start">>).
-define(NS_2048_NEW_TILES, <<"urn:xmpp:2048#new-tiles">>).
-define(NS_2048_WON, <<"urn:xmpp:2048#won">>).
-define(NS_2048_LOST, <<"urn:xmpp:2048#lost">>).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/2, stop/1]).
-export([process_votes/2]).
-export([process_room_iq/4, room_new_user/4]).

%% --------------------------------------------------------
%% API
%% --------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

process_votes(_PollID, Votes) ->
    CountedVotes = aggregate_votes(Votes),
    [{_, TopCount} | _] = SortedVotes = lists:reverse(lists:keysort(2, CountedVotes)),
    TopVotes = lists:takewhile(fun({_, Count}) -> Count == TopCount end, SortedVotes),
    {TopOption, _} = lists:nth(rand:uniform(length(TopVotes)), TopVotes),
   
    case call({move, TopOption}) of
        {ok, Result} ->
            MoveChild = #xmlel{ name = atom_to_binary(TopOption, utf8) },
            send_service_stanza(?NS_2048_MOVE, [MoveChild]),
            
            case Result of
                {new_tile, NewTile} ->
                    send_service_stanza(?NS_2048_NEW_TILES, [make_tile_el(NewTile)]);
                won ->
                    send_service_stanza(?NS_2048_WON, []);
                {lost, NewTile} ->
                    send_service_stanza(?NS_2048_NEW_TILES, [make_tile_el(NewTile)]),
                    send_service_stanza(?NS_2048_LOST, []);
                _ ->
                    ok
            end;
        Error ->
            ?ERROR_MSG("~p", [Error]),
            ErrorChild = #xmlel{ name = <<"error">> },
            send_service_stanza(?NS_2048_MOVE, [ErrorChild])
    end.

%% --------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------

init([]) ->
    State = reset_game(#{}),
    set_broadcast_timer(),
    {ok, State}.

handle_call({move, Direction}, _From, #{ board_state := BoardState0,
                                         move_result := MoveResult } = State)
  when MoveResult /= won, MoveResult /= lost ->
    BoardState1 = erl2048_lib:move(BoardState0, Direction),
    {BoardState, Reply} =
    case erl2048_lib:has_changed(BoardState1) of
        true ->
            BoardState2 = erl2048_lib:new_tile(BoardState1),
            NewTile = erl2048_lib:last_added(BoardState2),
            case erl2048_lib:won_or_lost(BoardState2) of
                won ->
                    mod_vote:stop_poll(game2048),
                    timer:send_after(10000, self(), reset),
                    {BoardState2#{ game_over => true }, won};
                lost ->
                    mod_vote:stop_poll(game2048),
                    timer:send_after(10000, self(), reset),
                    {BoardState2#{ game_over => true }, {lost, NewTile}};
                neither ->
                    {BoardState2, {new_tile, NewTile}}
            end;
        false ->
            {BoardState1, no_change}
    end,
    {reply, {ok, Reply}, State#{ board_state := BoardState,
                                 move_result := Reply }};
handle_call({move, _}, _From, State) ->
    {reply, {ok, no_change}, State};
handle_call(get_board, _From, #{ board_state := BoardState } = State) ->
    {reply, {ok, erl2048_lib:to_list(BoardState)}, State};
handle_call(get_players_since_start, _From,
            #{ players_since_start := PlayersSinceStart } = State) ->
    {reply, {ok, PlayersSinceStart}, State};
handle_call(get_move_result, _From, #{ move_result := Result } = State) ->
    {reply, Result, State};
handle_call(Unknown, _From, State) ->
    ?ERROR_MSG("unknown: ~p", [Unknown]),
    {reply, {error, unknown}, State}.

handle_cast(Unknown, State) ->
    ?ERROR_MSG("unknown: ~p", [Unknown]),
    {noreply, State}.

handle_info(reset, State) ->
    #{ board_state := BoardState,
       players_since_start := PlayersSinceStart } = NState = reset_game(State),
    BoardStanza = make_board_stanza(erl2048_lib:to_list(BoardState)),
    mod_muc:send_service_stanza(room_jid(), BoardStanza),
    PlayersSinceStartStanza = make_players_since_start_stanza(PlayersSinceStart),
    mod_muc:send_service_stanza(room_jid(), PlayersSinceStartStanza),
    {noreply, NState};
handle_info(broadcast, State) ->
    catch send_aggregated_votes(State),
    set_broadcast_timer(),
    {noreply, State};
handle_info(Unknown, State) ->
    ?ERROR_MSG("unknown: ~p", [Unknown]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------
%% IQ & hook handling
%% --------------------------------------------------------

process_room_iq(From, #jid{ luser = <<"game2048">> } = _Room, Acc, #iq{ sub_el = QueryEl } = IQ) ->
    VoteBin = exml_query:path(QueryEl, [{element, <<"vote">>}, cdata]),
    try
        mod_vote:store_vote(game2048, From, vote2atom(VoteBin))
    catch
        C:R ->
            ?DEBUG("event=vote_processing_crashed, class=~p, reason=~p", [C, R])
    end,
    {Acc, IQ#iq{ type = result, sub_el = QueryEl#xmlel{ children = [] } }};
process_room_iq(_From, _Room, Acc, _IQ) ->
    {Acc, ignore}.

room_new_user(Acc0, <<"game2048">>, _User, _Nick) ->
    %% Reverse order because mongoose_acc:append adds elements as list head

    Acc1 =
    case call(get_move_result) of
        won -> mongoose_acc:append(mod_muc, extra_stanzas, make_x_message(?NS_2048_WON, []), Acc0);
        lost -> mongoose_acc:append(mod_muc, extra_stanzas, make_x_message(?NS_2048_LOST, []), Acc0);
        _ -> Acc0
    end,

    {ok, Board} = call(get_board),
    {ok, PlayersSinceStart} = call(get_players_since_start),
    BoardStanza = make_board_stanza(Board),
    PlayersSinceStartStanza = make_players_since_start_stanza(PlayersSinceStart),
    mongoose_acc:append(mod_muc, extra_stanzas, [BoardStanza, PlayersSinceStartStanza], Acc1);
room_new_user(Acc, _Room, _User, _Nick) ->
    Acc.

%% --------------------------------------------------------
%% gen_mod callbacks
%% --------------------------------------------------------

start(Host, Opts) ->
    MUCHost = gen_mod:get_opt_subhost(Host, Opts, mod_muc:default_host()),
    AdminJID = jid:make(<<"admin">>, <<"localhost">>, <<>>),
    RoomOpts = [
                {persistent, true}
               ],
    ok = mod_muc:create_instant_room(Host, <<"game2048">>, AdminJID, <<"GM2048">>, RoomOpts),

    ChildSpec = {?MODULE, {?MODULE, start_link, []}, transient, 5000, worker, [?MODULE]},
    {ok, _} = supervisor:start_child(ejabberd_sup, ChildSpec),

    gen_iq_handler:add_iq_handler(mod_muc_iq, MUCHost, ?NS_2048_VOTE,
                                  ?MODULE, process_room_iq, parallel),
    ejabberd_hooks:add(room_new_user, MUCHost, ?MODULE, room_new_user, 50).

stop(Host) ->
    MUCHost = gen_mod:get_module_opt_subhost(Host, ?MODULE, mod_muc:default_host()),
    ejabberd_hooks:delete(room_new_user, MUCHost, ?MODULE, room_new_user, 50),
    gen_iq_handler:remove_iq_handler(mod_muc_iq, MUCHost, ?NS_2048_VOTE),
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE).

%% --------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------

send_aggregated_votes(#{ game_over := true }) ->
    ok;
send_aggregated_votes(#{ game_start := GameStart }) ->
    AllVotes = mod_vote:peek_votes(game2048),
    CountedVotes = aggregate_votes(AllVotes),
    VotesChildren0 = [ #xmlel{ name = atom_to_binary(Opt, utf8),
                              children = [#xmlcdata{ content = integer_to_binary(Count) }] }
                      || {Opt, Count} <- CountedVotes ],
    GameTime = timer:now_diff(os:timestamp(), GameStart) div 1000000,
    VotesChildren = [ #xmlel{ name = <<"gametime">>,
                              children = [#xmlcdata{ content = integer_to_binary(GameTime) }] }
                      | VotesChildren0],
    send_service_stanza(?NS_2048_VOTES, VotesChildren).

make_x_message(XMLNS, XChildren) ->
    XEl = #xmlel{ name = <<"x">>, attrs = [{<<"xmlns">>, XMLNS}], children = XChildren },
    #xmlel{ name = <<"message">>,
            attrs = [ {<<"type">>, <<"groupchat">>} ],
            children = [XEl] }.

send_service_stanza(XMLNS, XChildren) ->
    Stanza = make_x_message(XMLNS, XChildren),
    mod_muc:send_service_stanza(room_jid(), Stanza).

make_board_stanza(Board) ->
    TilesEls = [
                #xmlel{ name = <<"tile">>,
                        attrs = [ {<<"x">>, integer_to_binary(X)},
                                  {<<"y">>, integer_to_binary(Y)} ],
                        children = [#xmlcdata{ content = tile_to_binary(Tile) }] }
                || {{X, Y}, Tile} <- Board ],
    XEl = #xmlel{ name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_2048_BOARD}],
                  children = TilesEls },
    #xmlel{ name = <<"message">>,
            attrs = [ {<<"type">>, <<"groupchat">>} ],
            children = [XEl] }.

make_tile_el({{AddedX, AddedY}, AddedValue}) ->
    #xmlel{ name = <<"tile">>,
            attrs = [ {<<"x">>, integer_to_binary(AddedX)},
                      {<<"y">>, integer_to_binary(AddedY)} ],
            children = [#xmlcdata{ content = tile_to_binary(AddedValue) }] }.

make_players_since_start_stanza(Players) ->
    PlayersEls = [ #xmlel{ name = <<"player">>,
                           children = [#xmlcdata{ content = Player }] }
                   || Player <- Players ],
    XEl = #xmlel{ name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_2048_PLAYERS_SINCE_START}],
                  children = PlayersEls },
    #xmlel{ name = <<"message">>,
            attrs = [ {<<"type">>, <<"groupchat">>} ],
            children = [XEl] }.

set_broadcast_timer() ->
    erlang:send_after((countdown() * 1000) div 4, self(), broadcast).

countdown() -> 2.

vote2atom(<<"up">>) -> up;
vote2atom(<<"down">>) -> down;
vote2atom(<<"left">>) -> left;
vote2atom(<<"right">>) -> right.

aggregate_votes([]) ->
    [];
aggregate_votes(Votes) ->
    [FirstVote | _] = SortedVotes = lists:sort(Votes),
    aggregate_votes(SortedVotes, FirstVote, 0).

aggregate_votes([Same | RVotes], Same, Count) ->
    aggregate_votes(RVotes, Same, Count + 1);
aggregate_votes([Different | _] = Votes, Option, Count) ->
    [{Option, Count} | aggregate_votes(Votes, Different, 0)];
aggregate_votes([], Option, Count) ->
    [{Option, Count}].

tile_to_binary(null) -> <<"null">>;
tile_to_binary(Value) -> integer_to_binary(Value).

call(Msg) ->
    gen_server:call(?MODULE, Msg).

reset_game(State) ->
    mod_vote:stop_poll(game2048),
    mod_vote:start_poll(game2048, countdown() * 1000, fun ?MODULE:process_votes/2),
    PlayersSinceStart = case mod_muc_room:get_room_users(room_jid()) of
                            {ok, Players} -> filter_out_guests(Players);
                            {error, _} -> []
                        end,
    State#{ board_state => erl2048_lib:init(undefined),
            move_result => undefined,
            game_start => os:timestamp(),
            players_since_start => PlayersSinceStart,
            game_over => false }.

filter_out_guests([]) ->
    [];
filter_out_guests([#user{ nick = <<"--GUEST--", _/binary>> } | RPlayers]) ->
    filter_out_guests(RPlayers);
filter_out_guests([#user{ nick = Player } | RPlayers]) ->
    [Player | filter_out_guests(RPlayers)].

room_jid() ->
    jid:make(<<"game2048">>, <<"muc.localhost">>, <<>>).

