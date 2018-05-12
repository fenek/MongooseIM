-module(mod_vote).

-include("jlib.hrl").

-behaviour(gen_mod).

-export([start/2, stop/1]).
-export([handle_iq/4]).
-export([start_poll/3, stop_poll/1, store_vote/3, peek_votes/1, take_votes/1]).

-define(NS_VOTE, <<"urn:xmpp:vote">>).
-define(PTABLE, polls).

%% --------------------------------------------------------
%% API
%% --------------------------------------------------------

-spec start_poll(ID :: atom(), TimeLimit :: pos_integer(), CallbackFun :: fun()) -> true.
start_poll(ID, TimeLimit, CallbackFun) ->
    VotesTable = votes_table_name(ID),
    ets:new(VotesTable, [named_table, public]),
    ets:insert(?PTABLE, {ID, TimeLimit, CallbackFun, VotesTable}).

-spec stop_poll(ID :: atom()) -> true | {error, not_found}.
stop_poll(ID) ->
    case ets:take(?PTABLE, ID) of
        [{_ID, _TimeLimit, _CallbackFun, VotesTable}] ->
            ets:delete(VotesTable);
        [] ->
            {error, not_found}
    end.

store_vote(ID, User, Vote) ->
    [{_, TimeLimit, CallbackFun, VotesTable}] = ets:lookup(?PTABLE, ID),
    insert_vote(VotesTable, jid:to_binary(User), Vote),
    vote_timers:set_timer(ID, TimeLimit, CallbackFun).

peek_votes(ID) ->
    [{_, _, _, VotesTable}] = ets:lookup(?PTABLE, ID),
    Votes = ets:tab2list(VotesTable),
    [ Value || {_, Value} <- Votes ].

-spec take_votes(ID :: atom()) -> VotesValues :: [term()].
take_votes(ID) ->
    [{_, _, _, VotesTable}] = ets:lookup(?PTABLE, ID),
    Votes = ets:tab2list(VotesTable),
    ets:delete_all_objects(VotesTable),
    [ Value || {_, Value} <- Votes ].

%% --------------------------------------------------------
%% IQ handling
%% --------------------------------------------------------

-spec handle_iq(From :: jid:jid(),
                To :: jid:jid(),
                Acc :: mongoose_acc:t(),
                IQ :: jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq() | ignore}.
handle_iq(From, _To, Acc, #iq{ type = set, sub_el = QueryEl } = IQ) ->
    ID = binary_to_existing_atom(exml_query:path(QueryEl, [{element, <<"poll-id">>}, cdata]), utf8),
    Vote = exml_query:path(QueryEl, [{element, <<"vote">>}, cdata]),
    [{_, TimeLimit, CallbackFun, VotesTable}] = ets:lookup(?PTABLE, ID),
    store_vote(VotesTable, jid:to_binary(From), Vote),
    vote_timers:set_timer(ID, TimeLimit, CallbackFun),
    {Acc, IQ#iq{ type = result, sub_el = QueryEl#xmlel{ children = [] } }}.

%% --------------------------------------------------------
%% gen_mod callbacks
%% --------------------------------------------------------

start(_Host, _Opts) ->
    ets:new(?PTABLE, [named_table, public]),
    vote_timers:start().
%    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_VOTE, ?MODULE, handle_iq, no_queue).

stop(_Host) ->
%    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VOTE),
    vote_timers:stop(),
    lists:foreach(fun({ID, _, _, _}) ->
                          catch stop_poll(ID)
                  end, ets:tab2list(?PTABLE)),
    ets:delete(?PTABLE).

%% --------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------

votes_table_name(ID) ->
    binary_to_atom(<<"votes_", (atom_to_binary(ID, utf8))/binary>>, utf8).

insert_vote(VotesTable, User, Vote) ->
    ets:insert(VotesTable, {User, Vote}).

