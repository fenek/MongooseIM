-module(vote_timers).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, stop/0]).
-export([set_timer/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          timers = []
         }).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    TimersSpec = #{
      id => ?SERVER,
      start => {?MODULE, start_link, []},
      restart => transient,
      shutdown => 5000,
      type => worker,
      modules => [?MODULE]
     },
    {ok, _} = supervisor:start_child(ejabberd_sup, TimersSpec).

stop() ->
    supervisor:terminate_child(ejabberd_sup, ?SERVER),
    supervisor:delete_child(ejabberd_sup, ?SERVER).

-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Error :: any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

set_timer(PollID, TimeLimit, CallbackFun) ->
    gen_server:cast(?SERVER, {set_timer, PollID, TimeLimit, CallbackFun}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({set_timer, PollID, TimeLimit, CallbackFun}, #state{ timers = Timers } = State) ->
    case lists:keyfind(PollID, 1, Timers) of
        false ->
            TRef = erlang:start_timer(TimeLimit, self(), {poll_timeout, PollID, CallbackFun}),
            {noreply, State#state{ timers = [{PollID, TRef} | Timers] }};
        _ ->
            {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _TRef, {poll_timeout, PollID, CallbackFun}}, State) ->
    Votes = mod_vote:take_votes(PollID),
    spawn(fun() -> CallbackFun(PollID, Votes) end),
    {noreply, State#state{ timers = lists:keydelete(PollID, 1, State#state.timers) }};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================




