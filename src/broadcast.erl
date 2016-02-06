-module(broadcast).
-behaviour(gen_server).

%% API.
-export([start_pool/0, bump/0, bump/1]).
%% gen_server.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          counter = 0
}).

%% API.

start_pool() ->
    wpool:start_pool(
      ?MODULE, [
                {workers, 10},
                {worker, {?MODULE, []}}
               ]
    ).

bump() ->
    wpool:cast(?MODULE, bump).

bump(Pid) ->
    gen_server:cast(Pid, bump).

%%% gen_server.

init(_) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(bump, State = #state{counter=Counter}) ->
    Counter1 = Counter + 1,
    State1 = State#state{counter=Counter1},
    error_logger:info_msg("Counter of ~p is at ~p~n", [self(), Counter1]),
    {noreply, State1};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

