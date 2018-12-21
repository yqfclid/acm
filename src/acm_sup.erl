%%%-------------------------------------------------------------------
%% @doc acm4erl top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(acm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Procs1 = {acm_listener, {acm_listener, start_link, []}, transient, 5000, worker, []},
    {ok, {{simple_one_for_one, 10000, 3000}, [Procs1]}}.
%%====================================================================
%% Internal functions
%%====================================================================
