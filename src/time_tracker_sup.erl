-module(time_tracker_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(WORKER(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags = #{
    strategy => one_for_all,
    intensity => 5,
    period => 10
  },
  ChildSpecs = [
    ?WORKER(tt_db_worker),
    ?WORKER(tt_rabbitmq_worker),
    ?WORKER(tt_rpc_server),
    ?WORKER(tt_rpc_client)
  ],
  {ok, {SupFlags, ChildSpecs}}.
