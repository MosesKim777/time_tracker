-module(tt_rpc_client).

-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("time_tracker.hrl").

%% API
-export([
  start_link/0,
  call/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {client_pid}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

call(Payload) ->
  gen_server:call(?MODULE, {call, Payload}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  case tt_rabbitmq_worker:get_connection() of
    {ok, Connection} ->
      Pid = amqp_rpc_client:start_link(Connection, ?QUEUE),
      {ok, #state{client_pid = Pid}};
    {error, Reason} ->
      {error, {failed_to_get_connection, Reason}};
    {'EXIT', Reason} ->
      {error, {rabbitmq_worker_not_available, Reason}}
  end.

handle_call({call, Payload}, _From, State) ->
  Reply = send_msg(State#state.client_pid, Payload),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  case State#state.client_pid of
    undefined -> ok;
    Pid -> amqp_rpc_client:stop(Pid)
  end,
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_msg(Pid, Payload) ->
  Request = jsx:encode(Payload),
  Response = amqp_rpc_client:call(Pid, Request),
  jsx:decode(Response).
