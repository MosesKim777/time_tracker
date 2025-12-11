-module(tt_rpc_server).

-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("time_tracker.hrl").

%% API
-export([
  start_link/0
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

-record(state, {channel}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, Channel} = tt_rabbitmq_worker:get_channel(),
  case create_and_subscribe_queue(Channel) of
    ok -> {ok, #state{channel = Channel}};
    Error -> Error
  end.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload, props = Props}}, State) ->
  Response = tt_handler:handle(Payload),
  ok = reply(Tag, Response, Props, State#state.channel),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  case State#state.channel of
    undefined -> ok;
    Channel ->
      cleanup_queue(Channel),
      ok
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

create_and_subscribe_queue(Channel) ->
  case create_queue(Channel) of
    ok ->
      subscribe_to_queue(Channel);
    Error ->
      Error
  end.

create_queue(Channel) ->
  case amqp_channel:call(Channel, #'queue.declare'{queue = ?QUEUE}) of
    #'queue.declare_ok'{} -> ok;
    Error ->
    {stop, {error, queue_declare_failed, Error}}
  end.

subscribe_to_queue(Channel) ->
  case amqp_channel:subscribe(Channel, #'basic.consume'{queue = ?QUEUE}, self()) of
    #'basic.consume_ok'{} -> ok;
    Error ->
      {stop, {error, queue_subscribe_failed, Error}}
  end.


reply(Tag, Response, Props, Channel) ->
  ReplyTo = Props#'P_basic'.reply_to,
  CorrelationId = Props#'P_basic'.correlation_id,
  ResponseProps = #'P_basic'{
    correlation_id = CorrelationId
  },
  amqp_channel:cast(Channel,
    #'basic.publish'{
      exchange = <<"">>,
      routing_key = ReplyTo
    },
    #amqp_msg{payload = Response, props = ResponseProps}),
  amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
  ok.

cleanup_queue(Channel) ->
  Delete = #'queue.delete'{
    queue = ?QUEUE,
    if_unused = false,
    if_empty = false
  },
  amqp_channel:call(Channel, Delete).

