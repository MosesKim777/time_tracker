-module(tt_handler).

%% API
-export([
  handle/1
]).

%%%===================================================================
%%% API
%%%===================================================================

handle(Payload) ->
  try
    Request = jsx:decode(Payload, [return_maps]),
    Method = get_method(Request),
    ValidRequest = tt_validator:validate(Method, Request),
    Response = handle(Method, ValidRequest),
    jsx:encode(Response)
  catch
    throw:{ErrType, ErrMsg} ->
      jsx:encode(#{
        <<"status">> => <<"error">>,
        <<"error_type">> => ErrType,
        <<"message">> => ErrMsg
      });
    Class:Reason:Stacktrace ->
      io:format("Class: ~p~n Reason: ~p~n Stacktrace: ~p~n", [Class, Reason, Stacktrace]),
      jsx:encode(#{
        <<"status">> => <<"error">>,
        <<"error_type">> => <<"internal_error">>,
        <<"message">> => list_to_binary(
          io_lib:format("~p:~p:~p", [Class, Reason, Stacktrace])
        )
      })
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_method(Request) ->
  case maps:get(<<"method">>, Request, undefined) of
    undefined ->
      throw({<<"invalid_request">>, <<"Value 'method' is required in request">>});
    Method ->
      Method
  end.

handle(<<"/card/touch">>, Request) ->
  tt_card_handler_card:touch(Request);
handle(<<"/card/assign">>, Request) ->
  tt_card_handler_card:assign(Request);
handle(<<"/card/delete">>, Request) ->
  tt_card_handler_card:delete(Request);
handle(<<"/card/list_by_user">>, Request) ->
  tt_card_handler_card:list_by_user(Request);
handle(<<"/card/delete_all_by_user">>, Request) ->
  tt_card_handler_card:delete_all_by_user(Request);
handle(<<"/work_time/set">>, Request) ->
  tt_work_time_handler:set_work_time(Request);
handle(<<"/work_time/get">>, Request) ->
  tt_work_time_handler:get_work_time(Request);
handle(<<"/work_time/add_exclusion">>, Request) ->
  tt_work_time_handler:add_exclusion(Request);
handle(<<"/work_time/get_exclusion">>, Request) ->
  tt_work_time_handler:get_exclusion(Request);
handle(<<"/work_time/history_by_user">>, Request) ->
  tt_work_time_handler:history_by_user(Request);
handle(<<"/work_time/statistics_by_user">>, Request) ->
  tt_work_time_handler:statistics_by_user(Request);
handle(Method, _Request) ->
  throw({<<"invalid_request">>, <<"Method '", (Method)/binary, "' is undefined">>}).