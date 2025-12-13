-module(tt_handler).

%% API
-export([
  handle/1
]).

handle(Payload) ->
  try
    Request = jsx:decode(Payload, [return_maps]),
    Method = get_method(Request),
    ValidRequest = tt_validator:validate(Method, Request),
    Response = route(Method, ValidRequest),
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

get_method(Request) ->
  case maps:get(<<"method">>, Request, undefined) of
    undefined ->
      throw({<<"invalid_request">>, <<"Value 'method' is required in request">>});
    Method ->
      Method
  end.

route(<<"/card/touch">>, Request) ->
  tt_handler_card:touch(Request);
route(<<"/card/assign">>, Request) ->
  tt_handler_card:assign(Request);
route(<<"/card/delete">>, Request) ->
  tt_handler_card:delete(Request);
route(<<"/card/list_by_user">>, Request) ->
  tt_handler_card:list_by_user(Request);
route(<<"/card/delete_all_by_user">>, Request) ->
  tt_handler_card:delete_all_by_user(Request);
route(<<"/work_time/set">>, Request) ->
  tt_handler_work_time:set_work_time(Request);
route(<<"/work_time/get">>, Request) ->
  tt_handler_work_time:get_work_time(Request);
route(<<"/work_time/add_exclusion">>, Request) ->
  tt_handler_work_time:add_exclusion(Request);
route(<<"/work_time/get_exclusion">>, Request) ->
  tt_handler_work_time:get_exclusion(Request);
route(<<"/work_time/history_by_user">>, Request) ->
  tt_handler_work_time:history_by_user(Request);
route(<<"/work_time/statistics_by_user">>, Request) ->
  tt_handler_work_time:statistics_by_user(Request);
route(Method, _Request) ->
  throw({<<"invalid_request">>, <<"Method '", (Method)/binary, "' is undefined">>}).