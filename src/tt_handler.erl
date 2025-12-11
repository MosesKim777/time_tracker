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

route(<<"/card/touch">>, Body) ->
  tt_handler_card:touch(Body);
route(<<"/card/assign">>, Body) ->
  tt_handler_card:assign(Body);
route(<<"/card/delete">>, Body) ->
  tt_handler_card:delete(Body);
route(<<"/card/list_by_user">>, Body) ->
  tt_handler_card:list_by_user(Body);
route(<<"/card/delete_all_by_user">>, Body) ->
  tt_handler_card:delete_all_by_user(Body);
%%route(<<"/work_time/set">>, Body) ->
%%  tt_handler:set_work_time(Body);
%%route(<<"/work_time/get">>, Body) ->
%%  tt_handler:get_work_time(Body);
%%route(<<"/work_time/add_exclusion">>, Body) ->
%%  tt_handler:add_exclusion(Body);
%%route(<<"/work_time/get_exclusion">>, Body) ->
%%  tt_handler:get_exclusion(Body);
%%route(<<"/work_time/history_by_user">>, Body) ->
%%  tt_handler:history_by_user(Body);
%%route(<<"/work_time/statistics_by_user">>, Body) ->
%%  tt_handler:statistics_by_user(Body);
route(Method, _Body) ->
  throw({<<"invalid_request">>, <<"Method '", (Method)/binary, "' is undefined">>}).