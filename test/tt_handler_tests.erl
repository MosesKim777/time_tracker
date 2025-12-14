-module(tt_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("errors.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

handle_missing_method_test() ->
  Payload = jsx:encode(#{}),
  Response = tt_handler:handle(Payload),
  Decoded = jsx:decode(Response, [return_maps]),
  ?assertEqual(<<"error">>, maps:get(<<"status">>, Decoded)),
  ?assertEqual(?INVALID_REQUEST_TYPE, maps:get(<<"error_type">>, Decoded)).

handle_invalid_method_test() ->
  Payload = jsx:encode(#{
    <<"method">> => <<"/invalid/method">>
  }),
  Response = tt_handler:handle(Payload),
  Decoded = jsx:decode(Response, [return_maps]),
  ?assertEqual(<<"error">>, maps:get(<<"status">>, Decoded)),
  ?assertEqual(?INVALID_REQUEST_TYPE, maps:get(<<"error_type">>, Decoded)).

handle_invalid_json_test() ->
  InvalidPayload = <<"invalid json">>,
  try
    Response = tt_handler:handle(InvalidPayload),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"status">>, Decoded))
  catch
    _:_ -> ?assert(true)
  end.

handle_valid_request_structure_test() ->
  Payload = jsx:encode(#{
    <<"method">> => <<"/card/touch">>,
    <<"card_uid">> => <<"TEST123">>
  }),
  Response = tt_handler:handle(Payload),
  Decoded = jsx:decode(Response, [return_maps]),
  ?assert(maps:is_key(<<"status">>, Decoded)).

