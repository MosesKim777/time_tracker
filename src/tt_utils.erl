-module(tt_utils).

-export([
  val_to_binary/1
]).

val_to_binary(Value) ->
  list_to_binary(io_lib:format("~p", [Value])).
