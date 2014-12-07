-module(gpio).

-export([open_pin/1, close_pin/1]).
-export([write_pin/2, set_pin_high/1, set_pin_low/1]).
-export([read_pin/1]).
-export([set_direction/2, set_output/1, set_input/1]).

-define(GPIO_BASENAME, "/sys/class/gpio").
-define(PIN_NAME(Pin), io_lib:format("gpio~b", [Pin])).
-define(DEBUG, false).


debug(FileName, Pin) ->
    case ?DEBUG of
	true  -> io:format("Writing ~p to file ~p~n", [Pin, FileName]);
	false -> ok
    end.

open_pin(Pin) ->
    FileName = filename:join(?GPIO_BASENAME, "export"),
    debug(FileName, Pin),
    case file:write_file(FileName, integer_to_list(Pin)) of
	ok              -> ok;
	{error, Reason} ->
	    io:format("Warning, open() failed. Reason: ~p~n",
		      [Reason])
    end.

close_pin(Pin) ->
    FileName = filename:join(?GPIO_BASENAME, "unexport"),
    debug(FileName, Pin),
    ok = file:write_file(FileName, integer_to_list(Pin)).

write_pin(Pin, Value) when is_integer(Value),
			   Value >= 0,
			   Value =< 1 ->
    FileName = filename:join([?GPIO_BASENAME, ?PIN_NAME(Pin), "value"]),
    debug(FileName, Pin),
    ok = file:write_file(FileName, integer_to_list(Value)).

read_pin(Pin) ->
    FileName = filename:join([?GPIO_BASENAME, ?PIN_NAME(Pin), "value"]),
    debug(FileName, Pin),
    {ok, Binary} = file:read_file(FileName),
    {Val, _} = string:to_integer(lists:flatten(binary_to_list(Binary))),
    Val.
    
set_pin_high(Pin) ->
    write_pin(Pin, 1).

set_pin_low(Pin) ->
    write_pin(Pin, 0).

set_direction(Pin, Direction) when Direction =:= in;
				   Direction =:= out ->
    FileName = filename:join([?GPIO_BASENAME, ?PIN_NAME(Pin), "direction"]),
    debug(FileName, Pin),
    ok = file:write_file(FileName, atom_to_list(Direction)).
    
set_output(Pin) ->
    set_direction(Pin, out).

set_input(Pin) ->
    set_direction(Pin, in).
