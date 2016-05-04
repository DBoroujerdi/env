-module(env_cast).

-export([to_integer/1,
         to_string/1]).

%%------------------------------------------------------------------------------
%% public
%%------------------------------------------------------------------------------

to_integer(undefined) ->
    undefined;
to_integer(Value) when is_integer(Value) ->
    Value;
to_integer(Value) when is_list(Value) ->
    list_to_integer(Value).

to_string(undefined) ->
    undefined;
to_string(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_string(Value) when is_list(Value) ->
    Value;
to_string(Value) when is_integer(Value) ->
    integer_to_list(Value);
to_string(Value) when is_binary(Value) ->
    binary_to_list(Value).

%%------------------------------------------------------------------------------
%% tests
%%------------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_integer_of_undefined_test() ->
    ?assertEqual(undefined, to_integer(undefined)).

to_integer_test() ->
    ?assertEqual(1234, to_integer("1234")).

to_string_from_integer_test() ->
    ?assertEqual("1234", to_string(1234)).

to_string_from_binary_test() ->
    ?assertEqual("1234", to_string(<<"1234">>)).

to_string_from_undefined_test() ->
    ?assertEqual(undefined, to_string(undefined)).

to_string_from_atom_test() ->
    ?assertEqual("atom", to_string(atom)).

-endif.
