-module(env_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

%%------------------------------------------------------------------------------
%% callbacks
%%------------------------------------------------------------------------------

all() ->
    test_functions(proplists:get_value(exports, ?MODULE:module_info())).

test_functions(Funs) ->
    lists:foldl(fun({Fun, _}, Acc) ->
                        [Fun|Acc]
                end, [], lists:filter(is_test_fun(), Funs)).

is_test_fun() ->
    fun({Fun, _}) ->
            lists:suffix("_test", atom_to_list(Fun))
    end.

init_per_suite(Config) ->
    {ok, _} = env:start(),
    Config.

%%------------------------------------------------------------------------------
%% tests
%%------------------------------------------------------------------------------

get_caching_from_os_test(_Config) ->
    ok = set_os_var("greeting", "Hello, World"),

    ?assertEqual("Hello, World", env:get(app_name, greeting)),

    %% change it
    ok = set_os_var("greeting", "Not 'Hello, World'"),

    %% is value still the same in the cache?
    ?assertEqual("Hello, World", env:get(app_name, greeting)).

get_integer_caching_from_os_test(_Config) ->
    ok = set_os_var("some_port", "8080"),

    ?assertEqual(8080, env:get_integer(app_name, some_port)),

    %% change it
    ok = set_os_var("some_port", "8081"),

    ?assertEqual(8080, env:get_integer(app_name, some_port)).

get_string_caching_from_os_test(_Config) ->
    ok = set_os_var("some_host", "localhost"),

    ?assertEqual("localhost", env:get_string(app_name, some_host)),

    %% change it
    ok = set_os_var("some_host", "not_localhost"),

    ?assertEqual("localhost", env:get_string(app_name, some_host)).

get_integer_caching_from_app_test(_Config) ->
    ok = set_app_var(app_name, another_host, 8080),

    ?assertEqual(8080, env:get_integer(app_name, another_host)),

    %% change it
    ok = set_app_var(app_name, another_host, 8081),

    ?assertEqual(8080, env:get_integer(app_name, another_host)).

get_env_source_preference_test(_Config) ->
    ok = set_os_var("integer_test", "4321"),
    ok = set_app_var(app_name, integer_test, 1234),

    %% should prefer os source
    ?assertEqual(4321, env:get_integer(app_name, integer_test)).

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------

set_os_var(Name, Value) ->
    case os:putenv(Name, Value) of
        true ->
            ok;
        _ ->
            {error, os_var_not_set}
    end.

set_app_var(App, Name, Value) ->
    application:set_env(App, Name, Value).
