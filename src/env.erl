-module(env).

-behaviour(application).

-export([start/2,
         stop/1]).

-export([start/0]).

-export([get/2,
         get/3,
         get_integer/2,
         get_integer/3,
         get_string/2,
         get_string/3]).

%%------------------------------------------------------------------------------
%% callbacks
%%------------------------------------------------------------------------------

start(_Type, _Args) ->
    _ = init_cache(),

    env_sup:start_link().

stop(_State) ->
    ok.

%%------------------------------------------------------------------------------
%% public
%%------------------------------------------------------------------------------

start() ->
    application:ensure_all_started(?MODULE).

%%------------------------------------------------------------------------------

-type name()   :: atom().
-type value()  :: string() | integer().

-type result() :: value() | undefined | {error, term()}.

-spec get_string(atom(), name()) -> result().
get_string(App, Name) when is_atom(Name) ->
    get_string(App, Name, undefined).

-spec get_string(atom(), name(), any()) -> result().
get_string(App, Name, Default) when is_atom(Name) ->
    env_cast:to_string(get(App, Name, Default)).

-spec get_integer(atom(), name()) -> result().
get_integer(App, Name) when is_atom(Name) ->
    get_integer(App, Name, undefined).

-spec get_integer(atom(), name(), any()) -> result().
get_integer(App, Name, Default) when is_atom(Name) ->
    env_cast:to_integer(?MODULE:get(App, Name, Default)).

-spec get(atom(), name()) -> result().
get(App, Name) when is_atom(Name) ->
    get(App, Name, undefined).

-spec get(atom(), name(), any()) -> result().
get(App, Name, Default) when is_atom(Name) ->
    case cache_lookup(Name) of
        {ok, Value} ->
            Value;
        _ ->
            lookup(App, Name, Default)
    end.

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------

-spec cache_lookup(name()) -> {ok, value()} | not_found.
cache_lookup(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{_, Value}] ->
            {ok, Value};
        [] ->
            not_found
    end.

-spec lookup(atom(), name(), any()) -> result().
lookup(App, Name, Default) ->
    case do_lookup(App, Name) of
        {ok, Value} ->
            cache_save(Name, Value);
        _ ->
            Default
    end.

-spec do_lookup(atom(), name()) -> result().
do_lookup(App, Name) ->
    case os:getenv(atom_to_list(Name)) of
        false ->
            application:get_env(App, Name);
        Value ->
            {ok, Value}
    end.

-spec cache_save(name(), value()) -> boolean().
cache_save(Name, Value) ->
    true = ets:insert(?MODULE, {Name, Value}),
    Value.

init_cache() ->
    ets:new(?MODULE, [public, set, named_table]).
