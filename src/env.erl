-module(env).

-export([start/0]).

-export([get/1,
         get/2,
         get_integer/1,
         get_integer/2,
         get_string/1,
         get_string/2]).

start() ->
    application:ensure_all_started(?MODULE).

%%------------------------------------------------------------------------------
%% public
%%------------------------------------------------------------------------------

-type name()   :: atom().
-type value()  :: string() | integer().

-type result() :: value() | undefined | {error, term()}.

-spec get_string(name()) -> result().
get_string(Name) when is_atom(Name) ->
    get_string(Name, undefined).

-spec get_string(name(), any()) -> result().
get_string(Name, Default) when is_atom(Name) ->
    env_cast:to_string(get(Name, Default)).

-spec get_integer(name()) -> result().
get_integer(Name) when is_atom(Name) ->
    get_integer(Name, undefined).

-spec get_integer(name(), any()) -> result().
get_integer(Name, Default) when is_atom(Name) ->
    env_cast:to_integer(?MODULE:get(Name, Default)).

-spec get(name()) -> result().
get(Name) when is_atom(Name) ->
    get(Name, undefined).

-spec get(name(), any()) -> result().
get(Name, Default) when is_atom(Name) ->
    case cache_lookup(Name) of
        {ok, Value} ->
            Value;
        _ ->
            lookup(Name, Default)
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

-spec lookup(name(), any()) -> result().
lookup(Name, Default) ->
    case do_lookup(Name) of
        {ok, Value} ->
            cache_save(Name, Value);
        _ ->
            Default
    end.

-spec do_lookup(name()) -> result().
do_lookup(Name) ->
    case os:getenv(atom_to_list(Name)) of
        false ->
            application:get_env(application:get_application(), Name);
        Value ->
            {ok, Value}
    end.

-spec cache_save(name(), value()) -> boolean().
cache_save(Name, Value) ->
    true = ets:insert(?MODULE, {Name, Value}),
    Value.
