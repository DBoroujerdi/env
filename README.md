ENV
===

Environment Variable Util

Often, when building and using Docker containers, we like to override internal config via environment variables. In practice, this requires the use of 'application' and 'os' which have different API's. Gproc can be used to hide this but what if you don't want to depend on the whole of gproc just to use one corner of it?

```erlang
    %% os
    string()|boolean() = os:getenv("REMOTE_NAME"),
    string()|boolean() = os:getenv("REMOTE_PORT"),

    %% application
    {ok, any()}|undefined = application:get_env(app_name, 'REMOTE_HOST'),
    {ok, any()}|undefined = application:get_env(app_name, 'REMOTE_PORT'),
```

This can be replace with the following.

```erlang
    string()|undefined = env:get_string('REMOTE_HOST'),
    integer()|undefined = env:get_integer('REMOTE_PORT'),
```

An assumption has been made that you will always prefer an os environment variable first, then from the application context. If you want to control where variables are sourced from, gproc should be used. This API has prioritised simplicity over functionality.

### caching

Once a variable has been successfully obtained from either source, it is cached in ETS, then for any subsequent calls it is obtained from ETS. So there isn't really any need to store the variables outside in your application. If an application that requires a variables dies, it can simply read it again when it restarts.
