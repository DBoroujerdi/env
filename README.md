Env
===

Environment lookup and caching utility, primarily aimed at apps deployed with docker.


## What is it?

Often when building and using Docker containers we like to override internal config via environment variables. In practice this requires juggling the use of both the ``` application ``` and  ``` os ``` modules which have different API's.


## What is it like?

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
    string()|undefined = env:get_string(app_name, 'REMOTE_HOST'),
    integer()|undefined = env:get_integer(app_name, 'REMOTE_PORT'),
```


### Caching

Once a variable has been successfully obtained from either source it is cached in ETS, then for any subsequent calls it is obtained from ETS. So there isn't really any need to store the variables outside in your application. If an application or processs that requires a variable dies, it can simply read it again when it restarts.
