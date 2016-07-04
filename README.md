ENV
===

Environment lookup and caching utility, primarily aimed at apps deployed with docker.

## What is it?

Often, when building and using Docker containers, we like to override internal config via environment variables. In practice, this requires the use of ``` application ``` and  ``` os ``` which have different API's. Gproc's ``` gproc:get_env ``` can be used to hide this but you might not want to depend on the whole of gproc, which does many things, to use just one corner of it, although it is very good.


### Benefits over gproc:get_env?

With gproc you'll still have to convert types. Ports, for instance, are returned as strings, requiring their conversion, whereas when looking up from *.app.src or *.config you will get back whatever the type is in there. Type conversions inside env therefore creates safety, you get back the type you asked for.

Consitent API regardless of the source of the env, as shown below.


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

Once a variable has been successfully obtained from either source, it is cached in ETS, then for any subsequent calls it is obtained from ETS. So there isn't really any need to store the variables outside in your application. If an application or processs that requires a variables dies, it can simply read it again when it restarts.
