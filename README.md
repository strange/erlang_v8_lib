# `erlang_v8_lib`

An opinionated JavaScript framework built on top of `erlang_v8`.
`erlang_v8_lib` is an Erlang application that includes a small framework that
makes it easier to add functionality to a scripting environment.

The app adds three major components to accomplish this:

- A v8 worker pool
- A simple module system that makes it easy to connect JavaScript and Erlang.
- A few batteries included modules

## Registering handlers

A handler is an erlang module that exposes a `run/1` function that is
registered with an identifier: 

    -module(my_handler).

    -export([run/1]).

    run(_Args) ->
        {ok, <<"hello">>}.

Registering the module as `{<<"myhandler">>, my_handler}` (more docs inc) will
allow you to invoke the handler from JavaScript:

    external.run('myhandler').then(function(value) {
        process.return(value);
    });

The above will match `'myhandler'` with the handler registered as
`<<"myhandler">>`, call `run/1` without arguments, fulfill the promise with
the value `'hello'` and "return" (see below) the value to the calling process.

## JavaScript API

The application comes with a few pre-built modules.

### `process`

The `process` module does cool stuff.

#### `process.return(value)`

Return a value to the calling Erlang process. Execution in the current scope
proceeds in the VM, but no external events occur after the call to return.

    {ok, 1} = erlang_v8_lib:run(<<"process.return(1);">>).

### `http`

Simple HTTP api.

#### `http.get(url)`

    {ok, _Body} = erlang_v8_lib:run(<<"
    http.get('http://httpbin.org/get').then(function(data) {
        process.return(data.body);
    });
    ">>).

#### `http.post(url, data)`

    {ok, _Body} = erlang_v8_lib:run(<<"
    http.post('http://httpbin.org/post', 'hello').then(function(data) {
        process.return(data.body);
    });
    ">>).
