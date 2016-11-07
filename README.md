# `erlang_v8_lib`

An opinionated JavaScript framework built on top of `erlang_v8`.
`erlang_v8_lib` is an Erlang application that includes a small framework to
simplify the task of adding functionality to the scripting environment.

The app adds three major components to accomplish this:

- A v8 worker pool
- A simple module system that makes it easy to connect JavaScript and Erlang.
- A few batteries included modules

## Getting started

Compile and start a shell:

    $ make
    $ make shell

Ensure that all required applications have been started, start the lib
supervisor (note that this is a separate and mandatory step. the reasoning
will be outlined below) and run some code:

    1>  application:ensure_all_started(erlang_v8_lib).
    {ok, [...]}
    2> erlang_v8_lib_sup:start_link().
    {ok,<0.69.0>}
    3> erlang_v8_lib:run(<<"process.return(1 + 1)">>).
    {ok,2}

## Registering handlers

A handler is an erlang module that exposes a `run/2` function that is
registered with an identifier: 

    -module(my_handler).

    -export([run/2]).

    run(_Args, _HandlerContext) ->
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

#### `http.request(method, url, [options])`

Make a HTTP request to `url`.

Example:

    {ok, _Body} = erlang_v8_lib:run(<<"
    http.request('get', 'http://httpbin.org/get')
        .then((resp) => resp.body())
        .then((body) => process.return(body));
    });
    ">>).

Valid `options` attributes:

- `payload`: Data to be sent (String)
- `headers`: Headers to send (Object)

The function returns a promise. The resolve function takes one argument:
`response`. The response object has the following attributes and methods:

- `response.json()`: Convert payload to a JavaScript structure (Promise).
- `response.text()`: Return payload as text (Promise).

#### `http.get(url, [options])`

Shorthand for `http.request('get', url, [options])`.

#### `http.post(url, [options])`

Shorthand for `http.request('post', url, [options])`.
