'use strict';

var ws = (function() {
    function open(url, options) {
        options = options || {};
        var headers = options.headers || {};

        return external.run('ws', [
            'connect',
            String(url),
            Object(headers)
        ]);
    };

    function __resolve_conn_promise(status, ref, conn) {
        var socket = conn.ref;
        delete conn.ref;

        conn.receive = function() {
            return external.run('ws', ['receive', socket]);
        };

        conn.send = function(data) {
            return external.run('ws', ['send', socket, data]);
        };

        return __internal.handleExternal(status, ref, conn);
    };

    return {
        open: open,
        __resolve_conn_promise: __resolve_conn_promise
    };
})();
