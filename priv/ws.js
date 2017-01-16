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

    function __resolve_conn_promise(status, ref, socket) {
        var conn = {
            receive: function() {
                return external.run('ws', ['receive', socket]);
            },
            send: function(data) {
                return external.run('ws', ['send', socket, String(data)]);
            },
            close: function() {
                return external.run('ws', ['close', socket]);
            }
        };

        return __internal.handleExternal(status, ref, conn);
    };

    return {
        open: open,
        __resolve_conn_promise: __resolve_conn_promise
    };
})();
