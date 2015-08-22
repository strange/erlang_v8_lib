var __internal = {
    actions: [],
    data: [],
    promises: {},
    handleExternal: function(status, ref, args) {
        __internal.actions = [];
        if (status && ref) {
            var promise = __internal.promises[ref];

            if (status === 'success') {
                promise.resolve(args);
            } else if (status === 'error') {
                promise.reject(args);
            }
        }

        return __internal.actions;
    }
};

var external = {
    run: function(command, args) {
        var ref = String(Math.random());

        var p = new Promise(function(resolve, reject) {
            __internal.promises[ref] = {
                resolve: resolve,
                reject: reject
            };
            __internal.actions.push(['external', command, ref, args]);
        });

        return p;
    }
};

var console = {
    log: function(msg) {
        __internal.actions.push(['log', msg]);
    }
};
