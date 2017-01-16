// http://stackoverflow.com/a/18391400
if (!('toJSON' in Error.prototype))
Object.defineProperty(Error.prototype, 'toJSON', {
    value: function () {
        var alt = {};

        Object.getOwnPropertyNames(this).forEach(function (key) {
            alt[key] = this[key];
        }, this);

        return alt;
    },
    configurable: true,
    writable: true
});

var __internal = {
    actions: [],
    data: [],
    promises: {},
    context: {},
    setContext: function(context) {
        __internal.context = context || {};
    },
    handleExternal: function(status, ref, value) {
        __internal.actions = [];
        if (status && ref) {
            var promise = __internal.promises[ref];

            if (status === 'success') {
                promise.resolve(value);
            } else if (status === 'error') {
                promise.reject(value);
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

var process = {
    return: function(value) {
        __internal.actions.push(['return', value]);
    }
};
