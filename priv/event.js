var Event = {
    getType: function() {
        return __internal.context.type;
    },
    getContext: function() {
        return __internal.context;
    },
    allow: function(msg) {
        if (Event.getType() !== 'open') {
            throw "'allow' is not available in this scope.";
        }
        process.return(['allowed', msg || '']);
    },
    deny: function(msg) {
        if (Event.getType() !== 'open') {
            throw "'deny' not available in this scope.";
        }
        process.return(['denied', msg || '']);
    }
};
