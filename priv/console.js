var console = {
    log: function(msg) {
        external.run('console', ['log', msg]);
    },
    warn: function(msg) {
        external.run('console', ['warn', msg]);
    }
};
