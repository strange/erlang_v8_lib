var dns = {
    resolve: function(hostname, type) {
        return external.run('dns', ['resolve', hostname, type || 'A']);
    }
};
