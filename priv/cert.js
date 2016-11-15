var cert = {
    validity: function(hostname, port) {
        return external.run('cert', ['validity', hostname, port || 443]);
    }
};
