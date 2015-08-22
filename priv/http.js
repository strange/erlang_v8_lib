var http = {
    get: function(url) {
        return external.run('http', [url, 'get', '', '']);
    },
    post: function(url, data) {
        return external.run('http', [url, 'get', '', data]);
    }
};

