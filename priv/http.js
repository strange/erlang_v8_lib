var http = {
    request: function(url, options) {
        var method = options.method || 'GET';
        var data = options.data || '';
        return external.run('http', [url, method, '', data]);
    },
    get: function(url, queryParameters) {
        return http.request(url, {
            method: 'GET',
            data: queryParameters
        });
    },
    post: function(url, data) {
        return http.request(url, {
            method: 'POST',
            data: data
        });
    }
};
