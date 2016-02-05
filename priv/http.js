'use strict';

var http = {};

(function() {
    var methods = {
        DELETE: 'DELETE',
        GET: 'GET',
        HEAD: 'HEAD',
        POST: 'POST',
        PUT: 'PUT'
    };

    function formatQueryParams(params) {
        var prefix = '?';
        var qs = '';
        for (var k in params) {
            if (params.hasOwnProperty(k)) {
                qs += prefix + k.toString() + '=' + params[k].toString();
                prefix = '&';
            }
        }
        return qs;
    }

    http.request = function(method, url, data) {
        data = data || {};
        data.payload = data.payload || {};
        data.headers = data.headers || {};
        data.options = data.options || {};

        return external.run('http', [
            String(url),
            String(method).toUpperCase(),
            JSON.stringify(data.headers),
            String(data.payload)
        ]);
    };

    http.get = function(url, data) {
        if (data &&
            data.payload &&
            typeof data.payload !== 'string' &&
            Object.keys(data.payload).length !== 0) {
            url += formatQueryParams(data.payload);
            data.payload = {};
        }

        return http.request(methods.GET, url, data);
    };

    http.delete = http.request.bind(http, methods.DELETE);

    http.head = http.request.bind(http, methods.HEAD);

    http.post = http.request.bind(http, methods.POST);

    http.put = http.request.bind(http, methods.PUT);
})();
