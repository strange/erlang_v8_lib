'use strict';

var http = {};

var methods = {
    DELETE: 'DELETE',
    GET: 'GET',
    HEAD: 'HEAD',
    POST: 'POST',
    PUT: 'PUT'
};

(function() {
    function formatQueryParams(params) {
        var qs = '?';
        for (var k in params) {
            if (params.hasOwnProperty(k)) {
                qs += k.toString() + '=' + params[k].toString() + '&';
            }
        }
        return qs.substring(0, qs.length-1);
    }

    function parseArgs(headers, body) {
        if (!headers && !body) {
            return [[], {}];
        } else if (headers && !body) {
            if (Array.isArray(headers)) {
                return [headers, {}];
            } else {
                return [[], headers];
            }
        } else {
            return [headers, body];
        }
    }

    http.request = function(method, url, headers, body) {
        method = String(method).toUpperCase();
        url = String(url);
        var args = parseArgs(headers, body);
        headers = args[0];
        body = args[1];

        return external.run('http', [
            url,
            method,
            JSON.stringify(headers),
            body.toString()
        ]);
    };

    http.get = function(url, headers, body) {
        var args = parseArgs(headers, body);
        headers = args[0];
        body = args[1];

        if (typeof body !== 'string' && Object.keys(body).length !== 0) {
            url += formatQueryParams(body);
            body = {};
        }
        return http.request(methods.GET, url, headers, body);
    };

    http.delete = http.request.bind(http, methods.DELETE);

    http.head = http.request.bind(http, methods.HEAD);

    http.post = http.request.bind(http, methods.POST);

    http.put = http.request.bind(http, methods.PUT);
})();
