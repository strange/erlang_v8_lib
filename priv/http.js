'use strict';

var http = (function() {
    /**
     * Supported HTTP request methods.
     *
     * @enum {string}
     */
    var methods = {
        DELETE: 'DELETE',
        GET: 'GET',
        HEAD: 'HEAD',
        PATCH: 'PATCH',
        POST: 'POST',
        PUT: 'PUT'
    };

    /**
     * Takes an object and converts its key value pairs into a query string.
     *
     * @param {obect} params
     * @return {string}
     */
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

    /**
     * Makes a request.
     *
     * @param {string} method The HTTP "verb" to use.
     * @param {string} url The remote url to make the request to.
     * @param {object} config Configuration
     */
    function request(method, url, config) {
        config = config || {};

        var body = config.body || '';
        var headers = config.headers || {};
        var queryParams = config.queryParams || {};
        var followRedirect = config.followRedirect || false;

        url += formatQueryParams(queryParams);

        return external.run('http', [
            String(url),
            String(method).toUpperCase(),
            Object(headers),
            String(body),
            Boolean(followRedirect)
        ]);
    };

    function __resolve_promise(status, ref, resp) {
        var body = resp.body;
        delete resp.body;

        try {
            resp.headers = JSON.parse(resp.headers);
        } catch(e) {
            resp.headers = {};
        }

        resp.text = function() {
            var p = new Promise(function(resolve, reject) {
                resolve(body);
            });
            return p;
        };

        resp.json = function() {
            var p = new Promise(function(resolve, reject) {
                try {
                    var json = JSON.parse(body);
                    resolve(json);
                } catch(e) {
                    reject(e);
                }
            });
            return p;
        };

        resp.blob = function() {
            var p = new Promise(function(resolve, reject) {
                reject('Blobs are not supported yet.');
            });
            return p;
        };

        resp.arrayBuffer = function() {
            var p = new Promise(function(resolve, reject) {
                reject('ArrayBuffers are not supported yet.');
            });
            return p;
        };

        return __internal.handleExternal(status, ref, resp);
    };

    /**
     * Expose methods on the http object.
     */
    return {
        request: request,
        get: request.bind({}, methods.GET),
        delete: request.bind({}, methods.DELETE),
        head: request.bind({}, methods.HEAD),
        patch: request.bind({}, methods.PATCH),
        post: request.bind({}, methods.POST),
        put: request.bind({}, methods.PUT),
        __resolve_promise: __resolve_promise
    };
})();
