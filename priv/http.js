'use strict';

var http = (function() {
    /**
     * Request data parameters are contained in an object with the following
     * layout.
     * @typedef {Object} RequestData
     * @property {object|string} payload {type: Object<*string>|String}
     * @property {object} headers {type: Object<*string>}
     * @property {object} options {type: Object<*string|number>}
     */
    var RequestData;

    /**
     * Supported HTTP request methods.
     *
     * @enum {string}
     */
    var methods = {
        DELETE: 'DELETE',
        GET: 'GET',
        HEAD: 'HEAD',
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
     * @param {?RequestData=} data The data to append to the request.
     */
    function request(method, url, data) {
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

    /**
     * Convenience function for making a get request.
     *
     * @param {string} url The remote url to make the request to.
     * @param {?RequestData=} data The data to append to the request.
     */
    function get(url, data) {
        if (data &&
            data.payload &&
            typeof data.payload !== 'string' &&
            Object.keys(data.payload).length !== 0) {
            url += formatQueryParams(data.payload);
            data.payload = {};
        }

        return http.request(methods.GET, url, data);
    };

    function __resolve_promise(status, ref, resp) {
        var body = resp.body;
        delete resp.body;

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
                reject('blobs are not supported yet.');
            });
            return p;
        };

        resp.arrayBuffer = function() {
            var p = new Promise(function(resolve, reject) {
                reject('ArayBuffers are not supported yet.');
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
        get: get,
        delete: request.bind(http, methods.DELETE),
        head: request.bind(http, methods.HEAD),
        post: request.bind(http, methods.POST),
        put: request.bind(http, methods.PUT),
        __resolve_promise: __resolve_promise
    };
})();
