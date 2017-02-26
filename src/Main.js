"use strict";

exports.concatDefault = function (xss) {
    var result = [];
    for (var i = 0, l = xss.length; i < l; i++) {
        var xs = xss[i];
        for (var j = 0, m = xs.length; j < m; j++) {
            result.push(xs[j]);
        }
    }
    return result;
};

exports.concatApply = function (xss) {
    return Array.prototype.concat.apply([], xss);
};

exports.concatCall = function (xss) {
    var result = [];
    for (var i = 0, l = xss.length; i < l; i++) {
        result = result.concat(xss[i]);
    }
    return result;
};

exports.concatChunks = function (xss) {
    if (xss.length <= 10000) {
        return Array.prototype.concat.apply([], xss);
    }

    var result = [];
    for (var i = 0, l = xss.length; i < l; i += 10000) {
        var chunk = xss.slice(i, i + 10000);
        result = result.concat.apply(result, chunk);
    }
    return result;
};

exports.replicate = function (count) {
    return function (value) {
        var result = [];
        var n = 0;
        for (var i = 0; i < count; i++) {
            result[n++] = value;
        }
        return result;
    };
};

exports.replicateNew = function (count) {
    return function (value) {
        if (count < 1) {
            return [];
        }
        var result = new Array(count);
        return result.fill(value);
    };
};
