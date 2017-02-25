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
    for (var i = 0, l = xss.length; i < l; i++) {
        var xs = xss[i];
        for (var j = 0, m = xs.length; j < m; j++) {
            result.push(xs[j]);
        }
    }
    return result;
};
