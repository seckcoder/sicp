// laze evaluation for javascript
var cons = exports.cons = function (a, b) {
    return {
        car:function () {
            return a;
        },
        cdr:function () {
            return b;
        }
    };
};

var empty = exports.empty = null;

var isEmpty = exports.isEmpty = function (s) {
    return s === empty;
};

var car = exports.car = function (s) {
    return s.car();
};

var cdr = exports.cdr = function (s) {
    return s.cdr().apply();
};

var ref = exports.ref = function (s, n) {
};

var range = exports.range = function (low, high) {
    if (low > high) {
        return empty;
    } else {
        return cons(low, function () {
            return range(low + 1, high);
        });
    }
};

function integersStartFrom(n) {
    return cons(n, function () {
        return integersStartFrom(n + 1);
    });
};

function fibgen (a, b) {
    return cons(a, function () {
        return fibgen(b, a + b);
    });
};

var fibs = fibgen(0, 1);
