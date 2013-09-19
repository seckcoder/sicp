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
    return s.cdr();
};

function map(s, proc) {
    if (isEmpty(s)) return s;
    else {
        v = proc.apply(null, car(s));
        return cons(v, map(cdr(s), proc));
    }
};

function range(low, high) {
    if (low > high) {
        return empty;
    } else {
        return cons(low, range(low + 1, high));   
    }
};
