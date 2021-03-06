function Y (g) {
    return (function (f) {
        return f(f);
    })(function (f) {
        return g(function () {
            return f(f).apply(null, arguments);
        });
    });
};


var fib = Y(function (fib_t) {
    return function (n) {
        if (n == 0) return 0;
        if (n == 1) return 1;
        return fib(n-1) + fib(n-2);
    };
});

console.log(fib(10));
