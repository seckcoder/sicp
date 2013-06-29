#|
In applicative order: O(log(n))

In normal order: 
suppose for operation k: we will do:
gcd(vk ,uk), and remainder_op(vk) = ak, remainder_op(uk) = bk
then:
gcd(a,b,k) = (vk ,bk)
f(k) = (ak, bk)
=> 
gcd(a,b,k+1) = (bk, remainder(vk ,bk))
f(k+1) = (bk, ak + bk + 1)
and we have:
f(0) = (0, 0)

=> bk = bk-1 + bk-2 + 1
=> ak = bk-1
apparently bk > fib(k).

|#
