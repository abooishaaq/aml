ret_3() -> 
    (1, 2, 3, 4,)
;;

ret_2() -> 
    x = ret_3() in 
    (x._1, x._2,)
;;

ret_1() ->
    x = ret_2() in 
    x._1
;;

cmp_rets() ->
    y = ret_2() in 
    z = ret_3() in 
    z == y
;;

factorial(n) ->
    if n == 1 then 1
    else n * factorial(n-1)
;;

fib(n) ->
    if n == 0 then 0
        else if n == 1 then 1
            else fib(n-1) + fib(n-2)
;;

odd(n) ->
    if n == 0 then false
        else even(n-1)
;;

even(n) ->
    if n == 0 then true
        else odd(n-1)
;;


-- (f: t1 -> t1, x: t1) -> t1 
apply(f, x) ->
    f(x)
;;

-- (f: t1 -> t1, x: Int) -> t1
apply_twice(f, x) ->
    f(f(x))
;;


-- (f: t1 -> t1, t1, n: Int) -> t1
apply_n_times(f, x, n) ->
    if n == 0 then x
        else apply_n_times(f, f(x), n-1)
;;

-- (k: Int) -> Int
apply_fib_k_times(k) ->
    apply_n_times(fib, 10, k)
;;

-- () -> Int
apply_fib_twice() ->
    apply_twice(fib, 10)
;;
