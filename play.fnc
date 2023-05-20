factorial(n) ->
    if n == 1 then 1
    else n * factorial(n-1)
;;

fib(n) ->
    if n == 0 then 0
        else if n == 1 then 1
            else fib(n-1) + fib(n-2)
;;

apply_n_times(f, n, x) ->
    if n == 0 then x
    else apply_n_times(f, n-1, f(x))
;;

-- expected: forall 'a. 'a -> 'a -> 'a -> 'a
-- wrong: forall 'b, 'a. 'a -> 'b -> 'a -> 'b
apply_twice(f, x) ->
    f(f(x));;

