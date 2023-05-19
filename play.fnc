ret_3() -> 
    (1, 2, 3, 4);;

ret_2() -> 
    x = ret_3() in 
    (x._1, x._2);;

ret_1() ->
    x = ret_2() in 
    x._1;;

factorial(n) ->
    if n == 1 then 1
    else n * factorial(n-1);;

fib(n) ->
    if n == 0 then 0
        else if n == 1 then 1
            else fib(n-1) + fib(n-2);;

