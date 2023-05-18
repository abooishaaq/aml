ret_3 () -> 
    (1, 2, 3, 4);;

ret_2 () -> 
    x = ret_3() in 
    (x._1, x._2);;

ret_1 () ->
    x = ret_2() in 
    x._1;;

mul (X, Y) ->
    X == Y;;
