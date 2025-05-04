getData = function(object){
    mCall = object$call
    x = eval(mCall$x,envir=parent.frame())
    y = eval(mCall$y,envir=parent.frame())
    data = data.frame(x=x, y=y)

    return(as.list(data))
}

