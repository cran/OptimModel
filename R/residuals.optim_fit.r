residuals.optim_fit = function(object, type=c("raw", "studentized"), ...)
{

    type = match.arg(type)

    r = object$residuals
    lab = "Residuals"
    if ( type == "studentized" )
    {
        ## Get data
        x = object$x

        theta = coef(object)
        if ( object$fit.method == "mle" )
            theta = theta[1:(length(theta)-1)]  ## Remove log(sigma) from parameters
        f.model = eval( object$call$f.model, envir=parent.frame() )
        jacobian = attr(f.model, "gradient")
        ## Approximate the hat matrix
        X.mat = if ( is.null(jacobian) )
                f2djac(f.model, coef(object), x=x)
            else
                jacobian(theta, x)
        xTx.inv = solve(t(X.mat)%*%X.mat)
        Hat.mat = sqrt( 1 - apply( X.mat, 1, function(z){ t(z)%*%xTx.inv%*%z}) )
        r = r/(object$sigma*Hat.mat)
        r[ is.infinite(r) ] = NA

        lab = paste("Studentized.", lab, sep="")
    }
    attr(r, "label") = lab
    return(r)
}

