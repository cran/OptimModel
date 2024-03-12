residuals.optim_fit = function(object, type=c("raw", "studentized"),...)
{

    type = match.arg(type)

    r = object$residuals
    lab = "Residuals"
    if ( type == "studentized" )
    {
        ## Get data
        data = getData(object)
#        x.var = object$call$x
#        if ( is.data.frame(eval(x.var)) )
#            x.var = colnames( eval(x.var) )
#        else if ( length(x.var) > 1 )
#            x.var = eval(x.var[[length(x.var)]])
#        else
#            x.var = all.vars(x.var)

        ##  Sometimes the names have a ".x" in front.
#        if ( all( !is.na(match(paste("x.", x.var, sep=""), names(data))) ) )
#            x.var = paste("x.", x.var, sep="")
#        x = data[,x.var]
        x = data[[1]]
        theta = coef(object)
        if ( object$fit.method == "mle" )
            theta = theta[1:(length(theta)-1)]  ## Remove log(sigma) from parameters
        f.model = eval( object$call$f.model )
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

