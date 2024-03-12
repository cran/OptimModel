f2djac = function(Func, theta, ...)
{
  ## Numeric gradient of Func with respect to theta
  ## Func = a one-d function of theta (vector) and other arguments (the "...")
  ##        Func may return a single number or a vector.
    n = length( Func(theta, ...) )
    p = length(theta)
    df = matrix(NA, n, p)
    tol = 1e-8
    xx = theta

    for (j in 1:p)
    {
        temp = xx[j]
        h = tol*abs(temp)

        if (h == 0)
            h = tol
        xx[j] = temp + h
        h = xx[j] - temp

        fxPh = Func(xx, ...)
        xx[j] = temp - h
        fxMh = Func(xx, ...)
        xx[j] = temp

        df[,j] = .5*(fxPh - fxMh)/h

    }

    return (df)
}

