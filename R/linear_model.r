# Program:  linear.model.R
# Location:  s:/novick/R libraries/Hill Model/funs
# Version:  1
# Author:   Steven Novick
# Date:     November 30, 2023
# Purpose:  Linear Model, gradient algorithms
#       Can be used with "Optim Model" (Novick) library

    
linear_model = function(theta, x){ as.vector(x%*%theta) }

attr(linear_model, "gradient") = function(theta, x)
{

    return(x)
}

attr(linear_model, "start") = function(x, y)
{
    x.qr = qr(x)
    beta0 = qr.coef(x.qr, y)

    return(beta0)

}
