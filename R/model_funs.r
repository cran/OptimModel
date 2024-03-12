.f.ssq = function(theta, x, y, w=NULL)
{
  f.model = NA
  if ( is.null(w) )
    return( sum( (y-f.model(theta, x))^2 ) )
  else
    return( sum( w*(y-f.model(theta, x))^2 ) )
}

## This is the gradient of .f.ssq wrt theta
## gr.model is the gradient of f.model wrt theta
.gr.ssq = function(theta, x, y, w=NULL)
{
  f.model = NA
  gr.model = NA
  g = gr.model(theta, x)
  r = y-f.model(theta, x)
  if ( is.null(w) )
    return(-2*colSums(g*r))
  else
    return(-2*colSums(w*g*r))
}

## Used in the IRWLS algorithm to estimate variance parameters
.trkfunc = function(phi)
{
  mu = NA
  w.func = NA
  resid = NA
  n = length(mu)
  g = w.func(phi, mu)
  gdot = exp( mean( log(g) ) )
  s = sum( abs(resid)/g )
  trk = gdot*s

  return(trk)
}

## Try to minimize the log-likelihood
.loglik = function(theta, phi=0, lsigma=0)
{
  x = NA
  y = NA
  f.model = NA
  w.func = NA
  pred = f.model(theta, x)
  sigma = exp(lsigma)*w.func(phi, pred)
  out = -sum( dnorm(y, mean=pred, sd=sigma, log=TRUE) )

  return(out)
}

.f.mle = function(all.param)
{
  p.th = NA
  phi.start = NA
  phi.fixed =NA
  
  l = list(theta=all.param[1:p.th], lsigma=all.param[length(all.param)])
  if ( phi.fixed )
    l[["phi"]] = phi.start
  else if ( length(all.param) > p.th+1 )
    l[["phi"]] = all.param[(p.th+1):(length(all.param)-1)]

  return(do.call(".loglik", l))

}
## Convert functions to text
process_low_level_txt <- function(txt,x,fun){
	indx <- grep(x,txt)
	tval <- list()
	tval[[1]] <- txt[1:(indx - 1)]
	
	nmval <- names(attributes(fun))
	txt_val <- list()
	indV <- 1
	txt_val[[indV]] <- gsub('NA','',x)
	indV <- indV + 1
	
	if(length(nmval)> 0L){
		tmp_fun <- fun
		for(i in 1:length(nmval)){
			attr(tmp_fun,nmval[i]) <- NULL
		}
		a <- capture.output(tmp_fun)
		a <- remove_extra_attributes(a)
		txt_val[[indV]] <- a
		indV <- indV + 1
		for(i in 1:length(nmval)){
			txt_val[[indV]] <- paste0('attr(',gsub(' = NA','',x),',"',nmval[i],'") <- ')
			indV <- indV + 1
			
			a <- capture.output(attr(fun,nmval[i]))
			a <- remove_extra_attributes(a)
			txt_val[[indV]] <- a
			indV <- indV + 1
		}
	}else{
		a <- capture.output(fun)
		a <- remove_extra_attributes(a)
		txt_val[[indV]] <- a
		indV <- indV + 1
	}
	l1 <-do.call('c',txt_val)
	tval[[2]] <- l1
	tval[[3]] <- txt[(indx + 1):length(txt)]
	do.call('c',tval)
}
remove_extra_attributes <- function(x){
	x <- x[grep('<environment: ',x,fixed=TRUE,invert=TRUE)]
	x <- x[grep('<bytecode: ',x,fixed=TRUE,invert=TRUE)]
	x <- gsub('^\\[[0-9]*\\]','',x)
	x
}
