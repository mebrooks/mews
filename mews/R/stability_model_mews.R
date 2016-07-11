##' @title stability model fitting
##'
##' @param X data sampling time and sampled value as columns. If only
##' one column is provided, assumes that the time is uniform
##' @param model the model used
##' @param p initial parameter guesses, if NULL, will guess from data
##' @param ... additional parameters passed to the optim routine
##' @param store_data if FALSE will not return an object with 
##' a copy of the original data
##' @return a mews model fit object (list), containing
##' the data (if requested), the model type used, the maximum-likelihood
##' estimated parameters, the log-likelihood at those values, 
##' and a convergence indicator
##' @details depends on the mews class of functions and the model 
##' definitions LSN and constOU.  
##' @export
##' @useDynLib OU
##' @useDynLib LSN
stability_model_mews <- function(X, model=c("LSN", "OU"), p = NULL, ..., 
                            store_data=TRUE){
  #model <- match.arg(model)
  # reformat time series objects into proper data frames
  if(is(X, "ts"))
    X <- data.frame(as.numeric(time(X)), X@.Data)
  # if time values are not provided
  else if(is.null(dim(X)))
    X <- data.frame(1:length(X), X)
  

  if(!is.null(p)){ ## got everything? then rock & roll
  	p[["log_sigma"]]=log(p[["sigma"]])
  	p[["log_theta"]]=log(p[["theta"]])
  	p[["log_R0"]]=log(p[["R0"]])

  	if(is.null(p[["m"]])) p[["m"]]=0
  	if(model=="OU"){
  		f1 <- MakeADFun(data=list(times=X[,1], obs=X[,2]), parameters=p, DLL="OU")
  	}else{
  		p[["log_a"]]=1
        f1 <- MakeADFun(data=list(times=X[,1], obs=X[,2]), parameters=p, DLL="LSN")
	}
		f1$env$tracemgc=FALSE

	o <- nlminb(f1 $par, f1 $fn, f1 $gr)
	#o <- optim(f1 $par, f1 $fn, f1 $gr, ...)
	
  } else if(is.null(p)){ ## oh, need p? try:
    p <- list(log_R0=log(1/max(time(X[,1]))), m=0, log_theta=log(mean(X[,2])), log_sigma=log(sd(X[,2])))
    f2 <- MakeADFun(data=list(times=X[,1], obs=X[,2]), parameters=p, DLL="OU")
	f2$env$tracemgc=FALSE
       o <- nlminb(f2 $par, f2 $fn, f2 $gr)
    #o <- optim(f2 $par, f2 $fn, f2 $gr, ...)

    # if model is "OU", we're done.  otherwise:
    if(model=="LSN"){
 	  # switch to the LSN model
      p_est <- o$par  # & use the OU estimated pars as starting guess
      p_est["R0"]=exp(p_est["log_R0"])
      # but rescale them to the new definitions:
      R0 <- as.numeric(p_est["R0"]^2)
      theta <- as.numeric(exp(p_est["log_theta"])+p_est["R0"])
      sigma <- as.numeric(abs(exp(p_est["log_sigma"])/sqrt(2*p_est["R0"]+ exp(p_est["log_theta"]))))     
      p <- list(log_R0=log(R0), log_a=1, log_theta=log(theta), log_sigma=log(sigma))
	  f3 <- MakeADFun(data=list(times=X[,1], obs=X[,2]), parameters=p, DLL="LSN")
	f3$env$tracemgc=FALSE


      ## now fit the LSN model
      o <- nlminb(f3 $par, f3 $fn, f3 $gr)
	 o$par["R0"]=exp(o$par["log_R0"])
	 o$par["m"] <- (exp(o$par["log_a"])-1)*o$par["R0"]/tail(X[,1], 1);
    }
  }
  names(X) <- c("time", "value")
  ## Collect the results and we're done
  if(!store_data) # remove the data object to save space?
    X <- NULL
  out <- list(X=X, pars=c("R0"=unname(exp(o$par["log_R0"])), "m"=unname(o$par["m"]),  "theta"=unname(exp(o$par["log_theta"])),"sigma"=unname(exp(o$par["log_sigma"]))), model=model, loglik = -o$objective,
              convergence=(o$convergence==0) )
  class(out) <- c("mews", "list")
  out
}
