simulate.posterior <- function(object, nsim=1000, seed=NULL, ...){
  stuff <- list(...)
  if(is.null(stuff$exact)==TRUE) exact <- FALSE else 
       exact <- stuff$exact
  if(is.null(stuff$scale)==TRUE) scale <- 3  else
       scale <- stuff$scale
  
  if (exact==FALSE){
    aa <- chol(object$var)
    p <- length(object$mode)
    S <- matrix(rnorm(nsim * p), nsim, p) %*% aa + 
      outer(rep(1, nsim), object$mode)
      if(is.null(stuff$name)==FALSE){
        S <- data.frame(S)
        names(S) <- stuff$name}
    accept.rate <- NA
  } else {
    proposal <- list(var=object$var, scale=scale)
    start <- object$mode
    if(object$n_inputs==1)
     R <- rwmetrop(object$logpost, proposal, start, nsim, object[[7]])
    if(object$n_inputs==2)
      R <- rwmetrop(object$logpost, proposal, start, nsim, 
                    object[[7]], object[[8]])
      if(is.null(stuff$name)==FALSE){
        R$par <- data.frame(R$par)
        names(R$par) <- stuff$name}
    S <- coda::mcmc(R$par)
    accept.rate <- R$accept}
  list(sample=S, arate=accept.rate)
}
