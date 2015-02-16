simulate.posterior <- function(x, iter=1000, 
                               exact=FALSE, scale=3){
  require(coda)
  if (exact==FALSE){
    aa <- chol(x$var)
    p <- length(x$mode)
    S <- matrix(rnorm(iter * p), iter, p) %*% aa + 
      outer(rep(1, iter), x$mode)
    if(is.list(x$stuff)==TRUE){
      if(is.null(x$stuff$names)==FALSE){
        S <- data.frame(S)
        names(S) <- x$stuff$names}}
    accept.rate <- NA
  } else {
    proposal <- list(var=x$var, scale=scale)
    start <- x$mode
    R <- rwmetrop(x$logpost, proposal, start, iter, x$stuff)
    if(is.list(x$stuff)==TRUE){
      if(is.null(x$stuff$names)==FALSE){
        R$par <- data.frame(R$par)
        names(R$par) <- x$stuff$names}}
    S <- mcmc(R$par)
    accept.rate <- R$accept}
  list(sample=S, arate=accept.rate)
}
