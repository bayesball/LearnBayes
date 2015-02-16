plot.posterior <- function(y, exact=FALSE, scale=4, ...){
#  print(match.call())
  p <- length(y$mode)
  if(p == 1){
    if(exact==FALSE){
      if(is.list(y$stuff)==FALSE) nm <- "" else {
      if(is.null(y$stuff$name)==TRUE) nm <- "" else 
          nm <- paste(" of", y$stuff$name)}
      curve(dnorm(x, y$mode, sqrt(y$var)),
            y$mode - scale * sqrt(y$var), 
            y$mode + scale * sqrt(y$var),
            main=paste("Approx. Posterior Density", nm), 
            ylab="Density", ...)
    } else {
      if(is.list(y$stuff)==FALSE) nm <- "" else {
      if(is.null(y$stuff$name)==TRUE) nm <- "" else 
        nm <- paste(" of", y$stuff$name)}
      curve(exp(y$logpost(x, y$stuff)), 
            y$mode - scale * sqrt(y$var), 
            y$mode + scale * sqrt(y$var),
            main=paste("Exact Posterior Density", nm), 
            ylab="Density", ...)
    }
  }
  if(p == 2){
    lognorm <- function(y, stuff){
      dmnorm(y, stuff$mean, stuff$varcov, log=TRUE)
    }
    stuff <- list(mean=y$mode, varcov=y$var)
    xlo <- y$mode[1] - scale * sqrt(y$var[1, 1])
    xhi <- y$mode[1] + scale * sqrt(y$var[1, 1])
    ylo <- y$mode[2] - scale * sqrt(y$var[2, 2])
    yhi <- y$mode[2] + scale * sqrt(y$var[2, 2])
    if(exact==FALSE){
      if(is.list(y$stuff)==FALSE) nm <- c("", "") else {
      if(is.null(y$stuff$name)==TRUE) nm <- c("", "") else 
          nm <- y$stuff$name}
      mycontour(lognorm, c(xlo, xhi, ylo, yhi), stuff, 
                main="Contours of Approx. Posterior Density",
                xlab=nm[1], ylab=nm[2], ...)
    } else {
      if(is.list(y$stuff)==FALSE) nm <- c("", "") else {
      if(is.null(y$stuff$name)==TRUE) nm <- c("", "") else 
        nm <- y$stuff$name}
      mycontour(y$logpost, c(xlo, xhi, ylo, yhi), y$stuff, 
                main="Contours of Exact Posterior Density", 
                xlab=nm[1], ylab=nm[2], ...)
    }
  }
  if(length(y$mode) > 2)
    cat(paste("No plot available for more than 2 parameters."))
}
