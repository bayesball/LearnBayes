plot.posterior <- function(x, ...){
  stuff <- list(...)
  if(is.null(stuff$exact)==TRUE) exact <- FALSE else 
    exact <- stuff$exact
  if(is.null(stuff$scale)==TRUE) scale <- 4  else
    scale <- stuff$scale
  y <- x

  p <- length(y$mode)
  if(p == 1){
    if(exact==FALSE){
      if(is.null(stuff$name)==TRUE) nm <- "" else 
          nm <- paste(" of", stuff$name)
      curve(dnorm(x, y$mode, sqrt(y$var)),
            y$mode - scale * sqrt(y$var), 
            y$mode + scale * sqrt(y$var),
            main=paste("Approx. Posterior Density", nm), 
            ylab="Density")
    } else {
      if(is.null(stuff$name)==TRUE) nm <- "" else 
        nm <- paste(" of", stuff$name)
      if(y$n_inputs==1)
         curve(exp(y$logpost(x, y[[7]])), 
            y$mode - scale * sqrt(y$var), 
            y$mode + scale * sqrt(y$var),
            main=paste("Exact Posterior Density", nm), 
            ylab="Density")
      if(y$n_inputs==2)
        curve(exp(y$logpost(x, y[[7]], y[[8]])), 
              y$mode - scale * sqrt(y$var), 
              y$mode + scale * sqrt(y$var),
              main=paste("Exact Posterior Density", nm), 
              ylab="Density")
    }
  }
  if(p == 2){
    lognorm <- function(y, stuff){
      dmnorm(y, stuff$mean, stuff$varcov, log=TRUE)
    }
    xlo <- y$mode[1] - scale * sqrt(y$var[1, 1])
    xhi <- y$mode[1] + scale * sqrt(y$var[1, 1])
    ylo <- y$mode[2] - scale * sqrt(y$var[2, 2])
    yhi <- y$mode[2] + scale * sqrt(y$var[2, 2])
    if(exact==FALSE){
      if(is.null(stuff$name)==TRUE) nm <- c("", "") else 
          nm <- stuff$name
      mycontour(lognorm, c(xlo, xhi, ylo, yhi), 
                list(mean=y$mode, varcov=y$var))
      title(main="Contours of Approx. Posterior Density",
                xlab=nm[1], ylab=nm[2])
    } else {
      if(is.null(stuff$name)==TRUE) nm <- c("", "") else 
        nm <- stuff$name
      if(y$n_inputs==1)  
       mycontour(y$logpost, c(xlo, xhi, ylo, yhi), y[[7]])
       title(main="Contours of Exact Posterior Density", 
                xlab=nm[1], ylab=nm[2])
      if(y$n_inputs==2)
        mycontour(y$logpost, c(xlo, xhi, ylo, yhi), y[[7]], y[[8]]) 
        title(main="Contours of Exact Posterior Density", 
                        xlab=nm[1], ylab=nm[2])
    }
  }
  if(length(y$mode) > 2)
    cat(paste("No plot available for more than 2 parameters."))
}
