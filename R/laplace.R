laplace <- function (logpost, mode, stuff) {
  options(warn=-1)
  fit=optim(mode, logpost, gr = NULL, stuff, hessian=TRUE,
            control=list(fnscale=-1))
  options(warn=0)
  mode=fit$par
  h=-solve(fit$hessian)
  p=length(mode)
  int = p/2 * log(2 * pi) + 0.5 * log(det(h)) +
    logpost(mode, stuff)
  output = list(mode = mode, var = h, int = int, 
                converge=fit$convergence==0,
                logpost = logpost, stuff=stuff)
  class(output) <- "posterior"
  return(output)
}