mycontour <- function (logf, limits, ...) {
LOGF=function(theta, ...){
          if(is.matrix(theta)==TRUE){
          val=matrix(0,c(dim(theta)[1],1))
          for (j in 1:dim(theta)[1])
              val[j]=logf(theta[j,], ...)
           } 
          else val=logf(theta, ...)
          return(val)
        }
    ng = 50
    x0 = seq(limits[1], limits[2], len = ng)
    y0 = seq(limits[3], limits[4], len = ng)
    X = outer(x0, rep(1, ng))
    Y = outer(rep(1, ng), y0)
    n2 = ng^2
    Z = LOGF(cbind(X[1:n2], Y[1:n2]), ...)
    Z = Z - max(Z)
    Z = matrix(Z, c(ng, ng))
    contour(x0, y0, Z, levels = seq(-6.9, 0, by = 2.3), lwd = 2)
}
