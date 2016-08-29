summary.posterior <- function(object, ...){
  if(length(object$mode) == 1){
    if(is.list(object$stuff)==FALSE) vname="Var" else {
    if(is.null(object$stuff$name)==TRUE) vname="Var" else
         vname=object$stuff$name}
    cat(paste(vname, ": Mean =", round(object$mode[1], 3), 
              "SD =", round(sqrt(object$var[1, 1]), 3), "\n"))}
  if(length(object$mode) > 1){
    if(is.list(object$stuff)==FALSE) vname <- 
                 paste("Var", 1: length(object$mode)) else {
    if(is.null(object$stuff$name)==TRUE) 
      vname <- paste("Var", 1: length(object$mode)) else
      vname <- object$stuff$name}
    for(j in 1:length(object$mode))
      cat(paste(vname[j], ": Mean =", round(object$mode[j], 3), 
                "SD =", round(sqrt(object$var[j, j]), 3), "\n"))
  }
}
