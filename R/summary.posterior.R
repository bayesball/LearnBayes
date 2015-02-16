summary.posterior <- function(x){
  if(length(x$mode) == 1){
    if(is.list(x$stuff)==FALSE) vname="Var" else {
    if(is.null(x$stuff$name)==TRUE) vname="Var" else
         vname=x$stuff$name}
    cat(paste(vname, ": Mean =", round(x$mode[1], 3), 
              "SD =", round(sqrt(x$var[1, 1]), 3), "\n"))}
  if(length(x$mode) > 1){
    if(is.list(x$stuff)==FALSE) vname <- 
                 paste("Var", 1: length(x$mode)) else {
    if(is.null(x$stuff$name)==TRUE) 
      vname <- paste("Var", 1: length(x$mode)) else
      vname <- x$stuff$name}
    for(j in 1:length(x$mode))
      cat(paste(vname[j], ": Mean =", round(x$mode[j], 3), 
                "SD =", round(sqrt(x$var[j, j]), 3), "\n"))
  }
}
