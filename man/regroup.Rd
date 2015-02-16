\name{regroup}
\alias{regroup}
\title{Collapses a matrix by summing over rows}
\description{
Collapses a matrix by summing over a specific number of rows 
}
\usage{
regroup(data,g)
}
\arguments{
  \item{data}{a matrix}
  \item{g}{a positive integer beween 1 and the number of rows of data}
}

\value{
 reduced matrix found by summing over rows
}
\author{Jim Albert}

\examples{
data=matrix(c(1:20),nrow=4,ncol=5)
g=2
regroup(data,2)
}
\keyword{models}
