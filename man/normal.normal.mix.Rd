\name{normal.normal.mix}
\alias{normal.normal.mix}
\title{Computes the posterior for normal sampling and a mixture of normals prior}
\description{
 Computes the parameters and mixing probabilities for a normal sampling problem, variance known,
where the prior is a discrete mixture of normal densities.
} 
\usage{
normal.normal.mix(probs,normalpar,data)
}
\arguments{
  \item{probs}{vector of probabilities of the normal components of the prior}
  \item{normalpar}{matrix where each row contains the mean and variance parameters for a normal component of the prior}
  \item{data}{vector of observation and sampling variance}
}
\value{
\item{probs}{vector of  probabilities of the normal components of the posterior}
\item{normalpar}{matrix where each row contains the mean and variance parameters for a normal component of the posterior}
}
\author{Jim Albert}

\examples{
probs=c(.5, .5)
normal.par1=c(0,1)
normal.par2=c(2,.5)
normalpar=rbind(normal.par1,normal.par2)
y=1; sigma2=.5
data=c(y,sigma2)
normal.normal.mix(probs,normalpar,data)
}

\keyword{models}
