\name{normal.select}
\alias{normal.select}
\title{Selection of Normal Prior Given Knowledge of Two Quantiles}
\description{
 Finds the mean and standard deviation of a normal density that matches knowledge of
two quantiles of the distribution.
}
\usage{
normal.select(quantile1, quantile2)
}
\arguments{
  \item{quantile1}{list with components p, the value of the first probability,
  and x, the value of the first quantile}
  \item{quantile2}{list with components p, the value of the second probability,
  and x, the value of the second quantile}
}

\value{
\item{mean}{mean of the matching normal distribution}
\item{sigma}{standard deviation of the matching normal distribution}
}

\author{Jim Albert}

\examples{
# person believes the 15th percentile of the prior is 100
# and the 70th percentile of the prior is 150
quantile1=list(p=.15,x=100)
quantile2=list(p=.7,x=150)
normal.select(quantile1,quantile2)
}

\keyword{models}
