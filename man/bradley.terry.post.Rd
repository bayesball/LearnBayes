\name{bradley.terry.post}
\alias{bradley.terry.post}
\title{Log posterior of a Bradley Terry random effects model}
\description{
 Computes the log posterior density of the talent parameters and the log 
standard deviation for a Bradley Terry model with normal
random effects
}
\usage{
bradley.terry.post(theta,data)
}

\arguments{
  \item{theta}{vector of talent parameters and log standard deviation}
  \item{data}{data matrix with columns team1, team2, wins by team1, and
wins by team2}
}

\value{value of the log posterior}

\author{Jim Albert}

\examples{
data(baseball.1964)
team.strengths=rep(0,10)
log.sigma=0
bradley.terry.post(c(team.strengths,log.sigma),baseball.1964)
}

\keyword{models}
