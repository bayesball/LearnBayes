\name{careertraj.setup}
\alias{careertraj.setup}
\title{Setup for Career Trajectory Application}
\description{
 Setups the data matrices for the use of WinBUGS
 in the career trajectory application. 
}
\usage{
careertraj.setup(data)
}
\arguments{
 \item{data}{data matrix for ballplayers with variables Player, Year, Age, G, AB, R, H, X2B, X3B, HR, RBI, BB, SO}
}

\value{
\item{player.names}{vector of player names}
\item{y}{matrix of home runs for players where a row corresponds to the home runs for a player during
all the years of his career}
\item{n}{matrix of AB-SO for all players}
\item{x}{matrix of ages for all players for all years of their careers}
\item{T}{vector of number of seasons for all players}
\item{N}{number of players}
}

\author{Jim Albert}

\examples{
data(sluggerdata)
careertraj.setup(sluggerdata)
}

\keyword{models}
