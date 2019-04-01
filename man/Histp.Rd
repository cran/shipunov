\name{Histp}

\alias{Histp}

\title{Histogram with percents}

\description{Histogram with percents on the top of bars}

\usage{Histp(x, breaks, ...)}

\arguments{
\item{x}{numeric vector}
\item{breaks}{number of breaks}
\item{...}{other arguments to 'hist()'}
}

\details{Histp plots histogram with percents on the top of bars, it is useful to know exactly the proportion of observations in each bin}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{hist}}}

\examples{
Histp(iris[, 1])
}

\keyword{Plots}
