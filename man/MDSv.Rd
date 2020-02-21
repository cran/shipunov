\name{MDSv}
\alias{MDSv}
\title{MDS: explained variance (surrogate)}

\description{MDS: explained variance (surrogate)}

\usage{MDSv(scores)}

\arguments{
\item{scores}{Data frame or matrix with values (e.g., result of 'isoMDS()'}
}

\details{MDS explained variance (surrogate, regression-based)}

\value{Numeric vector, one values per column of scores}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
iris.dist <- dist(iris[, 1:4], method="manhattan")
iris.dist[iris.dist == 0] <- abs(jitter(0))
library(MASS)
iris.m <- isoMDS(iris.dist)
cor(iris[, 1:4], iris.m$points) # MDS loadings surrogate
MDSv(iris.m$points) # MDS explained variance surrogate
}

\keyword{multivariate}

