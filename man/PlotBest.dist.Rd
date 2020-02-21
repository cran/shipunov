\name{PlotBest.dist}
\alias{PlotBest.dist}
\title{Plots dotchart with best base distance method}

\description{Plots dotchart with best base distance method}
\usage{PlotBest.dist(data, distances=c("euclidean", "maximum", "manhattan",
 "canberra", "binary", "minkowski"), dim=2, plot=TRUE)}

\arguments{
\item{data}{Data frame with values}
\item{distances}{Distances to use}
\item{dim}{Number of dimensions}
\item{plot}{Plot?}
}

\details{
Plots the "best" base distance method.
Uses correlation between multidimensional scaling of distance object and PCA of data.
Two dimensions are default, change it with "dim" option.
}

\value{Numeric vector with correlation values (equal to the number of distances
involved)}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
PlotBest.dist(iris[, -5])
}

\keyword{hplot}
