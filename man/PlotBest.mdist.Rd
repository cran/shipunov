\name{PlotBest.mdist}
\alias{PlotBest.mdist}
\title{Plots dotchart with best distance method, use multiple non-base distances}

\description{Plots dotchart with best distance method, use multiple non-base distances}

\usage{PlotBest.mdist(data, distances=c("manhattan", "euclidean", "canberra", "clark",
 "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "binomial",
 "chao", "cao", "mahalanobis", "cor.pearson", "cor.spearman", "cor.kendall",
 "gower_dist", "daisy.gower", "smirnov"), dim=2, binary.only=FALSE, plot=TRUE, ...)}

\arguments{
\item{data}{Data frame with values}
\item{distances}{Distances to use}
\item{dim}{Number of dimensions}
\item{binary.only}{Use binary only distances?}
\item{plot}{Plot?}
\item{...}{Arguments to 'vegdist()'}
}

\details{
Plots the "best" distance method, uses many non-base distances from diverse packages.
Uses correlation between multidimensional scaling of distance object and PCA of data.
Does not include "mountford" and "raup" from vegdist() as they are very special.
Two dimensions are default, change it with "dim" option.
}

\value{Numeric vector with correlation values (equal to the number of distances
involved)}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{PlotBest.dist}}}

\examples{
PlotBest.mdist(iris[, -5], scale.pca=TRUE)

m1 <- t((moldino > 0) * 1)
PlotBest.mdist(m1, binary.only=TRUE)
}

\keyword{Statistics}
