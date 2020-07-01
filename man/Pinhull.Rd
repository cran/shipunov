\name{Pinhull}
\alias{Pinhull}
\title{Point in hull}
\description{For each observation, returns if it is within a polygon}

\usage{

Pinhull(pts, ppts, Hulls=TRUE)

}

\arguments{

\item{pts}{Data points, 2-dimensional}
\item{ppts}{List with polygon information (e.g., output from Hulls() or Ellipses())}
\item{Hulls}{Did the information come from Hulls() or Ellipses()?}

}

\details{

For each 'pts' observation, Pinhull() uses PBSmapping::findPolys() to
find if it is within (or on the border) of each polygon described in
'ppts'.

If 'Hulls=TRUE' (default), then list components with names 'centers' and
'outliers' will be attempted to remove from 'ppts' list.

The output or Pinhull is easy to use to calculate the "observation
overlap", it also allows to reveal "outliers" (points outside all
polygons) and all polygon membership features (e.g., which points belong
to more than one polygon).

}

\value{

Logical matrix, each column is the hull (polygon) name, rows correspond with rows of data points.

}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Hulls}}, \code{\link{Ellipses}}, \code{\link{Overlap}}}

\examples{

iris.p <- prcomp(iris[, -5])$x[, 1:2]
iris.h <- Hulls(iris.p, iris$Species, plot=FALSE)
iris.e <- Ellipses(iris.p, iris$Species, plot=FALSE)

## convex hulls
iris.pih <- Pinhull(iris.p, iris.h)
## this measure of membership overlap is analogous to Overlap()
dist(t(iris.pih), method="binary")

## confidence ellipses
iris.pie <- Pinhull(iris.p, iris.e)
## membership overlap
dist(t(iris.pie), method="binary")
## how to find outliers (points outside of all ellipses)
which(apply(iris.pie, 1, sum) == 0) # outliers
## how to make membership table
iris.pie.g <- cbind(iris.pie, group=Alldups(iris.pie, groups=TRUE))
key <- iris.pie.g[!duplicated(iris.pie), ]
key <- key[order(key[, "group"]), ]
mem <- aggregate(1:nrow(iris.p), list(group=iris.pie.g[, "group"]), paste0, collapse=", ")
mem <- cbind(key, mem)
mem[, mem \%-\% "group"] # all memberships

}

\keyword{multivariate}
