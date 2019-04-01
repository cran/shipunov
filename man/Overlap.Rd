\name{Overlap}
\alias{Overlap}
\alias{Overlap2}
\alias{summary.Overlap}

\title{Polygons' overlap}

\description{Polygons' overlap}

\usage{
Overlap(ppts)
Overlap2(ppts)
\method{summary}{Overlap}(object, ...)
}

\arguments{
\item{ppts}{Hulls information (e.g., output from 'Hulls()')}
\item{object}{Object of the class 'Overlap'}
\item{...}{Additional arguments}
}

\details{
Overlap() calculates polygons (hulls) overlap (it requires 'PBSmapping' package).

'Overlap2()' is deprecated (it requires 'gpclib' package).
}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Hulls}}}

\examples{
iris.pca <- prcomp(iris[, 1:4], scale=TRUE)
iris.p <- iris.pca$x[, 1:2]
iris.h <- Hulls(iris.p[, 1:2], as.numeric(iris[, 5]), plot=FALSE)
iris.o <- Overlap(iris.h)
summary(iris.o)
}

\keyword{Plots}

