\name{Squares}
\alias{Squares}
\title{Polygons' squares}
\description{Polygons' squares}

\usage{Squares(ppts, relative=FALSE)}

\arguments{
\item{ppts}{Hulls information (e.g., output from 'Hulls()')}
\item{relative}{Calculate relative squares?}
}

\details{Calculates polygons' squares (requires 'PBSmapping' package).}

\value{Numerical vector of squares}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Hulls}}}

\examples{
iris.pca <- prcomp(iris[, 1:4], scale=TRUE)
iris.p <- iris.pca$x[, 1:2]
iris.h <- Hulls(iris.p[, 1:2], as.numeric(iris[, 5]), plot=FALSE)
iris.o <- Squares(iris.h, relative=TRUE)
}

\keyword{multivariate}
