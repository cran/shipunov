\name{Squares}
\alias{Squares}
\title{Areas of polygons}
\description{Calculates areas of polygons}

\usage{Squares(ppts, relative=FALSE, Hulls=TRUE)}

\arguments{
\item{ppts}{Hulls information (e.g., output from Hulls() or Ellipses())}
\item{relative}{Calculate relative squares?}
\item{Hulls}{Did the information come from Hulls() or Ellipses()?}
}

\details{

Calculates areas of polygons (requires 'PBSmapping' package). Might be
useful to understand better the group variability.

If 'Hulls=TRUE' (default), then list components with names 'centers' and
'outliers' will be attempted to remove from 'ppts' list.

}

\value{

Numerical vector, one element per polygon.

}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Hulls}}, \code{\link{Ellipses}}}

\examples{

iris.pca <- prcomp(iris[, 1:4], scale=TRUE)
iris.p <- iris.pca$x[, 1:2]

iris.h <- Hulls(iris.p[, 1:2], as.numeric(iris[, 5]), plot=FALSE)
Squares(iris.h, relative=TRUE)

iris.e <- Ellipses(iris.p, iris$Species, plot=FALSE, centers=TRUE)
Squares(iris.e)

}

\keyword{multivariate}
