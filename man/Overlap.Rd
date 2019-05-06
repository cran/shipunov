\name{Overlap}
\alias{Overlap}
\alias{summary.Overlap}

\title{Polygons' overlap}

\description{Measures polygons' overlap}

\usage{
Overlap(ppts, Hulls=TRUE)
\method{summary}{Overlap}(object, ...)
}

\arguments{
\item{ppts}{List with hulls information (e.g., output from Hulls())}
\item{Hulls}{Did the information came from Hulls()?}
\item{object}{Object of the class 'Overlap'}
\item{...}{Additional arguments}
}

\details{
'Overlap()' calculates polygons (hulls) overlap (it requires 'PBSmapping' package).

If 'Hulls=FALSE', then list components with names 'centers' and 'outliers'
will not be attempted to delete from 'ppts' list.
}

\value{
Object of class 'Overlap', ot nothing.
}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Hulls}}}

\examples{
iris.pca <- prcomp(iris[, 1:4], scale=TRUE)
iris.p <- iris.pca$x[, 1:2]
iris.h <- Hulls(iris.p[, 1:2], iris[, 5], plot=FALSE)
iris.o <- Overlap(iris.h)
summary(iris.o)
}

\keyword{Multivariate}
