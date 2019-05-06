\name{Bclabels}
\alias{Bclabels}
\title{Plot bootstrap values}

\usage{
  Bclabels(hcl, values, coords=NULL, horiz=FALSE, method="text", threshold=NULL, ...)
}

\arguments{
  \item{hcl}{\code{hclust} object}

  \item{values}{\code{numeric}, (bootstrap) values to use}

  \item{coords}{If NULL (default), coordinates will be calculated with Hcoords(hcl)}

  \item{horiz}{Plot values for a horizontal tree?}

  \item{method}{If "text" (default), plot text values, if "points", plot points}

  \item{threshold}{If set, do not plot text or points for values < threshold}

  \item{...}{If "text" (default), additional arguments to text(), if "points", to points()}
}

\description{
  This low-level plot function plots text or points in accordance with bootstrap values to the
  corresponding node of the plotted hclust object.
}

\value{
  List with components: 'coords' for coordinates,
  'labels' for (selected) values.
}

\seealso{
  \code{\link{Bclust}}
}

\examples{

mo <- read.table("http://ashipunov.info/shipunov/open/moldino.txt", h=TRUE,
 sep="\t", row.names=1)
bb <- Bclust(t(mo)) # specify 'mc.cores=4' or similar to speed up the process
plot(bb$hclust)
Bclabels(bb$hclust, bb$values, col="red", pos=3, offset=0.1, threshold=0.5)

}

\keyword{Multivariate}
