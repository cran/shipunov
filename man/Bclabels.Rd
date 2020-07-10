\name{Bclabels}
\alias{Bclabels}
\title{Plot bootstrap values}

\description{Print (bootstrap) values on 'hclust' plot}

\usage{

Bclabels(hcl, values, coords=NULL, horiz=FALSE, method="text",
 threshold=NULL, top=NULL, percent=FALSE, ...)

}

\arguments{

  \item{hcl}{\code{hclust} object}

  \item{values}{\code{numeric}, (bootstrap) values to use}

  \item{coords}{If NULL (default), coordinates will be calculated with Hcoords(hcl)}

  \item{horiz}{Plot values for a horizontal tree?}

  \item{method}{If "text" (default), plot text values, if "points", plot points}

  \item{threshold}{If set, do not plot text or points for values < threshold; respects percents if set}

  \item{top}{If set as 'n', plot values only for 'n' highest clusters}

  \item{percent}{Plot values as percents?}

  \item{...}{If "text" (default), additional arguments to text(), if "points", to points()}

}

\details{

  This low-level plot function plots text or points in accordance with
  bootstrap values to the corresponding node of the plotted 'hclust'
  object.

}

\value{

  List with components: 'coords' for coordinates, 'labels' for (selected)
  values.

}

\seealso{
  \code{\link{Bclust}}
}

\examples{

bb <- Bclust(t(moldino)) # specify 'mc.cores=4' or similar to speed up the process
plot(bb$hclust)
Bclabels(bb$hclust, bb$values, col="red", pos=3, offset=0.1, threshold=0.5)

iris.bb <- Bclust(iris[, rep(1:4, 6)], iter=100)
## uses hyper-binding to make number of variables reliable
## remove iter=100 for better bootstrap
plot(iris.bb$hclust, labels=FALSE, main="", xlab="", sub="Bootstrap, 100 replicates")
Bclabels(iris.bb$hclust, iris.bb$values, top=5, percent=TRUE, pos=3, offset=0.1)
Fence(iris.bb$hclust, iris$Species)
legend("topright", legend=levels(iris$Species), col=1:3, lwd=2.5, bty="n")

}

\keyword{cluster}
