\name{Bclust}
\alias{Bclust}
\title{Bootstrapped hclust}

\description{Bootstraps (or jacknife) hierarchical clustering}

\usage{
Bclust(data, method.d="manhattan", method.c="ward.D",
FUN=function(.x) hclust(dist(.x, method=method.d), method=method.c),
iter=1000, mc.cores=1, monitor=TRUE, bootstrap=TRUE)
}

\arguments{
  \item{data}{Data suitable for the chosen distance method}

  \item{method.d}{Method for dist()}

  \item{method.c}{Method for hclust()}

  \item{FUN}{Function to make 'hclust' objects}

  \item{iter}{Number of replicates}

  \item{mc.cores}{\code{integer}, number of processes to
  run in parallel}

  \item{monitor}{If TRUE (default), prints a dot for each replicate}

  \item{bootstrap}{If FALSE (not default), performs jacknife (and makes 'iter=ncol(data)')}
}

\value{
  List with components: 'values' for bootstrapped frequencies of each node,
  'hcl' for original 'hclust' object,
  'consensus' which is a sum of all Hcl2mat() matrices.
}

\details{
  This function provides bootstrapping for hierarchical
  clustering (\code{\link{hclust}} objects). Internally, it uses
  Hcl2mat() which converts 'hclust' objects into binary matrix of cluster
  memberships.

  Bclust() and companion functions were based on functions from the
  'bootstrap' package of Sebastian Gibb.
}

\references{
  Felsenstein, Joseph. \emph{Confidence limits on
  phylogenies: an approach using the Bclust.} Evolution
  (1985): 783-791.

  Efron, Bradley, Elizabeth Halloran, and Susan Holmes.
  \emph{Bootstrap confidence levels for phylogenetic
  trees.} Proceedings of the National Academy of Sciences
  93.23 (1996): 13429-13429.
}

\seealso{
  \code{\link{Jclust}}, \code{link{BootA}, \link{Hcl2mat}}
}

\examples{
(bb <- Bclust(t(atmospheres))) # specify 'mc.cores=4' or similar to speed up the process

plot(bb$hclust)
Bclabels(bb$hclust, bb$values, col="blue", pos=3, offset=0.1)

plot(bb$hclust)
Bclabels(bb$hclust, bb$values, col="blue", pos=3, offset=0.1, threshold=0.9)

plot(bb$hclust)
Bclabels(bb$hclust, bb$values, method="points", threshold=0.9, pch=19, cex=2)

plot(bb$hclust)
Bclabels(bb$hclust, bb$values, method="points", pch=19, cex=bb$values*3)

coords1 <- Hcoords(bb$hclust)
plot(bb$hclust)
Bclabels(bb$hclust, bb$values, coords=coords1, method="points", pch=19,
 cex=bb$values*3)

oldpar <- par(mar=c(2,1,0,4))
Ploth(bb$hclust, horiz=TRUE)
Bclabels(bb$hclust, bb$values, col="blue", pos=3, offset=0.1, horiz=TRUE)
par(oldpar)

plot(hclust(dist(bb$consensus)), main="Net consensus tree") # net consensus
## majority rule is 'consensus >= 0.5', strict is like 'round(consensus) == 1'

bb1 <- Bclust(t(atmospheres), FUN=function(.x) hclust(Gower.dist(.x)), monitor=FALSE)
plot(bb1$hclust)
Bclabels(bb1$hclust, bb1$values, col="green", pos=3, offset=0.1)

Bclust(t(atmospheres), bootstrap=FALSE) # jacknife
}

\keyword{Multivariate}
