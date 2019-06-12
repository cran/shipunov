\name{MRH}
\alias{MRH}
\title{Matrix Representation of Hierarchical Clustering}

\description{Matrix Representation of Hierarchical clustering (MRH)}

\usage{
MRH(hcl, dim=NULL, method="groups")
}

\arguments{
  \item{hcl}{'hclust' object}

  \item{dim}{Number of desired dimensions, if defaults are not suitable}

  \item{method}{Either "groups" (default), or "height", or "cophenetic" (see below for explanations)}
}

\value{
 Matrix with number of columns equal to number of objects (n) minus 1 (method="height" or method="cophenetic")
 or n-2 (method="groups").
 Rows are objects, values are either cluster numbers (method="groups" or method="height")
 or MDS scores (method="cophenetic").
}

\details{
 This function calls cutree() or cmdscale(cophenetic()) in order to output the
 Matrix Representation of Hierarchical clustering (MRH).

 If method="groups" then clustering tree is cut by all possible numbers of clusters 'k'
 (excluding k=1 and k=n which bring no information).

 If method="height" then clustering tree is cut by equally spaced agglomeration heights
 (excluding minimal and maximal heights which bring no information). It is recommended to specify
 'dim' bigger then default here. Works only for ultrametric trees.

 If method="cophenetic" then multidimensional scaling scores with maximum dimensionality on cophenetic
 distances are computed. This is supposedly the most precise representation of clustering.

 Separate function Hcl2mat() represents the fourth method where in the resulting matrix, each column
 corresponds with one cluster, and values are 1 or 0 (cluster includes object or not).

 The main feature of the resulted matrices is that they provide the way of conversion between original data,
 distance matrices and clustering results. As they represent the _same_ objects, it is possible
 to "hyper-bind", or "average" (Ashkenazy et al., 2018) them.
}

\references{
Ashkenazy H., Sela I., Levy Karin E., Landan G., Pupko T. 2018. Multiple sequence alignment averaging
improves phylogeny reconstruction. Systematic Biology. 68: 117--130.
}

\seealso{
  \code{\link{cutree}}, \code{link{cmdscale}}, \code{link{Hcl2mat}}
}

\examples{
aa.h <- hclust(dist(t(atmospheres)))
plot(aa.h)
(aa.mrh1 <- MRH(aa.h))
aa.mrh2 <- MRH(aa.h, method="height", dim=100) # here 'dim' should better be large
str(aa.mrh2)
(aa.mrh3 <- MRH(aa.h, method="cophenetic"))
plot(hclust(dist(aa.mrh1)))
plot(hclust(dist(aa.mrh2)))
plot(hclust(dist(aa.mrh3)))
plot(hclust(dist(cbind(aa.mrh1, aa.mrh2)))) # hyper-bind
}

\keyword{Multivariate}
