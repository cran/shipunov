\name{Adj.Rand}
\alias{Adj.Rand}
\title{Adjusted Rand index}
\description{Adjusted Rand index}

\usage{Adj.Rand(cl1, cl2)}

\arguments{
\item{cl1}{First classification (character vector of group names)}
\item{cl2}{Second classification}
}

\details{Adjusted Rand Index.}

\value{Similarity: numerical vector of length 1}

\references{Hubert L. and Arabie P. 1985. Comparing partitions. Journal of Classification. 2. 193--218.}

\author{Alexey Shipunov}

\seealso{\code{\link{Misclass}}}

\examples{
iris.dist <- dist(iris[, 1:4], method="manhattan")
iris.hclust <- hclust(iris.dist)
iris.3 <- cutree(iris.hclust, 3)
Adj.Rand(iris.3, iris[, 5])
}

\keyword{Statistics}

