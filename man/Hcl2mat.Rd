\name{Hcl2mat}
\alias{Hcl2mat}
\title{Plot bootstrap values}

\usage{
  Hcl2mat(hcl)
}

\arguments{
  \item{hcl}{\code{hclust} object}
}

\description{
  This function converts 'hclust' object into binaty matrix
  in accordance with clusterings.
}

\seealso{
  \code{\link{Bclust}}
}

\examples{

head(Hcl2mat(hclust(dist(iris[, -5]))))

}

\keyword{Multivariate}
