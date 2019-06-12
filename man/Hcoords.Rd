\name{Hcoords}
\alias{Hcoords}
\title{Plot bootstrap values}

\usage{
  Hcoords(hcl)
}

\arguments{
  \item{hcl}{\code{hclust} object}
}

\description{
  This function calculate coordinates for each 'hclust'
  node. Inspired by pvclust::hc2axes()
}

\seealso{
  \code{\link{Bclust}}
}

\examples{

head(Hcoords(hclust(dist(iris[, -5]))))

}

\keyword{Multivariate}
