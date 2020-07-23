\name{SM.dist}
\alias{SM.dist}
\title{Simple Match distance}
\description{Calculates simple match distance}

\usage{SM.dist(data)}

\arguments{

\item{data}{Matrix (or data frame) with variables that should be used in
the computation of the distance between rows.}

}

\details{

If argument is the data frame, SM.dist() internally converts it into the
matrix. If there are character values, they will be converted column-wise
to factors and then to integers.

}

\value{Distance object with distances among rows of 'data'}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{dist}}}

\examples{

ii <- cluster::pam(SM.dist(sapply(iris[, -5], round)), k=3)
Misclass(ii$clustering, iris$Species, best=TRUE)

i2 <- cluster::pam(SM.dist(iris), k=3) # SM.dist() "consumes" all types of data
Misclass(i2$clustering, iris$Species, best=TRUE)

}

\keyword{multivariate}
