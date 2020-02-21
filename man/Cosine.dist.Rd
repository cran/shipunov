\name{Cosine.dist}
\alias{Cosine.dist}
\title{Cosine distance}
\description{Calculates cosine distance}

\usage{Cosine.dist(data.x, data.y=data.x)}

\arguments{
\item{data.x}{A matrix containing variables that should be used in the computation of the distance.}
\item{data.y}{A matrix compatible with 'data.x' (optional).}
}

\details{
Cosine.dist() does not work with data frames, convert them with e.g., as.matrix().
}

\value{Distance object with distances among rows of 'data.x' and optionally those of 'data.y'}

% \references{
% }

\author{Alexey Shipunov}

\seealso{\code{\link{dist}}}

\examples{
plot(hclust(Cosine.dist(as.matrix(iris[!c(rep(1, 9), 0), -5]))))
}

\keyword{multivariate}
