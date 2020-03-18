\name{Cosine.dist}
\alias{Cosine.dist}
\title{Cosine distance}
\description{Calculates cosine distance}

\usage{Cosine.dist(data.x, data.y=data.x)}

\arguments{
\item{data.x}{Matrix with variables that should be used in the computation of the distance.}
\item{data.y}{Matrix compatible with 'data.x' (optional).}
}

\details{
If arguments are data frames, Cosine.dist() internally converts them into matrices.

On normalized data, cosine distance is essentially the same as Euclidean.
}

\value{Distance object with distances among rows of 'data.x' and optionally those of 'data.y'}

% \references{
% }

\author{Alexey Shipunov}

\seealso{\code{\link{dist}}}

\examples{
plot(hclust(Cosine.dist(iris[!c(rep(1, 9), 0), -5])))
}

\keyword{multivariate}
