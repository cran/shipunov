\name{Co.test}
\alias{Co.test}
\title{Correlation test between cophenetic and original distances}
\description{Correlation test between cophenetic and original distances}

\usage{Co.test(hclust, dist, method="spearman")}

\arguments{
\item{hclust}{Hclust object}
\item{dist}{Distance matrix}
\item{method}{How to calculate correlation}
}

\details{Correlation between cophenetic distances and original distances.

Reveals the consistency of used methods.

Spearman correlation is default because cophenetic distances are frequently non-parametric.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
iris.d <- dist(iris[, -5])
iris.h <- hclust(iris.d)
Co.test(iris.h, iris.d, method="kendall")
}

\keyword{Statistics}
