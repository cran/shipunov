\name{Coeff.det}
\alias{Coeff.det}
\title{Average coefficients of determination for each variable}
\description{Average coefficients of determination for each variable}

\usage{Coeff.det(X, ...)}

\arguments{
\item{X}{Data frame or matrix with values}
\item{...}{Arguments to 'cor()'}
}

\details{Average coefficients of determination for each variable.

Allow to compare various correlation structures (Rostova, 1999).
}

\value{Numerical vector of coefficients of determination}

\references{Rostova N.S. 1999. The variability of correlations systems between the morphological characters.
Part 1. Natural populations of Leucanthemum vulgare (Asteraceae). Botanicheskij Zhurnal. 84(11): 50--66.}

\author{Alexey Shipunov}

% \seealso{}

\examples{
Coeff.det(trees, use="pairwise")
}

\keyword{Correlation}

