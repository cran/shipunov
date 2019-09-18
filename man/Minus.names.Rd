\name{\%-\%}
\alias{\%-\%}
\title{Minus names}
\description{Subtract names from names}

\usage{x \%-\% y}

\arguments{
\item{x}{Character vector (likely named) to subtract from}
\item{y}{Subtracting character vector}
}

\details{
In fact, 'names(x)' are used instead of 'x'. If 'x' has no names, they will be assigned from values.
}

\value{Character vector}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
str(iris[, iris \%-\% "Species"])
c("apples", "bananas") \%-\% c("apples")
}

\keyword{System}
