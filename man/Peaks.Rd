\name{Peaks}
\alias{Peaks}
\title{Find local maxima}

\description{Find local maxima}

\usage{Peaks(series, span=3, do.pad=TRUE)}

\arguments{
\item{series}{Numerical vector}
\item{span}{Window size}
\item{do.pad}{Padding}
}

\details{
Finding peaks in a simple dataset.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
## count peaks on joint histogram, this suggests number of clusters
histdata <- hist(apply(scale(iris[, -5]), 1, function(.x) sum(abs(.x))), breaks=10, plot=FALSE)
sum(Peaks(histdata$counts)) # 3 is the first value after 1 and does not change when 8 < breaks < 22
}

\keyword{System}
