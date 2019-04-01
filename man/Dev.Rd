\name{Dev}
\alias{Dev}
\title{Which object is predicted with less accuracy?}
\description{Allows to know which object is predicted with less accuracy}

\usage{Dev(pred, useNA = "no", adj = FALSE)}

\arguments{
\item{pred}{Pedictions as character matrix, each boot is a column}
\item{useNA}{Use NAs?}
\item{adj}{Adjust deviation coefficients? (see below)}
}

\details{
Allows to know which object is predicted with less accuracy.

It calculates deviation coeffitient out of the table where rows are objects, and columns are (bootstrapped) predictions.

By default, does not use NA's (use useNA="always" on your risk).

Deviation coefficient is a minimal absolute deviation from one of the range ends (0 and number of predictions),
divided by number of predictions, multiplied by number of deviations minus one, and (optionally) adjusted by division
to number of predition levels (to make different situations comparable).
}

\value{Numeric vector of deviation coefficients}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{BootRF}}, \code{\link{BootKNN}}}

\examples{
\donttest{
## could be slow
iris.bootrf <- BootRF(iris[, -5], iris[, 5])
data.frame(Iris=make.names(iris[, 5], unique=TRUE), Dev=Dev(iris.bootrf))
}
}

\keyword{Statistics}
