\name{Fill}
\alias{Fill}
\title{Fill data values downstream, like in spreadsheets}

\description{Replaces "missing" values with preceding values}

\usage{Fill(x, missing="")}

\arguments{
\item{x}{Vector, possibly with missing values}
\item{missing}{What to replace, typically empty string "" or NA}
}

\value{Vector with replaced values}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
aa <- c("a", "a", "", "b", "", "c", "d", "")
Fill(aa)
bb <- c("a", "a", NA, "b", NA, "c", "d", NA)
Fill(bb, missing=NA)
}

\keyword{System}
