\name{Is.tax.inform.char}
\alias{Is.tax.inform.char}
\title{Taxonomic informativeness}
\description{Is the character potentially taxonomically informative?}

\usage{Is.tax.inform.char(vec)}

\arguments{
\item{vec}{Character vector from the column of DNA alignment}
}

\details{
Is the character potentially taxonomically informative?

If DNA encoding used, 'Nn?' should be converted into NA. Gaps ('-') counted, NAs not.
}

\value{Number of the potentially taxonomically informative characters.}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
Is.tax.inform.char(c("A", "C", "T", "T", "T", "T", "A"))
Is.tax.inform.char(c("A", "C", "T", "T", "T", "T", "G"))
Is.tax.inform.char(c("-", "T", "T", "T", "T", "T", "-", NA))
Is.tax.inform.char(c("A", "T", "T", "T", "T", "T", NA, NA))
Is.tax.inform.char(c(1, 0, 1, 1, 0, 0, 0, 0))
}

\keyword{utilities}

