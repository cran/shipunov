\name{Read.tri.nts}
\alias{Read.tri.nts}
\title{Read 'NTSYSpc' files}

\description{Read a lower triangular matrix}

\usage{Read.tri.nts(file, ...)}

\arguments{
\item{file}{File to read}
\item{...}{Arguments to 'scan()'}
}

\details{
Reads a lower triangular matrix which at least in my practice, typically come from 'NTSYSpc' program.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
\donttest{
url.show("http://ashipunov.info/data/distances.nts")
download.file("http://ashipunov.info/data/distances.nts", "distances.nts")
distances <- Read.tri.nts("distances.nts", skip=2)
head(distances)
}
}

\keyword{System}

