\name{Alldups}
\alias{Alldups}
\title{Finds all duplicates}
\description{Finds duplicates from both ends, optionally returns indexes of duplicate groups}

\usage{Alldups(v, groups = FALSE)}

\arguments{
\item{v}{vector}
\item{groups}{If TRUE, uses as.numeric(as.character(v)) twice to index duplicated groups with natural numbers (and non-duplicated with 0)}
}

\details{In short, this is extension of unique() which skips the first duplicate in each group. 'NA' count as duplicate but do not count as duplicate group.}

\value{Logical vector of length equal to 'v', or numerical vector if 'groups=TRUE'}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{unique}}}

\examples{
aa <- c("one", "two", "", NA, "two", "three", "three", "three", NA, "", "four")
Alldups(aa)
data.frame(v=aa, dups=Alldups(aa), groups=Alldups(aa, groups=TRUE))
}

\keyword{System}
