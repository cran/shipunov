\name{Str}
\alias{Str}

\title{'str()' enchanced for data frames}

\description{Enhanced 'str()': with variable numbers, row names and missing data indication}

\usage{Str(df)}

\arguments{
\item{df}{Data frame}
}

\details{

'Str()' is an enhanced 'str()'. 'Str()' (1) shows data frame structure
with column indexes, (2) indicates presence of NA(s) with star (*) and
(3) lists first five row names, if they are not default.

If the object is a data frame with atomic columns, this function captures
output of internal 'str()', changes it and outputs the new one. If the
object is not a data frame or is a data frame with non-atomic columns,
then output is not changed.

Alternative: DescTools::Str() which uses cycles (slower!), has less
features, but works with non-atomic columns.

}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{str}}}

\examples{
trees1 <- trees
row.names(trees1)[1] <- "a"
trees1[1, 1] <- NA
Str(trees)
Str(trees1)
\dontrun{
trees.crazy <- trees
trees.crazy[[2]] <- trees[, 2, drop=FALSE]
Str(trees.crazy) # does not work with data frames with non-atomic columns
}
}

\keyword{environment}
