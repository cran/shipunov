\name{Rows}
\alias{Rows}
\title{Select rows from data frame}

\description{Selects rows from data frame basing on logical on the evaluation of the second argument}

\usage{Rows(df, ...)}

\arguments{
\item{df}{Data frame to select from}
\item{...}{Arguments to with(df, ...)}
}

\details{
If the first argument is not a data frame, function will stop with error.

Rows() is similar to dplyr::filter() function.

Please avoid using Rows() in non-ineractive mode.
}

\value{Data frame}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
`[`(trees, 3, 1) # ... so square bracket is a command
## arguments of `[` are independent; this is why square bracket does not "catch" the context:
trees[trees$Girth < 11 & trees$Height == 65, ] # boring and long
trees[trees$Girth < 11 & sample(0:1, nrow(trees), replace=TRUE), ] # yes, boring, long but flexible
trees[with(trees, Girth < 11 & Height == 65), ] # less boring but still long
## it would be nice to avoid typing "trees" twice:
Rows(trees, Girth < 11 & Height == 65) # shorter
Rows(trees, Girth < 11 & sample(0:1, nrow(trees), replace=TRUE)) # flexibility is still here
Rows(trees, Girth < 11 & sample(0:1, nrow(trees),
 replace=TRUE))$Height # if you want also select columns
Rows(trees, grep(81, Height)) # select not only by TRUE/FALSE but also by row index
}

\keyword{System}

