\name{Recode}
\alias{Recode}
\alias{Recode4}
\alias{RecodeR}
\alias{Recode4R}

\title{Basic multiple recoding (similar to 'SQL' left join)}
\description{Basic multiple recoding (similar to 'SQL' left join)}

\usage{
Recode(var, from, to, char=TRUE)
Recode4(var, from, to, missed="")
RecodeR(var, from, to, char=TRUE)
Recode4R(var, from, to, missed="")
}

\arguments{
\item{var}{variable to recode}
\item{from}{'from' column of the recoding table}
\item{to}{'to' column}
\item{char}{if TRUE, do not treat character vectors as factors}
\item{missed}{replace missed with something, default is ""}
}

\details{Recode Basic multiple recoding are similar to 'SQL' left join.

Inspired from Paul Johnston (Univ. of Kansas) 'recode()' function.

Alternatives are 'car::recode()', 'lessR::Recode()' and 'mgsub' package.

To understand the idea better, look on the examples.

There are four functions:

1. Recode() -- base function. If starting points ("from") are the same, only the \emph{last} rule ("from-to" pair) has an effect
If rules are chained, they still work independently (i.e., chaining has no effect).

2. Recode4() -- considers missing. By default, this will change non-Recode()'d entries with empty string ("")

3. RecodeR() -- running recode. If starting points ("from") are the same, only the \emph{first} rule ("from-to" pair) has an effect. Chaining is possible

4. Recode4R() -- running plus considers missing. By default, this will change non-RecodeR()'ed entries with empty string ("")
}

\value{Recoded vector}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
phrase <- "The quick brown fox jumps over 123 lazy dogs"
var <- unlist(strsplit(phrase, split=""))
from <- letters[1:20]
to <- rev(from)
Recode.result <- paste(Recode(var, from, to), collapse="")
Recode4.result <- paste(Recode4(var, from, to, missed="-"), collapse="")
RecodeR.result <- paste(RecodeR(var, from, to), collapse="")
Recode4R.result <- paste(Recode4R(var, from, to, missed="-"), collapse="")
from.rule <- paste(from, collapse=" ")
to.rule <- paste(to, collapse=" ")
rbind(from.rule, to.rule, phrase, Recode.result, Recode4.result, RecodeR.result, Recode4R.result)
}

\keyword{Multiple recode}

