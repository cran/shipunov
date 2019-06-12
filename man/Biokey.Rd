\name{Biokey}
\alias{Biokey}
\alias{Numranks}

\title{Convert diagnostic keys and classification lists}
\description{Convert the oldest biological data structures: diagnostic keys and classification lists}

\usage{
Biokey(data, from=NULL, to=NULL, recalculate=TRUE, internal=FALSE)
Numranks(nums=NULL, ranks=NULL, add=NULL, empty="Species")
}

\arguments{
\item{data}{Key-like data (see 'keys'), or other hierarchical data (see 'classifs') and also in details below}
\item{from}{Data type to convert from}
\item{to}{Data type to convert to}
\item{recalculate}{Recalculate the numeric ids?}
\item{internal}{Output internal 4-column 'key' objects instead?}
\item{nums}{Numbers to convert into ranks}
\item{ranks}{Ranks to convert into numbers}
\item{add}{Rank-number conversion rule to add (overrides embedded rules)}
\item{empty}{What rank to use for empty number?}
}

\details{
Biokey() is a way to convert classification lists ('classifs') or diagnostic keys into keys of other types or classifs.
Biokey() is able also to convert keys or classifs into more external data structures:
classification tables ('to="table"') or Newick trees ('to="newick"'.)
To know which conversions are allowed, simply type Biokey() whithout arguments (this will also induce the error message).

Numranks() converts biological rank names into numbers and numbers into rank names (Shipunov, 2017).
To see the embedded conversion table, type Numranks() without arguments.

To know more about keys and classifs, read 'classifs' and 'keys' help files.
}

\value{
Typically, the data frame or just a character string (in case of Newick output).
If 'internal=TRUE', outputs a standardized 4-column data frame in a form of branched key (columns 'id', 'description', 'terminal'),
plus 'goto' column which might be just NAs.
}

\references{
Shipunov A. 2017. "Numerical ranks" to improve biological nomenclature of higher groups. 2017. URL: 'https://arxiv.org/abs/1708.07260'
}

\author{Alexey Shipunov}

\seealso{\code{\link{classifs}}, \code{\link{keys}}}

\examples{
## Biokey() # will induce error message but also show which conversions are available
Numranks() # will show the conversion table

Numranks(nums=1:7)
Numranks(ranks="kingdom") # "kingdom", "order" and "family" will be translated into Latin

## three branched keys
i1 <- c("1 A ", "2 B Name1", "2 BB Name2", "1 AA ", "3 C Name3", "3 CC Name4")
i2 <- c("1 A Name1", "2 B Name2", "2 BB ", "3 C Name3", "3 CC Name4")
i3 <- c("1 A Name1", "2 B Name2", "2 BB ", "3 C Name3",
 "3 CC Name4", "2 BBB ", "4 D Name5", "4 DD Name6", "4 DDD Name7")
k1 <- read.table(textConnection(i1), sep=" ", as.is=TRUE)
k2 <- read.table(textConnection(i2), sep=" ", as.is=TRUE)
k3 <- read.table(textConnection(i3), sep=" ", as.is=TRUE)

## convert them into phylogeny trees and plot
t1 <- Biokey(k1, from="branched", to="newick")
t2 <- Biokey(k2, from="branched", to="newick")
t3 <- Biokey(k3, from="branched", to="newick")
library(ape) # load 'ape' to plot Newick trees
plot(read.tree(text=t1))
plot(read.tree(text=t2))
plot(read.tree(text=t3))
}

\keyword{Strictly biological}
