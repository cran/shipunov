\name{Rpart2newick}
\alias{Rpart2newick}
\title{Converts 'rpart' object into Newick tree}

\description{Converts 'rpart' object into Newick tree}

\usage{
Rpart2newick(rpart.object)
}

\arguments{
  \item{rpart.object}{'rpart' object, output of rpart::rpart()}
}

\value{
 Newick tree (text string).
}

\details{
Inspired by 'shaunpwilkinson/rpart2dendro.R' gist. Keeps only tip labels.
}

% \references{
% }

% \seealso{
% }

\examples{
library(rpart)
fit <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis)
plot(fit); text(fit)
library(ape)
plot(read.tree(text=Rpart2newick(fit)))
}

\keyword{Multivariate}
