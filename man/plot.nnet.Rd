\name{plot.nnet}
\alias{plot.nnet}
\title{Plots 'nnet' object}
\description{Plots 'nnet' object}

\usage{
\method{plot}{nnet}(x, ..., nid=TRUE, all.out=TRUE, all.in=TRUE, wts.only=FALSE,
 rel.rsc=5, circle.cex=5, node.labs=TRUE, line.stag=NULL, cex.val=1, alpha.val=1,
 circle.col="lightgrey", pos.col="black", neg.col="grey")
}

% TODO: expand argument descriptions
\arguments{
\item{x}{'nnet' model}
\item{...}{arguments to 'plot()'}
\item{nid}{TRUE}
\item{all.out}{TRUE}
\item{all.in}{TRUE}
\item{wts.only}{FALSE}
\item{rel.rsc}{5}
\item{circle.cex}{5}
\item{node.labs}{TRUE}
\item{line.stag}{NULL}
\item{cex.val}{1}
\item{alpha.val}{1}
\item{circle.col}{"lightgrey"}
\item{pos.col}{"black"}
\item{neg.col}{"grey"}
}

\details{Plots 'nnet' object. Requires 'nnet' and 'scales' libraries.
Based on the code from Marcus W Beck.}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
library(nnet)
iris.train <- iris[seq(1, nrow(iris), 5), ]
iris.unknown <- iris[-seq(1, nrow(iris), 5), ]
iris.nnet <- nnet(Species ~ . , data=iris.train, size=4)
iris.predicted <- predict(iris.nnet, iris.unknown[, -5], type="class")
Misclass(iris.predicted, iris.unknown[, 5])
##
oldpar <- par(mar=c(0, 0, 0, 0))
plot(iris.nnet)
par(oldpar)
}

\keyword{Special multivariate plots}
