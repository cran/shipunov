\name{BootRF}
\alias{BootRF}
\title{Bootstrap with 'randomForest()'}
\description{How to bootstrap with 'randomForest()'}

\usage{BootRF(data, classes, sub="none", nsam=4, nboot=1000, misclass=TRUE)}

\arguments{
\item{data}{Data frame to classify}
\item{classes}{Character vector of class names}
\item{sub}{Subsample to use (see example)}
\item{nsam}{Number of training items from each level of grouping factor, default 4}
\item{nboot}{Number of iterations}
\item{misclass}{Calculate misclassification table?}
}

\details{
This an example of how to bootstrap with 'randomForest::randomForest()'.

Samples equal numbers ('nsam') of training items from \emph{each level} of grouping factor.

Allows to use \emph{sub}set of data which will be used for subsampling of training data.
}

\value{Returns all predictions as character matrix, each boot is a column}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{randomForest::\link[randomForest]{randomForest}}, \code{\link{Dev}}}

\examples{
iris.sub <- 1:nrow(iris) \%in\% seq(1, nrow(iris), 5)
\donttest{
## could be slow
iris.bootrf <- BootRF(iris[, -5], iris[, 5], sub=iris.sub)
iris.bootrf <- BootRF(iris[, -5], iris[, 5])
## naturally, in the second case misclassification rate is lower
}
}

\keyword{Statistics}

