\name{BootKNN}
\alias{BootKNN}
\title{Bootstrap with kNN}
\description{How to bootstrap with kNN}

\usage{BootKNN(data, classes, sub="none", nsam=4, nboot=1000, misclass=TRUE)}

\arguments{
\item{data}{Data frame to classify}
\item{classes}{Character vector of class names}
\item{sub}{Subsample to use (see example)}
\item{nsam}{Number of training items from each level of grouping factor, default 4}
\item{nboot}{Number of iterations}
\item{misclass}{Calculate misclassification table?}
}

\details{
This is an example of how to bootstrap with 'class::knn1()'.

Samples equal numbers ('nsam') of training items from \emph{each level} of grouping factor.

Allows to use \emph{sub}set of data which will be used for sub-sampling of training data.
}

\value{Returns all predictions as character matrix, each boot is a column}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{class::\link[class]{knn1}}, \code{\link{Dev}}}

\examples{
iris.sub <- 1:nrow(iris) \%in\% seq(1, nrow(iris), 5)
iris.bootknn <- BootKNN(iris[, -5], iris[, 5], sub=iris.sub)
}

\keyword{Statistics}
