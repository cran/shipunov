\name{Misclass}
\alias{Misclass}
\title{Misclassification (confusion) table}

\description{Misclassification (confusion) table}

\usage{Misclass(pred, obs)}

\arguments{
\item{pred}{Predicted classification}
\item{obs}{Observed classification}
}

\details{'Misclass()' produces misclassification (confusion) 2D table based on two classifications.

Alternatives: confusion matrix from 'caret::confusionMatrix()'

It is assumed that group orders are concerted.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
iris.dist <- dist(iris[, 1:4], method="manhattan")
iris.hclust <- hclust(iris.dist)
iris.3 <- cutree(iris.hclust, 3)
Misclass(iris.3, iris[, 5])
}

\keyword{Statistics}
