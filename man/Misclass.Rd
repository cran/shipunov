\name{Misclass}
\alias{Misclass}
\title{Misclassification (confusion) table}

\description{Misclassification (confusion) table}

\usage{Misclass(pred, obs, best=FALSE, ignore=NULL, quiet=FALSE, ...)}

\arguments{
\item{pred}{Predicted class labels}
\item{obs}{Observed class labels}
\item{best}{Perform a search for the classification table with minimal misclassification rate?}
\item{ignore}{Vector of class labels to ignore}
\item{quiet}{Output summary?}
\item{...}{Arguments to 'table'}
}

\details{'Misclass()' produces misclassification (confusion) 2D table based on two classifications.

The simple variant ('best=FALSE') assumes that class labels are concerted (same number of corresponding classes).

Advanced variant ('best=TRUE') can search for the best classification table (with minimal misclassification rate),
this is especially useful in case of unsupervised classifications which typically return numeric labels. However,
internally it generates all permutations of factor levels, thei could be very slow if there are many class labels.

Variant with 'best=TRUE' might also add empty rows (filled with zeros) to the table in case if numbers of
classes are not equal.

Additional arguments could be passed for table(), for example, 'useNA="ifany"'. If supplied data contains NAs, there
will be also note in the end.

It is possible to ignore some class labels with 'ignore=...', this is useful for methods like DBSCAN
which output special label for outliers. In that case, note about missing data is also issued.

Alternatives: confusion matrix from 'caret::confusionMatrix()' which is more feature rich but much less flexible.

Note that partial "Misclassification errors" are reverse sensitivities, and "Mean misclassification error"
is a reverse accuracy.

Association plot assocplot() is probably the best to plot misclassification table.
}

\value{Invisibly returns the table of class comparison}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
iris.dist <- dist(iris[, -5], method="manhattan")
iris.hclust <- hclust(iris.dist)
iris.3 <- cutree(iris.hclust, 3)
Misclass(iris.3, iris[, 5])

set.seed(1)
iris.k <- kmeans(iris[, -5], centers=3)
Misclass(iris.k$cluster, iris[, 5])
Misclass(iris.k$cluster, iris[, 5], best=TRUE)

res <- Misclass(iris.k$cluster, iris[, 5], best=TRUE, quiet=TRUE)
## statistics implemented in caret::confusionMatrix()
binom.test(sum(diag(res)), sum(res))$conf.int
mcnemar.test(res + 1e-9) # to avoid NaN's

library(dbscan)
iris.db <- dbscan(iris[, -5], eps=0.3)
Misclass(iris.db$cluster, iris$Species, ignore=0, best=TRUE)

set.seed(NULL)
}

\keyword{Statistics}
