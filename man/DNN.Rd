\name{DNN}
\alias{DNN}

\title{Distance matrix based kNN classification}

\description{
  \code{DNN} uses pre-cooked distance matrix to replace missing values in class labels.
}

\usage{
DNN(dst, cl, k, d, details=FALSE)
}

\arguments{
  \item{dst}{Distance matrix (object of class 'dist').}
  \item{cl}{Class labels, should contain NAs to designate testing sub-group.}
  \item{k}{How many neighbors to select, odd numbers preferable. If specified, do not use "d".}
  \item{d}{Distance to consider for neighborhood, in fractions of maximal distance. If specified, do not use "k".}
  \item{details}{If TRUE, function will return voting matrix. Default is FALSE.}
}

\details{
  If classic kNN is a lazy classifier, DNN is super-lazy because it does not even
  calculate the distance matrix itself. Instead, you supply it with distance matrix
  (object of class 'dist') pre-computed with _any_ possible tool. This lifts
  many restrictions. For example, arbitrary distance could be used (like Gower distance
  which allows any type of variable). This is also much faster than typical kNN.

  In addition to neighbor-based kNN classification, DNN implements _neighborhood_ classification
  when all neighbors within selected distance used for voting.

  As usual in kNN, ties are broken at random. DNN also controls situations when no neighbors
  are within the given distance (and returns NA), and also when all neighbors are relevant (also returns NA).

  By default, DNN() returns missing part of class labels, completely or partially filled with new
  (predicted) class labels. If 'cl' has no NAs, DNN() returns it back with warning. It allows for
  combined and stepwise extensions (see examples). If 'details=TRUE', DNN() will return matrix
  where each column represents the table used for voting.
}

\value{
  Character vector with predicted class labels; or matrix if 'details=TRUE'.
}

\author{Alexey Shipunov}

\seealso{
  class::\code{\link[class]{knn}}
}

\examples{
iris.d <- dist(iris[, -5])

cl1 <- iris$Species
sam <- c(rep(0, 4), 1) > 0
cl1[!sam] <- NA
table(cl1, useNA="ifany")

## based on neighbor number
iris.pred <- DNN(dst=iris.d, cl=cl1, k=5)
Misclass(iris$Species[is.na(cl1)], iris.pred)

## based on neighborhood size
iris.pred <- DNN(dst=iris.d, cl=cl1, d=0.05)
table(iris.pred, useNA="ifany")
Misclass(iris$Species[is.na(cl1)], iris.pred)

## protection against "all points relevant"
DNN(dst=iris.d, cl=cl1, d=1)[1:5]
## and all are ties:
DNN(dst=iris.d, cl=cl1, d=1, details=TRUE)[, 1:5]

## any distance works
iris.d2 <- Gower.dist(iris[, -5])
iris.pred <- DNN(dst=iris.d2, cl=cl1, k=5)
Misclass(iris$Species[is.na(cl1)], iris.pred)

## combined
cl2 <- cl1
iris.pred <- DNN(dst=iris.d, cl=cl2, d=0.05)
cl2[is.na(cl2)] <- iris.pred
table(cl2, useNA="ifany")
iris.pred2 <- DNN(dst=iris.d, cl=cl2, k=5)
cl2[is.na(cl2)] <- iris.pred2
table(cl2, useNA="ifany")
Misclass(iris$Species, cl2)

## stepwise, note the warning when no NAs left
cl3 <- cl1
for (d in (5:14)/100) {
iris.pred <- DNN(dst=iris.d, cl=cl3, d=d)
cl3[is.na(cl3)] <- iris.pred
}
table(cl3, useNA="ifany")
Misclass(iris$Species, cl3)
## rushing to d=14\% gives much worse results
iris.pred <- DNN(dst=iris.d, cl=cl1, d=0.14)
table(iris.pred, useNA="ifany")
Misclass(iris$Species[is.na(cl1)], iris.pred)
}

\keyword{Multivariate}
