\name{Gradd}
\alias{Gradd}
\title{Classification grid}
\description{Adds to the 2D ordination plot small semi-transparent points which make color classification grid}

\usage{
Gradd(model2var, data2var, spacing=75, trnsp=0.3, pch=20, cex=0.2,
 palette=NULL, type="ids", User.Predict=function(model2var, X) {}, ...)
}

\arguments{
\item{model2var}{Model based on 'data2var' (see below).}
\item{data2var}{Data with exactly 2 variables.}
\item{spacing}{Space between points.}
\item{trnsp}{Transparency.}
\item{pch}{Type of point.}
\item{cex}{Scale of points.}
\item{palette}{Palette to use.}
\item{type}{Type of the model: "ids", "lda", "neuralnet", "tree", or "user" (see examples).}
\item{User.Predict}{Function to define in case of 'type="user"'.}
\item{...}{Additional arguments to plot().}
}

\details{
'Gradd()' adds to the 2D ordination plot small semi-transparent points which make color classification grid.

Requires model with 'predict' method to be computed first.

Model should use ids (to make colors) and exactly 2 variables with names same as 'data2var' column names, e.g:

model2var <- somefunction(ids ~ ., data=cbind(ids, data2var))

If type="user", uses predefined 'User.Predict(model2var, X)' function which must return factor ids from testing X data.

Please see examples to understand all of these better.

Note that instead of dots, one can use contours, but they are harder to employ because they need membership values in order to calculate borders (places where memberships are equal).
}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
## SVM:
library(e1071)
iris.p <- prcomp(iris[, 1:4], scale=TRUE)$x[, 1:2]
iris.svm.pca <- svm(Species ~ ., data=cbind(iris[5], iris.p))
plot(iris.p, type="n", main="SVM")
Gradd(iris.svm.pca, iris.p)
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))

## LDA:
library(MASS)
iris.p <- prcomp(iris[, 1:4], scale=TRUE)$x[, 1:2]
iris.lda.pca <- lda(Species ~ . , data=cbind(iris[5], iris.p))
plot(iris.p, type="n", main="LDA")
Gradd(iris.lda.pca, iris.p, type="lda")
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))

## 'tree::tree':
library(tree)
iris.p <- prcomp(iris[, 1:4], scale=TRUE)$x[, 1:2]
iris.tree.pca <- tree(Species ~ . , data=cbind(iris[5], iris.p))
plot(iris.p, type="n", main="tree")
Gradd(iris.tree.pca, iris.p, type="tree")
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))

## randomForest:
library(randomForest)
iris.p <- prcomp(iris[, 1:4], scale=TRUE)$x[, 1:2]
iris.rf.pca <- randomForest(Species ~ ., data=cbind(iris[5], iris.p))
plot(iris.p, type="n", main="randomForest")
Gradd(iris.rf.pca, iris.p)
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))

## naiveBayes:
library(e1071)
iris.p <- prcomp(iris[, 1:4], scale=TRUE)$x[, 1:2]
iris.nb.pca <- naiveBayes(Species ~ ., data=cbind(iris[5], iris.p))
plot(iris.p, type="n", main="naiveBayes")
Gradd(iris.nb.pca, iris.p)
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))

## neuralnet:
library(neuralnet)
iris.p2 <- prcomp(iris[, 1:4], scale=TRUE)$x[, 1:2]
iris.p2 <- cbind(iris.p2, Tobin(iris$Species, convert.names=FALSE))
iris.nn.pca <- neuralnet(setosa + versicolor + virginica ~ PC1 + PC2, data=iris.p2,
 hidden=3, lifesign="full")
plot(iris.p, type="n", main="neuralnet")
Gradd(iris.nn.pca, iris.p, type="neuralnet")
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))

## rpart + MDS for the base plot:
iris.dist <- dist(iris[, 1:4], method="manhattan")
iris.dist[iris.dist == 0] <- abs(jitter(0))
library(MASS)
iris.m <- isoMDS(iris.dist)$points
colnames(iris.m) <- c("Dim1", "Dim2")
library(rpart)
iris.rpart.mds <- rpart(Species ~ . , data=cbind(iris[5], iris.m))
plot(iris.m, type="n", main="rpart + MDS")
Gradd(iris.rpart.mds, iris.m, type="tree")
text(iris.m, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))

## QDA:
library(MASS)
iris.p <- prcomp(iris[, 1:4], scale=TRUE)$x[, 1:2]
iris.qda.pca <- qda(Species ~ . , data=cbind(iris[5], iris.p))
plot(iris.p, type="n", main="QDA")
Gradd(iris.qda.pca, iris.p, type="lda")
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))

## AdaBoost:
\donttest{
library(adabag)
iris.p <- prcomp(iris[, 1:4], scale=TRUE)$x[, 1:2]
iris.ada.pca <- boosting(Species ~ . , data=cbind(iris[5], iris.p)) # slow!
plot(iris.p, type="n", main="AdaBoost")
Gradd(iris.ada.pca, iris.p, type="lda")
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))
}

## nnet:
library(nnet)
iris.p <- prcomp(iris[, 1:4], scale=TRUE)$x[, 1:2]
iris.nnet.pca <- nnet(Species ~ . , data=cbind(iris[5], iris.p), size=4)
plot(iris.p, type="n", main="nnet")
Gradd(iris.nnet.pca, iris.p, type="tree")
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))

## kNN:
library(class)
iris.p <- prcomp(iris[, 1:4], scale=TRUE)$x[, 1:2]
plot(iris.p, type="n", main="kNN")
Gradd(cbind(iris[5], iris.p), iris.p, type="user",
 User.Predict=function(model2var, X) knn(model2var[, 2:3], X, model2var[, 1], k=5))
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))
}

\keyword{aplot}
