\name{Biarrows}
\alias{Biarrows}
\title{Adds correlation arrows to the scatterplot}
\description{Calculates correlations between original and derived variables and plots corresponding arrows}

\usage{Biarrows(deriv, orig, coeffs=NULL, shrink=0.45, closer=0.8,
 tx=NULL, tx.col=2, tx.cex=1, tx.font=1, xpd=TRUE,
 ar.col=2, ar.len=0.1, ...)}

\arguments{
\item{deriv}{Data derived from, e.g., dimension reducion of 'orig'}
\item{orig}{Original data}
\item{coeffs}{(Optional) two-column matrix with proposed coordinates of arrow tips, row names must represent 'orig' variables}
\item{shrink}{How to shrink arrows in relation to 'deriv' ranges, default is 45\% (0.45)}
\item{closer}{How closer to the center (in relation to the text label) is the arrow tip, default is 0.8}
\item{tx}{(Optional) text labels, default are column names of 'orig'}
\item{tx.col}{Color of text labels, default is 2}
\item{tx.cex}{Size of text, default is 1}
\item{tx.font}{Font of text, default is 1}
\item{xpd}{Allow text to go outside of plotting region?}
\item{ar.col}{Color of arrows, default is 2}
\item{ar.len}{Length of the edges of the arrow head (in inches)}
\item{...}{Further arguments to arrows()}
}

\details{

Biarrows() calculates correlations between two sets of variables which
generally belong to the same data: more then one 'orig' variables and
exactly two 'deriv' variables. Then is scales correlations to the 'deriv'
ranges and adds text labels and arrows to the scatterplot of derived
variables. These arrows represent the original variables in relation with
derived variables. Resulted plot may be seen as a biplot which
simultaneously shows two sets of variables.

This approach might work for data derived from (almost) any kind of
dimensional reduction.

If 'deriv' data contains more then 2 variables, they will be discarded.
It is supposed that 'deriv' data is centered around 0. If not, use
scale() to center it (see examples). Both 'deriv' and 'orig' should be
either data frames or matrices with compatible dimensions.

Biarrows(dr, coeffs=...) allows to use pre-calculated coefficients. In
this case, 'data' is ignored, and 'coeffs' will be scaled. See examples
to understand better how it works.

}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{biplot}}}

\examples{

iris.cmd <- cmdscale(dist(iris[, -5]))
plot(iris.cmd, xlab="Dim 1", ylab="Dim 2")
Biarrows(iris.cmd, iris[, -5])

# ===

library(MASS)
iris.mds <- isoMDS(dist(iris[, -5]) + 1e-9)
plot(iris.mds$points, xlab="Dim 1", ylab="Dim 2")
Biarrows(iris.mds$points, iris[, -5])

# ===

iris.p <- prcomp(iris[, -5], scale=TRUE)
biplot(iris.p, xpd=TRUE, main="Original PCA biplot")
plot(iris.p$x)
Biarrows(iris.p$x, iris[, -5])
title(main="PCA biplot with Biarrows()")

# ===

# how to use 'coeffs'
plot(iris.p$x, xlab="PCA1", ylab="PCA2")
coeffs <- cor(iris[, -5], iris.p$x, method="spearman")
Biarrows(iris.p$x, coeffs=coeffs)

# ===

plot(scale(iris[, c(1, 3)]))
Biarrows(scale(iris[, c(1, 3)]), iris.p$x)
title(main="\"Reversed biplot\"")

# ===

plot(scale(iris[, c(1, 3)]))
Biarrows(scale(iris[, c(1, 3)]), iris[, c(2, 4)])
title(main="Iris flowers: lengths vs. widths")

}

\keyword{aplot}
