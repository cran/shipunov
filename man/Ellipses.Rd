\name{Ellipses}
\alias{Ellipses}
\title{Groups' confidence ellipses}
\description{Groups' confidence ellipses}

\usage{
Ellipses(pts, groups, match.color=TRUE, usecolors=NULL, centers=FALSE, c.pch=0, c.cex=3,
 level=0.95, coords=NULL, plot=TRUE, ...)
}

\arguments{
\item{pts}{Data points to plot}
\item{groups}{Grouping variable}
\item{level}{Confidence level}
\item{match.color}{Match colors}
\item{usecolors}{Use colors (palette)}
\item{centers}{Show centers?}
\item{c.pch}{Color of center points}
\item{c.cex}{Scale of center points}
\item{coords}{List of coordinates: two-column matrices named as groups, does not need by default}
\item{plot}{Plot?}
\item{...}{Arguments to 'Confelli()' and finally to 'lines()'}
}

\details{
Uses internal 'Confelli()' function which plots an ellipse with
covariance matrix C, center b, and P-content level
according to the F(2, df) distribution.
}

\value{
Invisibly returns the list in form similar to Hulls(), to use as 'coords' or with Overlap()
}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Hulls}}}

\examples{
iris.p <- prcomp(iris[, 1:4], scale=TRUE)$x[, 1:2]
plot(iris.p, type="n", xlab="PC1", ylab="PC2")
text(iris.p, labels=abbreviate(iris[, 5], 1, method="both.sides"))
Ellipses(iris.p[, 1:2], iris[, 5], centers=TRUE)

library(MASS)
ch.lda <- lda(Species ~ ., data=chaetocnema[, -2])
ch.lda.pred <- predict(ch.lda, chaetocnema[, -(1:2)])
## ellipses here are by default bigger then plot so use workaround:
ee <- Ellipses(ch.lda.pred$x, chaetocnema$Species, plot=FALSE)
xx <- range(c(do.call(rbind, ee)[, 1], ch.lda.pred$x[, 1]))
yy <- range(c(do.call(rbind, ee)[, 2], ch.lda.pred$x[, 2]))
plot(ch.lda.pred$x, col=chaetocnema$Species, xlim=xx, ylim=yy)
Ellipses(ch.lda.pred$x, chaetocnema$Species, coords=ee)

## search for the maximal level which gives zero overlap
plot(x5 ~ x17, data=haltica, pch=as.numeric(haltica$Species))
for (i in (99:59)/100) {
cat(i, "\n")
ee <- Ellipses(haltica[, c("x17", "x5")], haltica$Species, level=i, plot=FALSE)
print(mean(Overlap(ee), na.rm=TRUE))
cat("\n")
}
Ellipses(haltica[, c("x17", "x5")], haltica$Species, level=.62)
}

\keyword{aplot}
