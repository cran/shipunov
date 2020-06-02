\name{Hulls}
\alias{Hulls}
\title{Groups' hulls}
\description{Groups' hulls with centroids calculation}

\usage{Hulls(pts, groups, match.colors=TRUE, usecolors=NULL,
 plot=TRUE, centers=FALSE, c.pch=0, c.cex=3,
 outliers=TRUE, coef=1.5, ...)}

\arguments{
\item{pts}{Data points to plot, 2-dimensional}
\item{groups}{Grouping variable, any type}
\item{match.colors}{Match colors with groups}
\item{usecolors}{Which group colors to use (does not rotate)}
\item{plot}{Plot?}
\item{centers}{Show centers?}
\item{c.pch}{Color of center points}
\item{c.cex}{Scale of center points}
\item{outliers}{Include outliers?}
\item{coef}{Determines how to detect outliers, see 'coef' from 'boxplot.stats()' }
\item{...}{Arguments to 'lines()'}
}

\details{

Groups' hulls with optional centroids calculation (requires
'PBSmapping').

If 'outliers=FALSE', uses 'boxplot.stats()' to detect outliers (points
which are most distant from centers). This option could be used for
cluster sharpening. It also automatically switches to 'centers=TRUE' so
if you want to plot smoothed hulls but do not want to plot their centers,
use something like 'c.pch=NA' or 'c.cex=0'.

}

\value{

Invisibly outputs list of hulls with coordinates, and possibly also with
'centers' and 'outliers' components

}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Ellipses}}, \code{\link{boxplot.stats}}}

\examples{
iris.p <- prcomp(iris[, -5], scale=TRUE)$x[, 1:2]
plot(iris.p, type="n", xlab="PC1", ylab="PC2")
pal <- rainbow(3)
text(iris.p, labels=abbreviate(iris[, 5], 1, method="both.sides"),
 col=pal[as.numeric(iris[, 5])])
Hulls(iris.p, iris[, 5], centers=TRUE, usecolors=pal)

## smoothed hulls
plot(iris.p, col=iris$Species, xlab="PC1", ylab="PC2")
ppts <- Hulls(iris.p, iris[, 5], centers=TRUE, outliers=FALSE, coef=2)
ppts$outliers
}

\keyword{aplot}
