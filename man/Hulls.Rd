\name{Hulls}
\alias{Hulls}
\title{Groups' hulls}
\description{Groups' hulls with centroids calculation}

\usage{Hulls(pts, groups, match.color=TRUE, usecolors=NULL, plot=TRUE, centers=FALSE,
 c.pch=0, c.cex=3, ...)}

\arguments{
\item{pts}{Data points to plot}
\item{groups}{Grouping variable (mumerical)}
\item{match.color}{Match color}
\item{usecolors}{Use colors (palette)}
\item{plot}{Plot?}
\item{centers}{Show centers?}
\item{c.pch}{Color of center points}
\item{c.cex}{Scale of center points}
\item{...}{Arguments to 'lines()'}
}

\details{
Groups' hulls with optoinal centroids calculation (requires 'PBSmapping')
}

\value{Invisibly outputs list of hulls with coordinates}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Ellipses}}}

\examples{
iris.p <- prcomp(iris[, 1:4], scale=TRUE)$x[, 1:2]
plot(iris.p, type="n", xlab="PC1", ylab="PC2")
pal <- rainbow(3)
text(iris.p, labels=abbreviate(iris[, 5], 1, method="both.sides"),
 col=pal[as.numeric(iris[, 5])])
Hulls(iris.p[, 1:2], as.numeric(iris[, 5]), centers=TRUE, usecolors=pal)
}

\keyword{Plots}
