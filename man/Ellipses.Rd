\name{Ellipses}
\alias{Ellipses}
\title{Groups' confidence ellipses}
\description{Groups' confidence ellipses}

\usage{
Ellipses(pts, groups, match.color=TRUE, usecolors=NULL, centers=FALSE,
 c.pch=0, c.cex=3, level=0.95, ...)
}

\arguments{
\item{pts}{Data points to plot}
\item{groups}{Grouping variable (mumerical)}
\item{level}{Confidence level}
\item{match.color}{Match colors}
\item{usecolors}{Use colors (palette)}
\item{centers}{Show centrs?}
\item{c.pch}{Color of center points}
\item{c.cex}{Scale of center points}
\item{...}{Arguments to 'Confelli()' and finally to 'plot()'}
}

\details{
Internally, uses 'Confelli()' function which plots an ellipse with covariance matrix C, center b, and P-content level according the F(2, df) distribution.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Hulls}}}

\examples{
iris.p <- prcomp(iris[, 1:4], scale=TRUE)$x[, 1:2]
plot(iris.p, type="n", xlab="PC1", ylab="PC2")
text(iris.p, labels=abbreviate(iris[, 5], 1, method="both.sides"))
Ellipses(iris.p[, 1:2], as.numeric(iris[, 5]), centers=TRUE)
}

\keyword{Plots}

