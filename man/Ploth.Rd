\name{Ploth}
\alias{Ploth}
\title{Changes the appearance of cluster dendrogram}

\description{Changes the appearance of cluster dendrogram}

\usage{Ploth(hclust, labels=hclust[["labels"]], lab.col=1, col=1, pch.cex=1, pch="",
 bg=0, col.edges=FALSE, ...)}

\arguments{
\item{hclust}{Hclust object}
\item{labels}{Labels}
\item{lab.col}{Label colors}
\item{col}{Colors}
\item{pch.cex}{Scale of points}
\item{pch}{Point types}
\item{bg}{Points backgrounds}
\item{col.edges}{Colorize edges?}
\item{...}{Arguments to 'plot()'}
}

\details{
Changes the appearance of cluster dendrogram.
Be sure to check plot margins!
Alternatives: "dendextend" package.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
iris.dist <- dist(iris[, 1:4], method="manhattan")
iris.hclust <- hclust(iris.dist)
Ploth(iris.hclust, col=as.numeric(iris[, 5]), pch=16, col.edges=TRUE, horiz=TRUE,
 leaflab="none")
legend("topleft", fill=1:nlevels(iris[, 5]), legend=levels(iris[, 5]))
}

\keyword{Plots}
