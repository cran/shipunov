\name{Jclust}
\alias{Jclust}
\alias{plot.Jclust}
\alias{print.Jclust}
\title{Simple bootstrap and jackknife clustering}

\description{Simple bootstrap and jackknife clustering}

\usage{
Jclust(data, n.cl, iter=100, method.d="manhattan", method.c="ward.D", bootstrap=TRUE)
\method{print}{Jclust}(x, ...)
\method{plot}{Jclust}(x, main="", xlab="", sub=NULL, rect.lty=3, rect.col=1, ...)
}

\arguments{
\item{data}{Data}
\item{n.cl}{Number of desired clusters}
\item{iter}{Number of iterations}
\item{method.d}{Distance method}
\item{method.c}{Hierarchical clustering method}
\item{bootstrap}{Bootstrap or jackknife?}
\item{x}{Object of the class 'Jclust'}
\item{rect.lty}{Line type for the rectangles}
\item{rect.col}{Color of rectangles}
\item{main}{Plot title}
\item{xlab}{Horizontal axis label}
\item{sub}{Horizontal axis sub-label}
\item{...}{Additional arguments to the 'print()' or 'plot.hclust()'}
}

\details{

Simple method to bootstrap and jackknife cluster memberships, and plot
consensus membership tree. Requires the desired number of clusters.

}

\value{Returns 'Jclust' object.}

% \references{}

\author{Alexey Shipunov}

\seealso{
  \code{\link{Bclust}}, \code{\link{BootA}}
}

\examples{
(mo.j <- Jclust(t(moldino), 3, iter=1000))
plot(mo.j)

\donttest{
## This is how one can bootstrap _all_ reliable cluster numbers:
for (i in 2:(nrow(t(moldino)) - 1)) print(Jclust(t(moldino), i, iter=1000, boot=TRUE))
}
}

\keyword{cluster}
