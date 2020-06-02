\name{Tcoords}
\alias{Tcoords}
\title{Calculate coordinates of tips from 'hclust' plot}

\usage{
  Tcoords(hcl, hang=0.1, add=0, horiz=FALSE)
}

\arguments{
  \item{hcl}{\code{hclust} object}
  \item{hang}{The fraction of the plot height by which labels should hang below the rest of the plot; better to make it equal to the 'hang' from \code{hclust} (default is 0.1).}
  \item{add}{Add to 'hang' to make labels look better; reliable value is about 0.03}
  \item{horiz}{Plot values for a horizontal tree?}
}

\description{

  This function calculates coordinates for each tip of the plotted
  'hclust' object. It is useful together with plot.hclust(...,
  labels=FALSE).

  There are numerous applications of Tcoords(). Typical situation is when
  user wants to change default labels on plot.hclust() but it is possible
  only after conversion into the dendrogram. However, this conversion
  might alter the graphical representation, and, what is worse, is not
  suitable for advanced forms of plot.hclust(), for example, in 'pvclust'
  package or those from Bclust() or Jclust()-related commands.

  Tcoords() allows to plot labels (or points, or any low lewel
  structures) separately. Therefore, it is possible to rotate them,
  colorize them, abbreviate them, change their font and so on. By
  default, labels will be plotted horizontally, not vertically as it is
  in plot.hclust().

  To plot new labels in the same order, do not forget to apply the
  'order' component of 'hclust' object (see below for the example).

}

\seealso{
  \code{\link{hclust}}
}

\examples{

hcl <- hclust(UScitiesD, "ward.D2")
newlabels <- abbreviate(hcl$labels, 3)
newlabels <- newlabels[hcl$order] # do not forget to reorder labels!
plot(hcl, labels=FALSE)
text(Tcoords(hcl, add=0.03), newlabels, col=ifelse(grepl("W.D", newlabels), 2, 1))

plot(hcl, labels=FALSE)
points(Tcoords(hcl), pch=19)

## can be used with Ploth(), i.e., with dendrogram
Ploth(hcl, labels=NA, horiz=TRUE) # hang=-1 is default
tc <- Tcoords(hcl, hang=-1, horiz=TRUE)
text(tc, newlabels, pos=4, xpd=TRUE)

}

\keyword{cluster}
