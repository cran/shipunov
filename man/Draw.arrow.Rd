\name{Draw.arrow}
\alias{Draw.arrow}
\title{Add arrow to the plot interactively}
\description{Add arrow to the plot interactively}

\usage{Draw.arrow(reverse=FALSE, horizontal=FALSE, vertical=FALSE, length=0.1, ...)}

\arguments{
\item{reverse}{If FALSE, first indicate the tail of the arrow and then the head; if TRUE, first indicate the head of the arrow and then the tail.}
\item{horizontal}{If TRUE, force the arrow to be horizontal; use the average y-axis value of the two clicks for the vertical placement.}
\item{vertical}{If TRUE, force the arrow to be vertical; use the average x-axis value of the two clicks for the horizontal placement.}
\item{length}{Length of the edges of the arrow head.}
\item{...}{arguments to 'arrows()'}
}

\details{'Draw.arrow()' adds arrow to the plot interactively. Based on analogous Karl W Broman function. Uses the 'locator()' function to plot an arrow.}

\value{The locations of the endpoints of the arrow, as a two-row matrix. The first row indicates the location of the tail of the arrow; the second row indicates the location of the head of the arrow.}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{locator}}}

\examples{
plot(1:20)
\dontrun{
## interactive command:
Draw.arrow(col="red", lwd=2)
}
}

\keyword{Graphical fun}
