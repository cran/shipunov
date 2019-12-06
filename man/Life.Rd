\name{Life}
\alias{Life}
\title{Game of Life}
\description{Conway's Game of Life}

\usage{Life(n.rows=40, n.cols=40, n.cycles=100, sleep.time=0.05,
 cols=c("#f0f0f0", "#2f81c1"), rnd.threshold=0.3)}

\arguments{
\item{n.rows}{Number of rows}
\item{n.cols}{Number of columns}
\item{n.cycles}{Number of cycles}
\item{sleep.time}{Time for pause after each cycle}
\item{cols}{Main colors}
\item{rnd.threshold}{0 empty board; 1 all squares are filled}
}

\details{Please note that at the moment, this function is not interactive.}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
Life(10, 10, 10, .3)
}

\keyword{Graphical fun}
