\name{Life}
\alias{Life}
\title{Game of Life}
\description{Conway's Game of Life}

\usage{Life(n.rows=100, n.cols=100, n.cycles=100, sleep.time=.1, cols=c("#f0f0f0", "#2f81c1"),
 rnd.threshold=0.3)}

\arguments{
\item{n.rows}{number of rows}
\item{n.cols}{number of columns}
\item{n.cycles}{number of cycles}
\item{sleep.time}{time for pause after each cycle}
\item{cols}{main colors}
\item{rnd.threshold}{0 empty board; 1 all squares are filled}
}

\details{Please note that at the moment, the function is not interactive.}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
Life(10, 10, 10, .3)
}

\keyword{Graphical fun}

