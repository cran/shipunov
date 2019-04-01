\name{Missing.map}
\alias{Missing.map}
\title{Textual plot of missing data}

\description{Textual plot of missing data}

\usage{Missing.map(df)}

\arguments{
\item{df}{Data frame with any data}
}

\details{'Missing.map()' makes textual plot of missing data, inspired by 'DescTools::PlotMiss()'.}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
sl <- read.table("http://ashipunov.info/shipunov/open/salix_leaves.txt", h=TRUE)
Missing.map(sl)
}

\keyword{Statistics}
