\name{Pleiad}
\alias{Pleiad}

\title{Correlation circles (correlation pleiads)}

\description{Correlation circles (correlation pleiads)}

\usage{Pleiad(tbl, abs=FALSE, corr=FALSE, dist=FALSE, treshold=FALSE,
 circ=list(1, 1, 1), breaks=5, auto=TRUE, gr=6, lwd=NULL, lty=NULL, lcol=NULL,
 abbr=-1, lbltext="internal", lblcex=1, off=1.09, hofft=0.07, hoff=1.02, legend=TRUE,
 legtext=1, legpos="topright", leghoriz=FALSE, show.int=FALSE, dig.lab=1, ...)}

\arguments{
\item{tbl}{Data: square matrix}
\item{abs}{If TRUE, uses absolute values istead of real}
\item{corr}{If TRUE, uses absolute values istead of real and cuts from 0 to 1, this is good for correlation matrices}
\item{dist}{If TRUE, converts distance matrix to the data frame -- good for "dist" objects}
\item{treshold}{If this is (saying) =.5, selects for plotting (with lty=1) only those values which are >.5}
\item{circ}{Line type, width and color for the cirle; if first or third =0, no cicrle}
\item{breaks}{How to cut() values, if "cramer", then =c(0, .1, .3, .5, 1)}
\item{auto}{If FALSE, specify lwd, lty and lcol}
\item{gr}{Grayscale scheme starts from 6 breaks}
\item{lwd}{If autolines=FALSE, change to vector concerted with breaks}
\item{lty}{If autolines=FALSE, change to vector concerted with breaks}
\item{lcol}{If autolines=FALSE, change to vector concerted with breaks; if length(lcol) == 1, all lines are of particular color}
\item{abbr}{If =-1, no abbreviation; if =0, no labels; other values run abbreviate(..., abbr)}
\item{lbltext}{If this is a vector starting from something else, will replace dimnames}
\item{lblcex}{Magnification of labels}
\item{off}{Radial offset of labels, be careful!}
\item{hofft}{Treshold determining which labels are rigtmost/leftmost, hofft=0 put all labes into this group}
\item{hoff}{Horizontal offset for rightmost/leftmost labels; hoff=1 removes offset}
\item{legend}{If FALSE, no legend}
\item{legtext}{If =1 then "weaker ... stronger"; if =2, shows cutting intervals; if =3, then 1:5; if >3, issues error}
\item{legpos}{This is from 'legend()'}
\item{leghoriz}{Equal to 'horiz=' from 'legend()'}
\item{show.int}{Show intervals in (...] form}
\item{dig.lab}{dig.lab for 'cut()'}
\item{...}{Arguments to 'points()'}
}

\details{
Correlation circles (correlation pleiads).
Based on the works of Petr Terentjev's (Saint Petersburg) school
Alternatives: packages "igraph", "circlize" and similar
It is probably a good idea to order data entries with hierarchical clustering results to optimize resulted graph
}

\value{Retunts data frame with position of points, helps in subsequent plot enhancing}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
l.c <- cor(datasets::longley, method="spearman", use="pairwise")
Pleiad(l.c, corr=TRUE, legtext=2, pch=21, cex=2, bg="white", breaks=3, gr=3, hoff=1, show.int=TRUE)

dr <- read.table("http://ashipunov.info/shipunov/open/drosera.txt", h=TRUE)
dr.c <- cor(dr[, -1], method="spearman", use="pairwise")
Pleiad(dr.c, corr=TRUE, legtext=2, pch=19, cex=1.2)
}

\keyword{Correlation}
