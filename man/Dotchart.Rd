\name{Dotchart}
\alias{Dotchart}
\alias{Dotchart1}
\alias{Dotchart3}

\title{Improved dotcharts}
\description{Dotcharts, improved and extended}

\usage{
Dotchart1(x, labels=NULL, groups=NULL, gdata=NULL, cex=par("cex"), pt.cex=cex,
 pch=21, gpch=21, bg=par("bg"), color=par("fg"), gcolor=par("fg"), lcolor="gray",
 xlim=range(x[is.finite(x)]), main=NULL, xlab=NULL, ylab=NULL, ...)

Dotchart(...)

Dotchart3(values, left, right, pch=21, bg="white", pt.cex=1.2, lty=1, lwd=2,
 gridcol="grey", ...)
}

\arguments{
\item{x}{Either a vector or matrix of numeric values. Inputs are coerced by 'as.numeric()', with a message.}
\item{labels}{A vector of labels for each point.}
\item{groups}{An optional factor indicating how the elements of 'x' are grouped.}
\item{gdata}{Data values for the groups. This is typically a summary such as the median or mean of each group.}
\item{cex}{The character size to be used.}
\item{pt.cex}{The 'cex' to be applied to plotting symbols.}
\item{pch}{The plotting character or symbol to be used.}
\item{gpch}{The plotting character or symbol to be used for group values.}
\item{bg}{The background color of plotting characters.}
\item{color}{The color(s) to be used for points and labels.}
\item{gcolor}{The single color to be used for group labels and values.}
\item{lcolor}{The color(s) to be used for the horizontal lines.}
\item{xlim}{Horizontal range for the plot.}
\item{main}{Overall title for the plot, see 'title'.}
\item{xlab, ylab}{Axis annotations as in 'title'.}
\item{values}{Centers for 'Dotchart3()'}
\item{left}{Left margins for 'Dotchart3()'}
\item{right}{Right margins for 'Dotchart3()'}
\item{lty}{Line type for 'Dotchart3()'}
\item{lwd}{Line width for 'Dotchart3()'}
\item{gridcol}{Grid color for 'Dotchart3()'}
\item{...}{Additional arguments}
}

\details{
For better explanations of options, see 'help(dotchart)'.

'Dotchart1()' is a default 'dotchart()' corrected for use with 1-dimensional tables with 'ylab' and/or changed 'par("mar")[2]'. So comparing with the default 'dotchart()', it has a better left margin. It also outputs message instead of warning.

'Dotchart()' is a prettified 'Dotchart1()' with the following defaults: 'Dotchart1(lcolor="black", bg="white", pt.cex=1.2, ...)'. Use it as a shortcut.

'Dotchart3()' shows values together with ranges. It is an extension of 'Dotchart1()'; for each value, it shows ranges. Does not work with grouped data. A bit similar to 'Linechart()' but more general.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{dotchart}}, \code{\link{Linechart}}}

\examples{
## Compare:
aa <- table(c(1, 1, 1, 2, 2, 3))
##
Dotchart1(aa, ylab="Ylab") # shows ylab; outputs message instead of warning
dotchart(aa, ylab="Ylab") # does not show ylab; produces warning
##
## changes all margins (note: Dotchart1() cannot forcedly _decrease_ left margin)
old.par <- par(mar=c(1, 10, 1, 1)) ; Dotchart1(aa, ylab="Ylab") ; par(old.par)
## does not change left margin
old.par <- par(mar=c(1, 10, 1, 1)) ; dotchart(aa, ylab="Ylab") ; par(old.par)

Dotchart(aa)

Dotchart3(structure(1:3, names=LETTERS[1:3]), 0:2, 2:4)
Dotchart3(structure(0:2, names=LETTERS[1:3]), 0:2, 2:4, pch="") # ranges only
}

\keyword{Plots}
