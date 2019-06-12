Ellipses <- function(pts, groups, match.color=TRUE, usecolors=NULL, centers=FALSE, c.pch=0, c.cex=3,
 level=0.95, coords=NULL, plot=TRUE, ...)
{
 # Plot an ellipse with covariance matrix C, center b, and P-content level according the F(2, df) distribution
 Confelli <- function(b, C, df=1000, prec=51, ...)
 {
 d <- sqrt(diag(C))
 dfvec <- c(2, df)
 phase <- acos(C[1, 2] / (d[1]*d[2]))
 angles <- seq(-(pi), pi, len=prec)
 mult <- sqrt(dfvec[1] * qf(level, dfvec[1], dfvec[2]))
 xpts <- b[1] + d[1]*mult*cos(angles)
 ypts <- b[2] + d[2]*mult*cos(angles + phase)
 if(plot) lines(xpts, ypts, ...)
 invisible(cbind(xpts, ypts))
 }
 ppts <- list()
 out <- seq(along=groups)
 inds <- names(table(groups))
 if(centers) ppts$centers <- matrix(ncol=2, nrow=length(inds), dimnames=list(inds))
 for (is in inds)
 {
 if (match.color) { m.col <- match(is, inds) } else { m.col <- "black" }
 if (!is.null(usecolors)) m.col <- usecolors[inds == is]
 gr <- out[groups == is]
 X <- pts[gr, ]
 c.X <- apply(X, 2, median)
 if (is.null(coords)) {
 if (length(gr) > 1) ppts[[is]] <- Confelli(b=c.X, C=cov(X), col=m.col, ...)
 } else {
 lines(coords[[is]][, 1], coords[[is]][, 2], col=m.col, ...)
 }
 if (centers) {
 ppts$centers[is, ] <- c(c.X[1], c.X[2])
 if(plot) points(c.X[1], c.X[2], pch=c.pch, cex=c.cex, col=m.col)
 }
 }
 invisible(ppts)
}
