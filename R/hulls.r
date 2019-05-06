Hulls <- function(pts, groups, match.color=TRUE, usecolors=NULL, plot=TRUE,
 centers=FALSE, c.pch=0, c.cex=3, outliers=TRUE, coef=1.5, ...)
{
ppts <- list()
out <- seq(along=groups)
inds <- names(table(groups))
for (is in inds) {
 if (match.color) {m.col <- match(is, inds)} else {m.col <- "black"}
 if (!is.null(usecolors)) m.col <- usecolors[match(is, inds)]
 gr <- out[groups == is]
 if (length(gr) > 1) {
 X <- pts[gr, ]
 hpts <- chull(X)
 ppts[[is]] <- X[hpts, ]
 hpts.l <- c(hpts, hpts[1])
 if (plot & outliers) lines(X[hpts.l, ], col=m.col, ...)
 }
 }
if(!outliers) centers <- TRUE
if(centers)
 {
 ppol <- ppts
 len <- length(ppol)
 for (i in 1:len)
 {
 ppol[[i]] <- data.frame(ppol[[i]], PID=i, POS=1:nrow(ppol[[i]]))
 names(ppol[[i]])[1:2] <- c("X", "Y")
 }
 centers <- matrix(ncol=2, nrow=len)
 for (i in 1:len) centers[i,] <- unlist(PBSmapping::calcCentroid(ppol[[i]])[c("X", "Y")])
 if (match.color) {m.col <- 1:len} else {m.col <- "black"}
 if (!is.null(usecolors)) m.col <- usecolors
 if (plot & outliers) points(centers, pch=c.pch, cex=c.cex, col=m.col)
 row.names(centers) <- names(ppol)
 ppts$centers <- centers
 }
if(!outliers) {
 if (plot) points(centers, pch=c.pch, cex=c.cex, col=m.col)
 lout <- numeric(0)
 for (is in inds) {
  if (match.color) {m.col <- match(is, inds)} else {m.col <- "black"}
  if (!is.null(usecolors)) m.col <- usecolors[match(is, inds)]
  gr <- out[groups == is]
  if (length(gr) > 1) {
  X <- pts[gr, ]
  C <- centers[match(is, inds), ]
  D <- apply(X, 1, function(.x) sqrt(sum((.x - C)^2)))
  outliers <- D %in% boxplot.stats(D, coef=coef)$out
  X <- X[!outliers, ]
  hpts <- chull(X)
  ppts[[is]] <- X[hpts, ]
  hpts.l <- c(hpts, hpts[1])
  if (plot) lines(X[hpts.l, ], col=m.col, ...)
  }
  lout <- c(lout, (1:nrow(pts))[gr][outliers])
 }
 ppts$outliers <- lout
}
invisible(ppts)
}
