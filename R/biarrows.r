Biarrows <- function(
 deriv,
 orig,
 coeffs=NULL,
 shrink=0.45,
 closer=0.8,
 tx=NULL,
 tx.col=2,
 tx.cex=1,
 tx.font=1,
 xpd=TRUE,
 ar.col=2,
 ar.len=0.1,
 ...) {
 deriv <- deriv[, 1:2] # all other variables discarded
 ranges <- apply(deriv, 2, function(.x) max(.x, na.rm=TRUE) - min(.x, na.rm=TRUE))
 if(is.null(coeffs)) coeffs <- cor(orig, deriv, method="pearson", use="pairwise.complete.obs")
 if(is.null(tx)) tx <- row.names(coeffs) # originated from 'orig' column names
 x <- coeffs[, 1] * ranges[1] * shrink
 y <- coeffs[, 2] * ranges[2] * shrink
 text(x, y, col=tx.col, cex=tx.cex, font=tx.font, labels=tx, xpd=xpd)
 arrows(0, 0, x*closer, y*closer, col=ar.col, length=ar.len, ...)
}
