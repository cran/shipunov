Bclust <- function(data, method.d="euclidean", method.c="ward.D",
FUN=function(.x) hclust(dist(.x, method=method.d), method=method.c),
iter=1000, mc.cores=1, monitor=TRUE, bootstrap=TRUE) {
 .calc.matches <- function(origin, current, nc=ncol(origin)) {
  one <- tcrossprod(origin, current) # both 1
  zero <- tcrossprod(1 - origin, 1 - current) # both 0
  return(rowSums(one + zero == nc)) # calc matches
 }
 fun <- match.fun(FUN)
 hcl <- fun(data)
 origin <- Hcl2mat(hcl)
 if (!bootstrap) iter <- ncol(data) # jacknife
 v <- parallel::mclapply(seq_len(iter), function(y, origin, size, fun, nco) {
  if(!bootstrap) {
   current <- Hcl2mat(fun(data[, -y])) # jacknife
  } else {
   current <- Hcl2mat(fun(data[, sample.int(ncol(data), replace=TRUE)]))
  }
  if (monitor) cat(".")
  return(list(matches=.calc.matches(origin, current, nco), current=current))
  },
 mc.cores=mc.cores, origin=origin, size=ncol(data), fun=fun, nco=ncol(origin))
 if (monitor) cat("\n")
 values <- colSums(do.call(rbind, lapply(v, `[[`, "matches")))/iter
 con <- t(Reduce("+", lapply(v, `[[`, "current")))/iter
 row.names(con) <- row.names(data)
 return(list(values=values, hclust=hcl, consensus=con))
}

## ===

Hcl2mat <- function(hcl) {
 nr <- as.integer(nrow(hcl$merge))
 m <- matrix(0L, nrow=nr, ncol=nr+1L)
 for (i in seq_len(nr)) {
  left <- hcl$merge[i, 1L]
  if (left < 0L) {
  m[i, -left] <- 1L # negative values are observations
  } else {
  m[i, ] <- m[left, ] # positive values are child clusters
  }
 right <- hcl$merge[i, 2L]
 if (right < 0L) {
  m[i, -right] <- 1L
  } else {
  m[i, ] <- m[i,] | m[right, ]
  }
 }
return(m)
}

## ===

Hcoords <- function(hcl) {
 nr <- as.integer(nrow(hcl$merge))
 p <- matrix(c(rep(0L, nr), hcl$height), nrow=nr, ncol=2, byrow=FALSE,
  dimnames=list(c(), c("x", "y")))
 o <- order(hcl$order)
 tmp <- double(2)
 for (i in seq_len(nr)) {
  left <- hcl$merge[i, 1L]
  if (left < 0L) {
   tmp[1L] <- o[-left] # negative values are observations
  } else {
  tmp[1L] <- p[left, 1L] # positive values are child clusters
 }
 right <- hcl$merge[i, 2L]
 if (right < 0L) {
  tmp[2L] <- o[-right]
  } else {
  tmp[2L] <- p[right, 1L]
  }
 p[i, 1L] <- mean(tmp)
 }
 return(p)
}

## ===

Tcoords <- function(hcl, hang=0.1, add=0, horiz=FALSE) {
 yh <- numeric(length(hcl$labels))
 for(i in seq_len(nrow(hcl$merge))){
  sngls <- hcl$merge[i, ] < 0 # negative values are observations
  yi <- -hcl$merge[i, sngls]
  yh[yi] <- hcl$height[i]
 }
 x <- seq_len(nrow(hcl$merge) + 1L) # like in plot.hclust()
 if (hang >= 0) {
  y <- yh[hcl$order] - (diff(range(hcl$height)) * (hang + add))
  } else {
  y <- 0 - (diff(range(hcl$height)) * add)
  }
 if(!horiz) cbind(x, y) else cbind(x=y, y=x)
}

## ===

Fence <- function(hcl, fct, ex=0.05, lwd=2.5, horiz=FALSE, hang=0.1, ...) {
 pos <- Tcoords(hcl, hang=hang, horiz=horiz)
 add <- diff(range(hcl$height)) * ex
 segments(pos[, 1], pos[, 2], y1=pos[, 2] + add, col=as.factor(fct)[hcl$order], lwd=lwd, ...)
}

## ===

Bclabels <- function(hcl, values, coords=NULL, horiz=FALSE, method="text",
 threshold=NULL, top=NULL, percent=FALSE, ...) {
if (is.null(coords)) coords <- Hcoords(hcl)
if (horiz) coords[, 2:1] <- coords
if (method == "text") {
 if (percent) values <- round(values*100)
 if (!is.null(threshold)) values[values < threshold] <- NA
 if (percent) values[!is.na(values)] <- paste0(values[!is.na(values)], "%")
 if (!is.null(top)) values[1:(length(values) - top)] <- NA
 text(coords, labels=values, ...)
 }
if (method == "points") {
 if (!is.null(threshold)) coords <- coords[values >= threshold, ]
 if (!is.null(top)) coords <- coords[1:(length(values) - top), ]
 points(coords, ...)
 }
invisible(list(coords=coords, labels=values))
}
