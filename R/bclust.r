Bclust <- function(data, method.d="manhattan", method.c="ward.D",
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
  m[i, -left] <- 1L # negative values correspond to observations
  } else {
  m[i, ] <- m[left, ] # positive values correspond to childcluster
  }
 right <- hcl$merge[i, 2L]
 if (right < 0L) {
  m[i, -right] <- 1L # negative values correspond to observations
  } else {
  m[i, ] <- m[i,] | m[right, ] # positive values correspond to childcluster
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
   tmp[1L] <- o[-left] # negative values correspond to observations
  } else {
  tmp[1L] <- p[left, 1L] # positive values correspond to childcluster
 }
 right <- hcl$merge[i, 2L]
 if (right < 0L) {
  tmp[2L] <- o[-right] # negative values correspond to observations
  } else {
  tmp[2L] <- p[right, 1L] # positive values correspond to childcluster
  }
 p[i, 1L] <- mean(tmp)
 }
 return(p)
}

## ===

Bclabels <- function(hcl, values, coords=NULL, horiz=FALSE, method="text", threshold=NULL, ...) {
 if (is.null(coords)) coords <- Hcoords(hcl)
 if (horiz) coords[, c(2, 1)] <- coords
 labels <- sprintf("%.2f", values)
  if (method == "text") {
  if (!is.null(threshold)) labels[values < threshold] <- NA
  text(coords, labels=labels, ...)
  }
 if (method == "points") {
  if (!is.null(threshold)) coords <- coords[values >= threshold, ]
  points(coords, ...)
  }
 invisible(list(coords=coords, labels=labels))
}
