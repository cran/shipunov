Misclass <- function (pred, obs, best=FALSE, ignore=NULL, quiet=FALSE, ...)
{
if (!is.null(ignore)) {
 pred[as.character(pred) %in% as.character(ignore)] <- NA
 obs[as.character(obs) %in% as.character(ignore)] <- NA
}
predl <- levels(factor(pred))
obsl <- levels(factor(obs))
predn <- length(predl)
obsn <- length(obsl)
if (predn < 2 || obsn < 2) stop("Both 'pred' and 'obs' must have > 2 classes")
if (best) {
 if (obsn >= predn) {
  ## scales badly!
  all <- expand.grid(rep(list(obsl), obsn))
  alln <- t(all[apply(all, 1, anyDuplicated) == 0, ])
  alll <- vector("list", length=ncol(alln))
  for (x in 1:ncol(alln)) {
   obs <- factor(obs, levels=as.character(alln[, x]))
   tbl <- table(pred, obs, ...)
   dif <- nrow(tbl) - ncol(tbl)
   if (dif < 0) tbl <- rbind(tbl, matrix(0, nrow=-dif, ncol=ncol(tbl)))
   sum <- colSums(tbl)
   dia <- diag(tbl)
   alll[[x]]$tbl <- tbl
   alll[[x]]$msc <- 100 * (sum - dia)/sum
   alll[[x]]$m.m <- mean(alll[[x]]$msc)
   }
  } else {
  ## scales badly!
  all <- expand.grid(rep(list(predl), predn))
  alln <- t(all[apply(all, 1, anyDuplicated) == 0, ])
  alll <- vector("list", length=ncol(alln))
  for (x in 1:ncol(alln)) {
   pred <- factor(pred, levels=as.character(alln[, x]))
   tbl <- table(pred, obs, ...)
   dif <- nrow(tbl) - ncol(tbl)
   tbl <- t(cbind(tbl, matrix(0, ncol=dif, nrow=nrow(tbl))))
   sum <- colSums(tbl)
   dia <- diag(tbl)
   alll[[x]]$tbl <- t(tbl)
   alll[[x]]$msc <- 100 * (sum - dia)/sum
   alll[[x]]$m.m <- mean(alll[[x]]$msc)
   }
  }
 allmin <- which.min(sapply(alll, `[[`, "m.m"))
 tbl <- alll[[allmin]]$tbl
 msc <- alll[[allmin]]$msc
 m.m <- alll[[allmin]]$m.m
} else {
 tbl <- table(pred, obs, ...)
 dif <- nrow(tbl) - ncol(tbl)
 if (dif !=0) stop("When 'best=FALSE' (default), numbers of classes must be equal")
 sum <- colSums(tbl)
 dia <- diag(tbl)
 msc <- 100 * (sum - dia)/sum 
 m.m <- mean(msc)
}
if (!quiet) {
if (best) cat("Best c") else cat("C")
cat("lassification table:\n")
print(tbl)
cat("Misclassification errors (%):\n")
print(round(msc, 1))
cat("Mean misclassification error: ", round(m.m, 1), "%\n", sep="")
if (sum(is.na(c(pred, obs))) > 0) cat("Note: data contains NAs\n")
}
invisible(tbl)
}
