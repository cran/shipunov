Alldups <- function(v, groups=FALSE)
{
 if (is.matrix(v) || is.data.frame(v)) v <- apply(v, 1, paste0, collapse="")
 alld <- duplicated(v) | duplicated(v, fromLast=TRUE)
 if (groups) {
  v[!alld] <- NA
  anaf <- as.numeric(as.factor(v))
  anaf[!alld] <- 0
  anaf
  } else {
  alld
 }
}
