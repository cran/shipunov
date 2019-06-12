Fill <- function(x, missing="")
{
 if (is.na(missing)) {
  what <- !is.na(x)
  } else {
  what <- x != missing
  }
 y <- x[what]
 z <- cumsum(what)
 z[z == 0] <- NA # do not skip positions before the first fill
 res <- y[z] # a[NA] returns NA but keeps position
 res[is.na(res)] <- missing # keep them as 'missing' defined
 res
}
