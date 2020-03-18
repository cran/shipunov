Cosine.dist <- function(data.x, data.y=data.x) {
  if (is.data.frame(data.x)) data.x <- as.matrix(data.x)
  if (is.data.frame(data.y)) data.y <- as.matrix(data.y)
  res <- tcrossprod(data.x, data.y)
  t1 <- sqrt(apply(data.x, 1, crossprod))
  t2 <- sqrt(apply(data.y, 1, crossprod))
  as.dist(res / outer(t1, t2))
}
