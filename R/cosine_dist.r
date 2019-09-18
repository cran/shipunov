Cosine.dist <- function(data.x, data.y=data.x) {
  res <- tcrossprod(data.x, data.y)
  t1 <- sqrt(apply(data.x, 1, crossprod))
  t2 <- sqrt(apply(data.y, 1, crossprod))
  as.dist(res / outer(t1, t2))
}
