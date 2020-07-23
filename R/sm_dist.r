SM.dist <- function(data) {
 if (is.data.frame(data)) data <- as.matrix(data)
 if (is.character(data)) data <- apply(data, 2, function(.x) as.integer(factor(.x)))
 d <- outer(seq_len(nrow(data)), seq_len(nrow(data)),
  Vectorize(function(i, j) sum(data[i, ] == data[j, ])/ncol(data)))
 dimnames(d) <- list(rownames(data), rownames(data))
 as.dist(1 - d)
}
