MDSv <- function(scores)
{
scores <- as.matrix(scores)
res <- numeric(length=ncol(scores))
scoresdist <- dist(scores)
for (i in seq_len(ncol(scores))) res[i] <- summary(lm(dist(scores[,i]) ~ scoresdist))$adj.r.squared
100*res/sum(res)
}

