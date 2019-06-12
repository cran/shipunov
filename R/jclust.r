Jclust <- function(data, n.cl, iter=100, method.d="manhattan", method.c="ward.D", bootstrap=TRUE)
{
j.res <- matrix(rep(0, nrow(data)^2), ncol=nrow(data))
if (method.c == "ward.D" & sum(grep("ward.D", body(hclust))) == 0) method.c <- "ward"
if (bootstrap)
{
for (i in 1:iter)
 {
 j.sample <- sample(1:ncol(data), replace=TRUE)
 j.data <- data[, j.sample]
 j.dist <- dist(j.data, method=method.d)
 j.clust <- cutree(hclust(j.dist, method=method.c), k=n.cl)
 j.mat <- outer(j.clust, j.clust, "==")
 j.res <- j.res + j.mat
 }
} else {
iter <- ncol(data)
for (i in 1:ncol(data))
 {
 j.data <- data[, -i]
 j.dist <- dist(j.data, method=method.c)
 j.clust <- cutree(hclust(j.dist, method=method.d), k=n.cl)
 j.mat <- outer(j.clust, j.clust, "==")
 j.res <- j.res + j.mat
 }
}
j.supp <- c(rep(0, n.cl))
j.hcl <- hclust(dist(j.res, method=method.d), method=method.c)
j.group <- cutree(j.hcl, k=n.cl)
for(j in 1:n.cl)
 {
 j.which <- which(j.group == j)
 j.subset <- j.res[j.which, j.which]
 j.supp[j] <- median(as.vector(j.subset), na.rm=TRUE)/iter
 }
j.meth <- ifelse(bootstrap, "Bootstrap", "Jackknife")
j.clust <- list(meth=j.meth, mat=j.res, hcl=j.hcl, gr=j.group, supp=j.supp, iter=iter, n.cl=n.cl)
class(j.clust) <- "Jclust"
j.clust
}

## ===

print.Jclust <- function(x, ...)
{
cat("\n", x$meth, "support for", x$n.cl, "clusters,", x$iter, "iterations: \n")
cat("\n")
if (is.null(names(x$gr))) names(x$gr) <- as.character(x$gr)
clus <- aggregate(names(x$gr), list(x$gr), toString)
clus <- cbind(x$supp*100, clus)
colnames(clus) <- c("support", "cluster", "members")
print(clus[rev(order(clus$support)),], row.names=FALSE, ...)
}

## ===

plot.Jclust <- function(x, main="", xlab="", sub=NULL, rect.lty=3, rect.col=1, ...)
{
if (is.null(sub)) sub <- paste(x$meth, ", ", x$iter, " replicates", sep="")
plot(x$hcl, main=main, xlab=xlab, sub=sub, ...)
tree <- x$hcl
k <- x$n.cl
cluster <- x$gr
clusorder <- unique(cluster[tree$order]) # order of clusters
clustab <- table(cluster)[clusorder] # widths of clusters
m <- c(0, cumsum(clustab)) # position of each cluster
which <- 1L:k # clusters
for (n in seq_along(which))
{
 xleft <- m[which[n]] + .7
 ybottom <- par("usr")[3L]
 xright <- m[which[n] + 1] + .32
 xmid <- (xleft + xright)/2
 ytop <- mean(rev(tree$height)[(k - 1):k])
 rect(xleft, ybottom, xright, ytop, lty=rect.lty, border=rect.col)
 text(xmid, ybottom, labels=paste0(round(x$sup[clusorder[n]]*100, 1), "%"), pos=3)
}
}