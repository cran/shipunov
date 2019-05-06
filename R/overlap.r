Overlap <- function(ppts, Hulls=TRUE)
{
if(Hulls) {
if("centers" %in% names(ppts)) ppts <- ppts[-which(names(ppts) == "centers")]
if("outliers" %in% names(ppts)) ppts <- ppts[-which(names(ppts) == "outliers")]
}
ppol <- ppts
len <- length(ppol)
for (i in 1:len) {
 ppol[[i]] <- data.frame(ppol[[i]], PID=i, POS=1:nrow(ppol[[i]]))
 names(ppol[[i]])[1:2] <- c("X","Y")
 }
over.m <- matrix(ncol=len, nrow=len)
for (i in 2:len) {
 for (j in 1:i) {
  p.i <- ppol[[i]]
  p.j <- ppol[[j]]
  ij <- PBSmapping::joinPolys(p.i, p.j, "INT")
  if (is.null(ij)) {
   over.m[j, i] <- over.m[i, j] <- NA
  } else {
   ij.a <- PBSmapping::calcArea(ij)$area
   over.m[j, i] <- ij.a/PBSmapping::calcArea(p.j)$area
   over.m[i, j] <- ij.a/PBSmapping::calcArea(p.i)$area
  }
 }
}
diag(over.m) <- NA
dimnames(over.m) <- list(names(ppts), names(ppts))
class(over.m) <- "Overlap"
return(over.m)
}

# ===

summary.Overlap <- function(object, ...)
{
total.overlap <- round(rowSums(object, na.rm=TRUE)*100, 2)
mean.overlap <- round(rowMeans(object, na.rm=TRUE)*100, 2)
res <- data.frame(mean.overlap, total.overlap)
overall.overlap <- round(mean(object, na.rm=TRUE)*100, 2)
cat("Overlaps for each hull, %:\n")
print(res)
cat("Mean overlap for the whole dataset", overall.overlap, "%\n")
}

## ===

BestOverlap <- function(xylabels, ci="95%", round=4)
{
LVL <- levels(factor(xylabels[[1]][, "labels"]))
RES <- vector("list", length=length(xylabels))
for (i in 1:length(xylabels)) {
 if (!identical(levels(factor(xylabels[[i]][, "labels"])), LVL)) next
 tmp <- Hulls(pts=xylabels[[i]][, c("x", "y")], groups=xylabels[[i]][, "labels"],
  plot=FALSE)
 tmpo <- Overlap(tmp)
 RES[[i]]$tmpm <- mean(tmpo, na.rm=TRUE)
 RES[[i]]$tmpo <- tmpo
}
TMPO <- lapply(RES, `[[`, "tmpo")
NUMS <- sapply(TMPO, function(.x) as.data.frame(as.table(.x))[, "Freq"])
NUMS <- rbind(NUMS, sapply(TMPO, rowMeans, na.rm=TRUE))
NUMS <- rbind(NUMS, sapply(TMPO, rowSums, na.rm=TRUE))
NUMS <- rbind(NUMS, sapply(RES, `[[`, "tmpm"))
lower <- (1 - as.numeric(sub("%", "", ci))/100) / 2
CI <- t(apply(NUMS, 1, function(.x) round(quantile(.x, c(0, lower, 0.5, 1-lower, 1),
 na.rm=TRUE), round)))
NAMES <- as.data.frame(table(Class1=LVL, Class2=LVL), stringsAsFactors=FALSE)[, 1:2]
ADD <- matrix(c(LVL, LVL, "Total:", rep("mean:", length(LVL)),
 rep("total:", length(LVL)), ""), ncol=2)
NAMES <- rbind(as.matrix(NAMES), ADD)
SUMM <- cbind(data.frame(NAMES, stringsAsFactors=FALSE), CI)
SUMM <- SUMM[!(SUMM$Class1 == SUMM$Class2), ]
row.names(SUMM) <- NULL
MIN <- which.min(sapply(RES, `[[`, "tmpm"))
BESTO <- RES[[MIN]]$tmpo
return(list(best=MIN, best.overlap=BESTO, summary=SUMM))
}

## ===

Squares <- function(ppts, relative=FALSE) {
if("centers" %in% names(ppts)) ppts <- ppts[-which(names(ppts) == "centers")]
ppol <- ppts
len <- length(ppol)
sq <- numeric(len)
for (i in 1:len)
 {
 ppol[[i]] <- data.frame(ppol[[i]], PID=i, POS=1:nrow(ppol[[i]]))
 names(ppol[[i]])[1:2] <- c("X", "Y")
 sq[i] <- PBSmapping::calcArea(ppol[[i]])$area
 }
if (relative) sq <- sq/(sum(sq))
sq
}
