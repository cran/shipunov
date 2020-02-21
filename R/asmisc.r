## Miscellaneous functions for R

Rresults <- function() {
 if (Sys.info()[["sysname"]] == "Windows") {
  cat("Windows users: first, install all required software\n",
  "(bash, tee, UNIX date and mv, and optionally pdftk)\n",
  "somewhere in your PATH. Second, copy there 'Rresults' script from\n",
  paste0(system.file("bin", "Rresults", package="shipunov"), ".\n"))
  } else {
  cat("Linux and macOS users: to run 'Rresults' script installed in\n",
  paste0(system.file("bin", "Rresults", package="shipunov"), ",\n"),
  "link it from anywhere in your PATH.\n")
 }
}

# ===

Alldups <- function(v, groups=FALSE)
{
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

# ===

Toclip <- function(x, sep="\t", row.names=FALSE, col.names=TRUE, ...) {
if (Sys.info()[["sysname"]] == "Linux") {
 con <- pipe("xclip -selection clipboard -i", open="w")
 write.table(x, con, sep=sep, row.names=row.names, col.names=col.names, ...)
 close(con)
}
}

## ===

Xpager <- function(pager="xterm")
{
if (Sys.info()[["sysname"]] == "Linux") {
if (pager == "old") { options(old.pager) }
old.pager <- NULL
if (is.null(old.pager)) { old.pager <<- options("pager") }
if (pager == "xterm") { options(pager=function(file, header, title, delete.file) system(paste("xterm", "-fa 'Monospace' -fs 10.5 -e less", file, "&"))) }
if (pager == "mate") { options(pager=function(file, header, title, delete.file) system(paste("mate-terminal", "--sm-client-disable --disable-factory -x less", file, "&"))) }
}
}

## ===

Str <- function(df)
{
 if (is.data.frame(df) & sum(sapply(df, is.atomic)) == length(df)) {
 str.tmp <- sub("^ \\$ ", "", capture.output(str(df, list.len=ncol(df))))
 nums <- prettyNum(0:length(df), width=2)
 nums[1] <- ""
 nas <- c(0, sapply(df, function(.x) sum(is.na(.x))))
 str.tmp <- ifelse(nas > 0, sub(": ", "* ", str.tmp), str.tmp)
 cat(paste(nums, str.tmp, "\n", sep=" "))
 if (!identical(as.character(1:nrow(df)), row.names(df))) {
  rown.tmp <- capture.output(str(row.names(df), vec.len=5))
  rown.tmp <- sub("chr", "row.names", rown.tmp)
  cat(rown.tmp, "\n")
  }
 } else {
 str(df)
 }
}

## ===

Ls <- function(pos=1, pattern, mode="any", type="any", exclude="function", sort="name")
{
Name <- ls(pos=pos, envir=as.environment(pos), pattern=pattern)
Mode <- rep("", length(Name))
Type <- rep("", length(Name))
Vars <- rep("-", length(Name))
Obs <- rep("-", length(Name))
Size <- rep("-", length(Name))
OSize <- rep(0, length(Name))
for (i in 1:length(Name))
 {
 Mode[[i]] <- mode(get(Name[[i]]))
 Size[[i]] <- capture.output(print(object.size(get(Name[[i]])), units="auto"))
 OSize[[i]] <- object.size(get(Name[[i]]))
 if(is.list(get(Name[[i]])))
 {
 if((is.null(class(get(Name[[i]]))) | is.null(attributes(get(Name[[i]]))$class)))
 {
 Type[[i]] <- c("unknown")
 } else {
 Object.Attrib <- attributes(get(Name[[i]]))
 Type[[i]] <- Object.Attrib$class
 if(Type[[i]]=="data.frame")
 {
 Vars[[i]] <- as.character(length(Object.Attrib$names))
 Obs[[i]] <- as.character(length(Object.Attrib$row.names))
 }
 }
 }
 if(is.matrix(get(Name[[i]])))
 {
 Object.Attrib <- dim(get(Name[[i]]))
 Type[[i]] <- c("matrix")
 Vars[[i]] <- as.character(Object.Attrib[2])
 Obs[[i]] <- as.character(Object.Attrib[1])
 }
 if(is.vector(get(Name[[i]])) && (Mode[[i]]=="character" || Mode[[i]]=="numeric"))
 {
 Type[[i]] <- c("vector")
 Vars[[i]] <- c("1")
 Obs[[i]] <- as.character(length(get(Name[[i]])))
 }
 if(is.factor(get(Name[[i]])))
 {
 Type[[i]] <- c("factor")
 Vars[[i]] <- c("1")
 Obs[[i]] <- as.character(length(get(Name[[i]])))
 }
 if(is.function(get(Name[[i]]))) Type[[i]] <- c("function")
 }
res <- data.frame(Name, Mode, Type, Obs, Vars, Size)
if(sort == "size") res <- res[rev(order(OSize)), ]
if(mode != "any") res <- res[res[["Mode"]] == mode, ]
if(type != "any") res <- res[res[["Type"]] == type, ]
if(exclude != "none") res <- res[res[["Type"]] != exclude, ]
row.names(res) <- NULL
return(res)
}

## ===

Table2df <- function(table)
{
 F <- array(table, dim(table), dimnames(table))
 as.data.frame(F)
}

## ===

Cdate <- function() gsub("-", "", Sys.Date())
Ctime <- function() format(Sys.time(), "%Y%m%d_%H%M%S")
Save.history <- function() { name <- paste0(Ctime(), ".r"); savehistory(name); cat("Created", paste0("\"", name, "\""), "history file\n") }

## ===

Files <- function(root=getwd(), # root directory to explore (default is current working directory)
multiple=FALSE, # allows multiple files to be selected
hidden=FALSE) # converts into listfiles(all.files=TRUE)
{
 x <- c(dirname(normalizePath(root)), list.files(root, full.names=TRUE, all.files=hidden))
 isdir <- file.info(x)$isdir
 obj <- sort(isdir, index.return=TRUE, decreasing=TRUE)
 isdir <- obj$x
 x <- x[obj$ix]
 lbls <- sprintf('%s%s', basename(x), ifelse(isdir,'/',''))
 lbls[1] <- sprintf('../ (%s)', basename(x[1]))
 lbls <- append(lbls, 'Enter new name...')
 files <- c()
 sel <- -1
 while (TRUE)
 {
 sel <- menu(lbls, title=sprintf('Select file(s) (0 to quit with dirname)\nCurrent folder: %s', root))
 if (sel == 0)
 {
 files <- root
 break
 }
 if (sel == length(lbls))
 {
 files <- paste0(root, "/", readline('File name: '))
 break
 }
 if (isdir[sel]) # directory, browse further
 {
 files <- c(files, Files(x[sel], multiple))
 break
 } else {
 files <- c(files, x[sel]) # file, add to list
 if (!multiple) break
 lbls <- lbls[-sel] # remove selected file from choices
 x <- x[-sel]
 isdir <- isdir[-sel]
 }
 }
 return(files)
}

# ===

Read.tri.nts <- function(file, ...)
{
 elements <- scan(file, ...)
 n <- (sqrt(1 + 8 * length(elements)) - 1)/2
 U <- matrix(0, n, n)
 U[outer(1:n, 1:n, '<=')] <- elements
 return(t(U))
}

# ===

Tobin <- function(var, convert.names=TRUE)
{
 u.var <- sort(unique(var))
 if (convert.names)
 {
 mat.var <- matrix((rep(var, length(u.var)) == rep(u.var, each=length(var)))*1, ncol=length(u.var))
 colnames(mat.var) <- paste(deparse(substitute(var)), u.var, sep=".")
 } else {
 mat.var <- sapply(levels(factor(var)), function(.x) {d <- rep(0, length(var)); d[var==.x] <- 1; d})
 }
 return(mat.var)
}

## ===

Peaks <- function(series, span=3, do.pad=TRUE) {
 if((span <- as.integer(span)) %% 2 != 1) stop("'span' must be odd")
 s1 <- 1:1 + (s <- span %/% 2)
 if(span == 1) return(rep.int(TRUE, length(series)))
 z <- embed(series, span)
 v <- apply(z[,s1] > z[, -s1, drop=FALSE], 1, all)
 if(do.pad) {
  pad <- rep.int(FALSE, s)
  c(pad, v, pad)
 } else v
}

## ===

Aggregate1 <- function(df, by, ...) {
if (!is.atomic(by)) stop("'by' should be atomic")
tmp.ag <- aggregate(df, list(by), ...)
row.names(tmp.ag) <- tmp.ag[, 1]
tmp.ag[, 1] <- NULL
tmp.ag
}

## ===

Normality <- function(x, p=.05)
{
 if (length(x) < 25) warning("Normality tests do not work well on small samples (< 25)")
 ifelse(shapiro.test(x)$p.value >= p, "NORMAL", "NOT NORMAL")
}

## ===

CVs <- function(sample, na.rm=TRUE)
{
if(na.rm) sample <- na.omit(sample)
cv <- 100 * sd(sample)/mean(sample)
cvcorrected <- (1 + 1/(4*length(sample))) * cv
iqrv <- 100 * IQR(sample)/(IQR(sample) + median(sample))
madv <- 100 * mad(sample)/(mad(sample) + median(sample))
##
values <- c(cv, cvcorrected, iqrv, madv)
names(values) <- c("CV, %", "CV.corr, %", "IQR.V, %", "MAD.V, %")
values
}

## ===

Mag <- function(x, squared=TRUE)
{
magnitudev <- c(0.1, 0.3, 0.5, 0.7)
magnitudes <- c("negligible", "low", "medium", "high", "very high")
magnitudes[findInterval(ifelse(squared, sqrt(abs(x)), abs(x)), magnitudev) + 1]
}

## ===

pairwise.Eff <- function(vec, fac, eff="K", dec=2, mad=FALSE) {
lst <- split(vec, fac)
if (eff == "K") {
 ee <- sapply(lst, function(.x) sapply(lst, function(.y) K(.x, .y, mad=mad)))
 mm <- sapply(lst, function(.x) sapply(lst, function(.y) summary(K(.x, .y))[[2]]))
 }
if (eff == "cohen.d") {
 ee <- sapply(lst, function(.x) sapply(lst, function(.y) effsize::cohen.d(.x, .y)$estimate))
 mm <- sapply(lst, function(.x) sapply(lst, function(.y) effsize::cohen.d(.x, .y)$magnitude))
 }
if (eff == "cliff.delta") {
 ee <- sapply(lst, function(.x) sapply(lst, function(.y) effsize::cohen.d(.x, .y)$estimate))
 mm <- sapply(lst, function(.x) sapply(lst, function(.y) effsize::cohen.d(.x, .y)$magnitude))
 }
rr <- paste(round(ee, dec), " (", mm, ")", sep="")
attributes(rr) <- attributes(mm)
rr[upper.tri(rr, diag=TRUE)] <- ""
noquote(rr)
}

## ===

Rro.test <- function(x1, y1)
{
x1 <- x1[!is.na(x1)]
y1 <- y1[!is.na(y1)]
nx <- length(x1)
ny <- length(y1)
ux1 <- numeric(nx)
uy1 <- numeric(ny)
for (i1 in 1:nx)
 {
 for (i2 in 1:ny)
 {
 ux1[i1] <- ux1[i1] + 0.5 * sign(x1[i1] - y1[i2]) + 0.5
 uy1[i2] <- uy1[i2] + 0.5 * sign(y1[i2] - x1[i1]) + 0.5
 }
 }
mux1 <- mean(ux1)
muy1 <- mean(uy1)
sux1 <- sum(ux1)
suy1 <- sum(uy1)
dux1 <- ux1 - mux1
duy1 <- uy1 - muy1
Vux1 <- sum(dux1^2)
Vuy1 <- sum(duy1^2)
ufp <- (sux1 - suy1)/2/sqrt(Vux1 + Vuy1 + mux1 * muy1)
p <- (1-pnorm(abs(ufp))) * 2
return(c("z"=ufp, "p.value"=p))
}

## ===

pairwise.Rro.test <- function(x, g, p.adjust.method="BH")
{
p.adjust.method <- match.arg(p.adjust.method)
DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
g <- factor(g)
METHOD <- "Robust rank order test"
compare.levels <- function(i, j)
 {
 xi <- x[as.integer(g) == i]
 xj <- x[as.integer(g) == j]
 Rro.test(xi, xj)["p.value"]
 }
PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method)
ans <- list(method=METHOD, data.name=DNAME, p.value=PVAL, p.adjust.method=p.adjust.method)
class(ans) <- "pairwise.htest"
ans
}

## ===

pairwise.Table2.test <- function(tbl, names=rownames(tbl), p.adjust.method="BH", exact=FALSE, ...)
{
if(length(dim(tbl)) > 2) stop("Only tables with 2 dimensions accepted")
compare.levels <- function(i, j) chisq.test(matrix(c(tbl[i,], tbl[j,]), nrow=2, byrow=TRUE), ...)$p.value
if (exact) compare.levels <- function(i, j) fisher.test(matrix(c(tbl[i,], tbl[j,]), nrow=2, byrow=TRUE), ...)$p.value
PVAL <- pairwise.table(compare.levels, level.names=names, p.adjust.method)
ifelse(exact, method <- "Fisher's Exact Test", method <- "Pearson's Chi-squared test")
ans <- list(method=method, data.name=deparse(substitute(tbl)), p.value=PVAL, p.adjust.method=p.adjust.method)
class(ans) <- "pairwise.htest"
return(ans)
}

## ===

Fibonacci <- function(x)
{
 if (x < 0 | x%%1 != 0) stop("Only whole non-negative numbers expected")
 num <- numeric(x)
 num[1:2] <- 1
 if (x > 2) for(i in 3:x) num[i] <- num[i-1] + num[i-2]
 if (x == 0) 0 else num[x]
}

Phyllotaxis <- function(n, angle=FALSE)
{
 numerator <- Fibonacci(n)
 denominator <- Fibonacci(n+2)
 if (!angle) paste(numerator, denominator, sep="/") else 180*numerator/denominator
}

## ===

PlotBest.dist <- function(data, distances=c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"), dim=2, plot=TRUE) {
if (any(data < 0)) distances <- setdiff(distances, "canberra") # because canberra wants positive values
if (!any(data == 0)) distances <- setdiff(distances, "binary") # because binary wants zero and non-zero
res <-  structure(numeric(length(distances)), names=distances)
for (i in 1:length(distances)) {
ddc <- cor(cmdscale(dist(data, method=distances[i]), k=dim), prcomp(data)$x[, 1:dim])
res[i] <- mean(apply(abs(ddc), 2, max)) # chooses the best correlation because axes are frequently swapped
}
if (plot) Dotchart(sort(res))
invisible(res)
}

# ===

PlotBest.mdist <- function(data, distances=c("manhattan", "euclidean", "canberra", "clark", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "binomial", "chao", "cao", "mahalanobis", "cor.pearson", "cor.spearman", "cor.kendall", "gower_dist", "daisy.gower", "smirnov"), dim=2, binary.only=FALSE, plot=TRUE, ...) {
notveg <- c( "gower_dist", "cor.pearson", "cor.spearman", "cor.kendall", "daisy.gower", "smirnov")
if (binary.only) distances <- setdiff(distances, c("morisita", "cor.pearson"))
if (any(data < 0, na.rm=TRUE)) distances <- setdiff(distances, "canberra") # canberra wants positive values only
if (any(!is.integer(data), na.rm=TRUE)) distances <- setdiff(distances, c("cao", "chao", "morisita")) # they want integer mode data
if (any(!data %in% c(0, 1))) distances <- setdiff(distances, c("smirnov")) # wants 0/1 occurrence only
if (ncol(data) > nrow(data)) distances <- setdiff(distances, c("mahalanobis")) # mahalanobis transformation fails if there are too many cols
res <-  structure(numeric(length(distances)), names=distances)
for (i in 1:length(distances)) {
mdist <- distances[i]
cat(mdist, "\n")
if (!binary.only && !mdist %in% notveg) ddist <- vegan::vegdist(data, method=mdist, ...)
if (binary.only && !mdist %in% notveg) ddist <- vegan::vegdist(data, method=mdist, binary=TRUE, ...)
if (grepl("^cor\\.", mdist)) ddist <- as.dist(1 - abs(cor(t(data), method=sub("^cor\\.", "", mdist), use="pairwise.complete.obs")))
if (mdist == "gower_dist") ddist <- as.dist(Gower.dist(data))
if (mdist == "daisy.gower") ddist <- cluster::daisy(data, metric="gower")
if (mdist == "smirnov") ddist <- as.dist(1 - smirnov::smirnov(data))
ddist[is.na(ddist)] <- 0 # cmdscale does not work with NA
ddc <- cor(cmdscale(ddist, k=dim), prcomp(data)$x[, 1:dim])
res[i] <- mean(apply(abs(ddc), 2, max))
}
if (plot) Dotchart(sort(res))
invisible(res)
}

## ===

Co.test <- function(hclust, dist, method="spearman") {
cor.test(cophenetic(hclust), dist, method=method)
}

## ===

PlotBest.hclust <- function(dist, clust=c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"), plot=TRUE){
res <-  structure(numeric(length(clust)), names=clust)
for(i in 1:length(clust)) res[i] <- suppressWarnings(Co.test(hclust(dist, method=clust[i]), dist)$estimate)
if (plot) Dotchart(sort(res))
invisible(res)
}

## ===

BootKNN <- function(data, classes, sub="none", nsam=4, nboot=1000, misclass=TRUE) {
PRED <- matrix(character(0), nrow=nrow(data), ncol=nboot)
TBL <- table(results=classes, observed=classes)
TBL[TBL > 0] <- 0
for(b in 1:nboot) {
cat(".")
if (length(sub) == 1 && sub == "none") sub <- !logical(nrow(data))
data.sub <- data[sub, ]
classes.sub <- classes[sub]
sel <- ave(1:nrow(data.sub), classes.sub, FUN=function(.x) sample.int(length(.x))) <= nsam
train <- data.sub[sel, ]
classes.train <- classes.sub[sel]
res <- class::knn1(train, data, classes.train)
if (misclass) TBL <- TBL + table(res, classes)
PRED[, b] <- as.character(res)
}
cat("\n")
if (misclass){
cat("\n")
TBLb <- round(TBL/1000)
sum <- colSums(TBLb)
dia <- diag(TBLb)
msc <- (sum - dia)/sum * 100
m.m <- mean(msc)
cat("Classification table:", "\n")
print(TBLb)
cat("Misclassification errors:", "\n")
print(round(msc, 1))
cat("Mean misclassification error: ", round(m.m, 1), "%", "\n", sep="")
}
invisible(PRED)
}

## ===

BootA <- function(dat, FUN=function(.x) ape::nj(dist(.x)), iter=1000, mc.cores=1, tresh=50, cons=TRUE, prop=0.5) {
tree <- FUN(dat)
boots <- ape::boot.phylo(tree, dat, FUN, B=iter, trees=TRUE, mc.cores=mc.cores)
pclad <- round((boots$BP/iter)*100)
tree$node.label <- ifelse(pclad >= tresh, pclad, "")
result <- list(boot.tree=NA, cons.tree=NA)
result$boot.tree <- tree
if(cons) result$cons.tree <- ape::consensus(boots$trees, p=prop)
invisible(result)
}

## ===

Hclust.match <- function(hc1, hc2, scale=FALSE) {
obj <- hc1$labels
if (!all(obj == hc2$labels)) stop("Labels are not identical")
nobj <- length(obj)
res <- matrix(0, ncol=nobj, nrow=nobj, dimnames=list(obj, obj))
for (n in 2:(nobj - 1)) {
 hc1.c <- cutree(hc1, n)
 hc2.c <- cutree(hc2, n)
 hc1.o <- outer(hc1.c, hc1.c, "==")
 hc2.o <- outer(hc2.c, hc2.c, "==")
 res <- res + hc1.o + hc2.o
 }
if (scale) res <- res / (length(2:(nobj - 1)) * 2)
res
}

## ===

MDSv <- function(scores)
{
scores <- as.matrix(scores)
res <- numeric(length=ncol(scores))
scoresdist <- dist(scores)
for (i in 1:ncol(scores)) res[i] <- summary(lm(dist(scores[,i]) ~ scoresdist))$adj.r.squared
100*res/sum(res)
}

## ===

Missing.map <- function(df)
{
nas <- as.data.frame(lapply(df, function(.x) as.numeric(is.na(.x))))
total <- colSums(nas)
percent <- round(100*total/nrow(nas), 1)
## alternative: findInterval(1:nrow(df), pretty(1:nrow(df), 50))
groups <- rep(1:50, each=ceiling(nrow(df)/50))[1:nrow(df)]
nas.agg <- t(round(aggregate(nas, list(groups), mean),1)[,-1])
nas.agg <- ifelse(nas.agg == 1, "!", nas.agg)
nas.agg <- ifelse(nas.agg == "0", "_", nas.agg)
nas.agg <- ifelse(nas.agg != "_" & nas.agg != "!", ":", nas.agg)
nas.agg.str <- apply(nas.agg, 1, function(.x) paste(.x, collapse=""))
##
M <- data.frame(var=names(df), missing.map=nas.agg.str, total=total, percent=percent)
row.names(M) <- NULL
cat(paste("\n", "Legend:", "'_' no,", "':' some,", "'!' all", "\n", collapse=" "))
return(M)
}

## ===

Histr <- function(x, overlay="normal", rug=FALSE, col="gray80", ...) {
stopifnot(is.numeric(x) & is.vector(x))
stopifnot(overlay == "normal" | overlay == "density")
if (any(is.na(x)))
 {
 warning(paste(sum(is.na(x)), "missing values"))
 x <- na.omit(x)
 }
h <- hist(x, plot=FALSE, ...)
if (overlay=="normal")
 {
 xfit <- seq(min(x), max(x), length=40)
 yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
 yfit <- yfit * diff(h$mids[1:2]) * length(x)
 hist(x, ylim=c(0, max(yfit, h$counts)), col=col, ...)
 lines(xfit, yfit, col="blue", lwd=2)
 } else {
 if (overlay=="density")
 {
 hist(x, probability=TRUE, ylim=c(0, max(density(x)$y, h$density)), col=col, ...)
 lines(density(x), col = "red", lwd=2)
 }
 }
if (rug) rug(x)
}

## ===

Cladd <- function(model, data, level=.95, lty=2, ab.lty=0, col="black", ab.col="black")
{
if (class(model) != "lm") stop("Not an object of class 'lm'")
var <- names(model$model)[2]
sel <- data[, var]
new.var <- seq(min(sel, na.rm=TRUE), max(sel, na.rm=TRUE), length.out=length(sel))
new <- data.frame(new.var)
names(new)[1] <- var
pp <- predict(model, interval="confidence", level=level, newdata=new)
matlines(new.var, pp, lty=c(ab.lty,lty,lty), col=c(ab.col,col,col))
}

## ===

Boxplots <- function(vars, groups, boxcols=Pastels, legpos="topleft", srt=45, adj=1, slty=3, yticks=FALSE, ymarks=FALSE, ...)
{
 Pastels <- c("white", "lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk")
 if (!is.factor(groups)) stop("Grouping variable must be a factor")
 svars <- scale(vars)
 nvars <- ncol(vars)
 groups <- droplevels(groups)
 ngroups <- length(levels(groups))
 oldpar <- par(c(xaxt="n", yaxt="n"))
 xlim <- c(0, nvars+1)
 ylim <- c(min(svars, na.rm=TRUE), max(svars, na.rm=TRUE))
 if (ngroups == 1)
 {
 boxplot(svars, boxwex=.5, col=boxcols[1], ...)
 shift2 <- 0
 }
 if (ngroups == 2)
 {
 boxwex <- .3
 shift2 <- seq(-0.5, 0.5, along=1:nvars)
 boxplot(svars[groups==levels(groups)[1],], at=1:nvars-.2+shift2, boxwex=boxwex, col=boxcols[1], ylim=ylim, xlim=xlim, ...)
 boxplot(svars[groups==levels(groups)[2],], at=1:nvars+.2+shift2, boxwex=boxwex, col=boxcols[2], add=TRUE)
 shift3 <- .5 + (shift2[1:(nvars-1)] + shift2[2:nvars])/2
 segments(1:(nvars-1)+shift3, ylim[1]-.2, 1:(nvars-1)+shift3, ylim[2]+.2, lty=slty)
 }
 if (ngroups > 2)
 {
 span <- .64
 begin <- -(span/2)
 step <- span/(ngroups-1)
 shift <- begin + step*((1:ngroups)-1)
 boxwex <- .7*step
 shift2 <- seq(-0.5, 0.5, along=1:nvars)
 boxplot(svars[groups==levels(groups)[1],], at=1:nvars+shift[1]+shift2, boxwex=boxwex, col=boxcols[1], ylim=ylim, xlim=xlim, ...)
 for (i in 2:ngroups) boxplot(svars[groups==levels(groups)[i],], at=1:nvars+shift[i]+shift2, boxwex=boxwex, col=boxcols[i], add=TRUE)
 shift3 <- .5 + (shift2[1:(nvars-1)] + shift2[2:nvars])/2
 segments(1:(nvars-1)+shift3, ylim[1]-.2, 1:(nvars-1)+shift3, ylim[2]+.2, lty=slty)
 }
 par(oldpar)
 axis(side=1, at=1:nvars+shift2, labels=FALSE)
 if(yticks) axis(side=2, labels=ymarks)
 text(1:nvars+shift2, par("usr")[3]-.28, srt=srt, adj=adj, labels=colnames(vars), xpd=TRUE, cex=.9)
 legend(legpos, legend=levels(groups), fill=boxcols[1:ngroups], bg="white")
 invisible()
}

## ===

Linechart <- function(vars, groups, xticks=TRUE, xmarks=TRUE, mad=FALSE, pch=19, se.lwd=1, se.col=1, ...)
{
 if (!is.factor(groups)) stop("Grouping variable must be a factor")
 svars <- scale(vars)
 nvars <- ncol(vars)
 groups <- droplevels(groups)
 ngroups <- length(levels(groups))
 #
 if (mad)
 {
 centers <- aggregate(svars, list(groups), median, na.rm=TRUE); row.names(centers) <- centers$Group.1; centers <- as.matrix(centers[-1])
 starts <- as.matrix(aggregate(svars, list(groups), function(.x) median(.x, na.rm=TRUE)-mad(.x, na.rm=TRUE))[-1])
 ends <- as.matrix(aggregate(svars, list(groups), function(.x) median(.x, na.rm=TRUE)+mad(.x, na.rm=TRUE))[-1])
 } else {
 centers <- aggregate(svars, list(groups), function(.x) fivenum(.x)[3]); row.names(centers) <- centers$Group.1; centers <- as.matrix(centers[-1])
 starts <- as.matrix(aggregate(svars, list(groups), function(.x) fivenum(.x)[2])[-1])
 ends <- as.matrix(aggregate(svars, list(groups), function(.x) fivenum(.x)[4])[-1])
 }
 #
 oldpar <- par(xaxt="n")
 dotchart(centers, xlim=c(min(starts), max(ends)), pch=pch, ...)
 par(oldpar)
 yval <- rev(c(1:(nvars * (ngroups+2)))[c(rep(TRUE, ngroups), FALSE, FALSE)])
 yval <- unlist(lapply(split(yval, rep(1:nvars, each=ngroups)), rev))
 segments(starts, yval, ends, yval, lwd=se.lwd, col=se.col)
 if(xticks) axis(side=1, labels=xmarks)
 invisible(list(starts=starts, medians=centers, ends=ends))
}

## ===

Ploth <- function(hclust, labels=hclust[["labels"]], lab.col=1, col=1, pch.cex=1, pch="", bg=0, col.edges=FALSE, ...)
{
plot(dendrapply(as.dendrogram(hclust), function(n)
 {
 if(is.leaf(n))
 {
 at <- attributes(n)
 if (length(lab.col) > 1) lab.col <- lab.col[n]
 if (length(col) > 1) col <- col[n]
 if (length(pch.cex) > 1) pch.cex <- pch.cex[n]
 if (length(pch) > 1) pch <- pch[n]
 if (length(bg) > 1) bg <- bg[n]
 attr(n, "nodePar") <- c(at$nodePar, list(lab.col=lab.col, col=col, pch=pch, bg=bg, cex=pch.cex))
 attr(n, "label") <- labels[n]
 if (col.edges) attr(n, "edgePar") <- list(col=col)
 }
 n
 }), ...)
}

## ===

Topm <- function(X,
level=0.45, # treshold
values=0, # if > 0, ignores "level" and outputs until reaches number, if "all", outputs all values
corr=TRUE, # if FALSE, does not show magnitude
square=TRUE) # if FALSE, does not use lower triangle, some rows could be redundant
{
X.nam <- dimnames(X)[[1]]
X.col <- dimnames(X)[[2]]
X.rep.g <- rep(X.nam, length(X.col))
X.rep.e <- rep(X.col, each=length(X.nam))
X.vec <- as.vector(X)
X.df <- data.frame(Var1=X.rep.g, Var2=X.rep.e, Value=X.vec)
##
if(square) X.df <- X.df[as.vector(lower.tri(X)),]
if(values == "all") values <- nrow(X.df)
{if (!values)
 {X.df <- X.df[abs(X.df$Value) >= level, ]
 X.df <- X.df[order(-abs(X.df$Value)), ]}
else
 {X.df <- X.df[order(-abs(X.df$Value)), ]
 X.df <- X.df[1:min(values, nrow(X.df)), ]}}
X.df <- na.omit(X.df)
if (nrow(X.df) > 0) row.names(X.df) <- 1:nrow(X.df)
##
magnitudev <- c(0.1, 0.3, 0.5, 0.7)
magnitudes <- c("negligible", "low", "medium", "high", "very high")
if(corr) X.df$Magnitude <- magnitudes[findInterval(abs(X.df$Value), magnitudev) + 1]
##
return(X.df)
}

## ===

Cor <- function(X, # matrix or data frame with values
stars=TRUE, # replaces p-values with stars if it not greater than "p.level"
dec=4, # round to 4
p.level=0.05, ...)
{
 nc <- ncol(X)
 cor.mat <- matrix(0, nc, nc)
 p.mat <- matrix(0, nc, nc)
 low.mat <- lower.tri(p.mat)
 for (i in 1:nc)
 {
 for (j in 1:nc)
 {
 if(low.mat[i,j])
 {
 cor.res <- cor.test(X[,i], X[,j], ...)
 cor.mat[j,i] <- cor.mat[i,j] <- cor.res$estimate
 p.mat[j,i] <- p.mat[i,j] <- cor.res$p.value
 }
 }
 }
 cor.mat <- round(cor.mat, dec)
 p.mat <- round(p.mat, dec)
 if (stars)
 {
 p <- ifelse(p.mat <= p.level, "*", " ")
 sep <- ""
 } else {
 p <- p.mat
 sep <- "/"
 }
 result <- matrix(paste(cor.mat, p, sep=sep), ncol=nc)
 result <- gsub("NANA", "NA", result)
 dimnames(result) <- list(colnames(X), colnames(X))
 diag(result) <- "- "
 return(data.frame(result))
}
##
Cor2 <- function(X, dec=4, p.level=0.05)
{
 R <- cor(X)
 above <- row(R) < col(R)
 R[!above] <- round(R[!above], dec)
 r2 <- R[above]^2
 dfr <- nrow(X)-2
 Fstat <- r2 * dfr / (1 - r2)
 R[above] <- ifelse(1 - pf(Fstat, 1, dfr) > p.level, " ", "*")
 diag(R) <- "- "
 noquote(R)
}

# ===

Coeff.det <- function(X, ...)
{
X.cor <- cor(X, ...) # X is matrix or data frame with values
X.det <- NULL
X.dim <- dimnames(X.cor)[[2]]
for (i in 1:length(X.dim)) X.det <- c(X.det, mean(X.cor[,i]^2))
names(X.det) <- X.dim
return(X.det)
}

## ===

Cor.vec <- function(X, ...)
{
X.cor <- cor(X, ...)
X.nam <- row.names(X.cor)
X.tri <- as.vector(lower.tri(X.cor))
X.rep.g <- rep(X.nam, length(X.nam))
X.rep.e <- rep(X.nam, each=length(X.nam))
X.pas <- paste(X.rep.g, X.rep.e, sep=" & ")
X.vec <- as.vector(X.cor)[X.tri]
names(X.vec) <- X.pas[X.tri]
return(X.vec)
}

## ===

Rostova.tbl <- function(X, GROUP, ...)
{
 r.table <- NULL
 r.names <- unique(X[[GROUP]])
 for (i in r.names) r.table <- cbind(r.table, Cor.vec(subset(X, X[[GROUP]]==i)[,-GROUP], ...))
 dimnames(r.table)[[2]] <- r.names
 r.table[is.na(r.table)] <- 0
 r.table <- t(r.table)
 return(r.table)
}
