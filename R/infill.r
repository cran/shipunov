## Strictly biological

Read.fasta <- function(file) {
 fasta <- readLines(file)
 ind <- grep(">", fasta)
 s <- data.frame(ind=ind, from=ind+1, to=c((ind-1)[-1], length(fasta)))
 seqs <- rep(NA, length(ind))
 for(i in 1:length(ind))
 {
 seqs[i] <- paste(fasta[s$from[i]:s$to[i]], collapse="")
 }
 data.frame(name=gsub(">", "", fasta[ind]), sequence=seqs, stringsAsFactors=FALSE)
}

## ===

Write.fasta <- function(df, file) {
 if (ncol(df) > 2) warning("Only two first columns used!")
 write(file=file, paste(">", df[, 1], "\n", df[, 2], "\n", collapse="", sep=""))
}

## ===

Gap.code <- function(seqs)
{
bb <- gsub("[^-N]", "_", seqs)
bb <- do.call(rbind, strsplit(bb, split=""))
## remove consecutively duplicated columns
aa <- rle(apply(bb, 2, function(.x) paste(.x, collapse="")))$values
aa <- do.call(rbind, strsplit(aa, split=""))
bb <- apply(aa, 2, function(.x) paste(.x, collapse=""))
## (preallocation does not improve result)
gc <- matrix(nrow=length(bb), ncol=0)
nn <- nchar(bb[1])
for (pos in 1:(nn-2))
{
 cat(".")
 for (gap in (nn-pos-1):1)
 {
 r1 <- paste("^", "[N_-]", "{", pos, "}", "-", "{", gap, "}", "[N_-]", sep="")
 a1 <- ifelse(grepl(r1, bb, perl=T), "-", "C")
 r2 <- paste("^", "[N_-]", "{", pos-1, "}", "_", "-", "{", gap, "}", "_", sep="")
 a2 <- ifelse(grepl(r2, bb, perl=T), "A", "?")
 a1[a2 == "A"] <- "A"
 if (grepl("A", paste(a1, collapse=""))) gc <- cbind(gc, a1, deparse.level=0)
 }
}
cat("\n")
gc
return(gc)
}

# ===

MrBayes <- function(x, file = "", nst = 6, rates = "invgamma", ngammacat = 4,
    nruns = 2, ngen = 1e+06, printfreq = 100, samplefreq = 10,
    nchains = 4, savebrlens = "yes", temp = 0.2, burnin = 10,
    contype = "allcompat", run = FALSE,
    simple = TRUE, exec="mb-mpi")                                             # two new options
{
    requireNamespace("ips")
    if (!inherits(x, "DNAbin")) 
        stop("object 'x' is not of class 'DNAbin'")
    bayes <- c("\nbegin mrbayes;", paste("\tlset nst=", nst,
        " rates=", rates, " ngammacat=", ngammacat, ";", sep = ""),
        paste("\tmcmc nruns=", nruns, " ngen=", as.integer(ngen),
            " printfreq=", printfreq, " samplefreq=", samplefreq,
            " nchains=", nchains, " savebrlens=", savebrlens,
            " temp=", temp, ";", sep = ""), paste("\tsumt filename=",
            file, " burnin=", burnin, " contype=", contype,
            if(simple) { " conformat=simple;" },                              # 'simple format' allows to import node labels
            sep = ""), "end;")
    if (file == "") {
        nexus <- ips::write.nex(x, interleave = FALSE)
        nexus <- c(nexus, bayes)
        cat(bayes, sep = "\n")
    }
    else {
        nexus <- ips::write.nex(x, file = "", interleave = FALSE)
        nexus <- c(nexus, bayes)
        write(nexus, file = file)
    }
    if (run) {
        if (.Platform$OS.type == "unix") {
            system(paste(exec, file, "| tee -a", paste0(file, ".out")))        # use 'exec' and view _and_ save output (into specific files)
        }
        else {
            system(paste("mrbayes ", file, ".bayes", sep = ""))
        }
        tr <- ape::read.nexus(paste(file, ".con.tre", sep = ""))
        tr
    }
}

# ===

Is.tax.inform.char <- function(vec) sum(table(vec, useNA="no") > 1) > 1

## ===

Plot.phylocl <- function(
tree, ## phylo object
cl, ## two columns classification table
strict=TRUE, ## do not join all descendants
what="triangles", ## also possible to use "rectangles"
col.ed="black", ## default edge color
col.td="black", ## default tips color
col.etr="transparent", ## color to suppress original edges
col.ttr="transparent", ## color to suppress original tips
col.pfl="lightgrey", ## fill color for polygons
col.pbr="black", ## border color of polygons
lty.p=1, ## line type of polygon borders
lwd.p=1, ## line width of polygon borders
col.ct="black", ## color of clade labels
ct.off=0, ## text offset of clade labels
ct.fnt=1, ## text font of clade labels
longer="0%", ## percent to increase xlim to fit longer clade labels
... ## options to _plot.phylo()_
) {
## make list
cladelist <- split(cl[, 1], cl[, 2])
##
## remove monotypic
monoty <- sapply(cladelist, function(.x) length(.x) == 1)
## tell what removed
cat("Monotypic clades removed: ", unlist(cladelist[monoty]), "\n")
cladelist <- cladelist[!monoty]
##
## set default colors
colo <- rep(col.ed, dim(tree$edge)[1])
tcolo <- rep(col.td, length(tree$tip.label))
##
## null plotting to determine size of tree
pdf(file=NULL)
tmp1 <- ape::plot.phylo(tree, plot=FALSE, ...)
dev.off()
longer <- as.numeric(sub("%", "", longer))
newx <- tmp1$x.lim * (1 + longer / 100)
## then open device with proper size
tmp2 <- ape::plot.phylo(tree, plot=FALSE, x.lim=newx, ...)
##
## propagate clade labels offset, font and colors
if (length(ct.off) == 1) ct.off <- rep(ct.off, length(cladelist))
if (length(ct.fnt) == 1) ct.fnt <- rep(ct.fnt, length(cladelist))
if (length(col.ct) == 1) col.ct <- rep(col.ct, length(cladelist))
if (length(col.pfl) == 1) col.pfl <- rep(col.pfl, length(cladelist))
if (length(col.pbr) == 1) col.pbr <- rep(col.pbr, length(cladelist))
if (length(lty.p) == 1) lty.p <- rep(lty.p, length(cladelist))
if (length(lwd.p) == 1) lwd.p <- rep(lwd.p, length(cladelist))
##
## MAIN CYCLE
for (n in 1:length(cladelist)) {
##
clade <- cladelist[[n]]
tipsn <- match(clade, tree$tip.label)
mm <- ape::getMRCA(tree, clade)
## if strict=FALSE, get all descendants as tips numbers
if (!strict) {
tipsn <- which(sapply(ape::nodepath(tree, tipsn), function(.x) mm %in% .x))
clade <- tree$tip.label[tipsn]
}
##
x0 <- ape::node.depth.edgelength(tree)[mm] # x of MRCA node
y0 <- ape::node.height(tree)[mm] # y of MRCA node
y1 <- min(ape::node.height(tree)[tipsn]) # y of top tip
y2 <- max(ape::node.height(tree)[tipsn]) # y of bottom tip
xx <- max(ape::node.depth.edgelength(tree)[tipsn]) # x of tips
y3 <- (y1 + y2) / 2 # middle y, for label
##
## add polygons
if(what == "rectangles") polygon(c(x0, x0, xx, xx), c(y1, y2, y2, y1), col=col.pfl, border=col.pbr, lty=lty.p, lwd=lwd.p)
if(what == "triangles") polygon(c(x0, xx, xx), c(y0, y1, y2), col=col.pfl, border=col.pbr, lty=lty.p, lwd=lwd.p)
## add clade text labels
text(xx, y3, names(cladelist)[n], pos=4, font=ct.fnt[n], offset=ct.off[n], col=col.ct[n])
## make transparent part
colo[ape::which.edge(tree, clade)] <- col.etr
tcolo[tree$tip.label %in% clade] <- col.ttr
}
## plot partially transparent tree
oldpar <- par(new=TRUE)
ape::plot.phylo(tree, edge.color=colo, tip.color=tcolo, x.lim=newx, ...)
par(oldpar)
invisible(names(cladelist))
}

# ===

Infill <- function(x, n=10) {
x <- as.matrix(x)
x <- x[, colSums(x)!= 0]
mat <- numeric(0)
for (j in 1:n) {
 ini <- rep(0, nrow(x))
 sam <- sample(1:nrow(x), nrow(x))
 dat <- cbind(1:nrow(x), x[sam, ])
 for (i in 2:ncol(dat)){
 nums <- dat[dat[, i] > 0, 1]
 ini[nums[1]] <- ini[nums[1]] + 1}
 ini <- cumsum(ini)
 mat <- cbind(mat, ini)}
dimnames(mat)[[2]] <- 1:n
mat <- apply(mat, 1, mean)
class(mat) <- "Infill"
attr(mat, "nspecies") <- ncol(x)
attr(mat, "nperm") <- n
mat}
#
plot.Infill <- function(x, ...) {
sp <- attr(x, "nspecies")
n <- attr(x, "nperm")
plot(unclass(x), type="l", ylab="species", xlab="sites", sub=paste(n, "permutations"),  axes=FALSE, ...)
abline(h=.5*sp, lty=2, col="green")
abline(h=.75*sp, lty=2, col="blue")
abline(h=.9*sp, lty=2, col="red")
axis(1, seq(0, length(x), by=1))
axis(2, seq(0, max(x)))
box()}
##
summary.Infill <- function(object, ...) {
sp <- attr(object, "nspecies")
n <- attr(object, "nperm")
cat("One site infill:", (object[1]/sp)*100, "%", "\n")
cat("50% infill:", which(abs(object - .5*sp) == min(abs(object - .5*sp)))[1], "sites", "\n")
cat("75% infill:", which(abs(object - .75*sp) == min(abs(object - .75*sp)))[1], "sites", "\n")
cat("90% infill:", which(abs(object - .9*sp) == min(abs(object - .9*sp)))[1], "sites", "\n")
cat(sp, "species", "/", length(object), "sites", "/", n, "permutations", "\n")}

## ===

Coml <- function(df1, df2)
{
df1.sp <- (rowSums(df1) > 0) * 1
df2.sp <- (rowSums(df2) > 0) * 1
per <- sum((df1.sp > 0) * (df2.sp > 0)) / sum((((df1.sp + df2.sp) > 0) * 1))
p1 <- apply(df1 > 0, 1, function(x) round(sum(x) / ncol(df1) * 100, 2))
p2 <- apply(df2 > 0, 1, function(x) round(sum(x) / ncol(df2) * 100, 2))
ind.1 <- rev(sort(p1 - p2))
ind.2 <- rev(sort(p2 - p1))
C.list <- list(per=per, ind.1=ind.1, ind.2=ind.2)
class(C.list) <- "Coml"
invisible(C.list)
}
##
summary.Coml <- function(object, ..., n=10)
{
cat("Mean difference between two groups:", object$per, "\n")
cat("======================\n")
cat("Group I top", deparse(substitute(n)), "indicators:\n")
print(head(object$ind.1, n=n))
cat("======================\n")
cat("Group II top", deparse(substitute(n)), "indicators:\n")
print(head(object$ind.2, n=n))
}
