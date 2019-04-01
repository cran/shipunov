# Graphical examples, fun and special plots

Ex.lty <- Ex.lines <- function(custom="431313")
{
oldpar <- par(mar=c(0,0,0,0))
plot(1, ylim=c(0,7), xlim=c(0,.7), axes=FALSE, type="n", xlab="", ylab="")
ltypes <- c(0:6)
print(ltypes)
lnames <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
labels <- paste(ltypes, lnames, sep="  ")
labels <- c(labels, paste("custom:", custom))
for(i in 0:7) lines(c(.3,.7), c(i,i), lty=ifelse(i==0, custom, ltypes[8-i]), lwd=3)
text(rep(.1,7), 7:0, labels=labels, pos=4)
par(oldpar)
}

## ===

Ex.pch <- Ex.points <- function(extras=c("*",".","+","a"), cex=2, col="black", bg="gray", coltext="black", cextext=1.2, main="")
{
nex <- length(extras)
np <- 26 + nex
ipch <- 0:(np-1)
k <- floor(sqrt(np))
dd <- c(-1,1)/2
rx <- dd + range(ix <- ipch %/% k)
ry <- dd + range(iy <- 3 + (k-1)- ipch %% k)
pch <- as.list(ipch) # list with integers & strings
if(nex > 0) pch[26+ 1:nex] <- as.list(extras)
oldpar <- par(mar=c(0,0,0,0))
plot(rx, ry, type="n", axes=FALSE, xlab="", ylab="", main=main)
abline(v=ix, h=iy, col="lightgray", lty="dotted")
for(i in 1:np)
 {
 pc <- pch[[i]]
 ## 'col' symbols with a 'bg'-colored interior (where available):
 points(ix[i], iy[i], pch=pc, col=col, bg=bg, cex=cex)
 if(cextext > 0) text(ix[i]-.3, iy[i], pc, col=coltext, cex=cextext)
 }
par(oldpar)
}

## ===

Ex.col <- Ex.cols <- function(all=FALSE)
{
if(!all)
{
 oldpar <- par(mar=c(0,0,0,0))
 plot(1, ylim=c(0,9), xlim=c(0,.7), axes=FALSE, type="n", xlab="", ylab="")
 cols <- c("white", "black", "red", "green3", "blue", "cyan", "magenta", "yellow", "gray")
 nums <- c(0:8)
 labels <- paste(nums, cols, sep="  ")
 for(i in 0:8) rect(.3, i-.2, .7, i+.2, col=cols[9-i], border=NA)
 text(rep(.1,6), 8:0, labels=labels, pos=4)
 par(oldpar)
 invisible(cols) # just in case if default palette replaced with something else
}
else
{
color.rgb <- t(col2rgb(colors()))
color.text <- ifelse(apply(color.rgb, 1, mean) > 127, "black", "white")
color.df <- data.frame(name=colors(), red=color.rgb[, "red"], green=color.rgb[, "green"], blue=color.rgb[, "blue"], text=color.text)
color.df <- color.df[-grep("gray[1-9][1-9]|grey[1-9]", colors()),] # remove most of greys/grays
color.df <- droplevels(color.df)
n.col <- 10
n.row <- 48
op <- par(mar=rep(0,4))
plot(c(0, n.col), c(0, n.row), type="n", bty="n", ylab="", xlab="", axes=FALSE)
for(i in 1:n.col)
{
 color.count <- (i-1) * n.row
 color.mod <- length(colors()) - color.count
 y.val <- ifelse(color.mod < n.row, n.row - color.mod + 1, 1)
 color.names <- as.character(color.df[color.count + 1:n.row, "name"])
 rect(i - 1, y.val - 0.5, i, n.row:y.val + 0.5, border="black", col=color.names)
 color.text <- as.character(color.df[color.count + 1:n.row, "text"])
 text(i-0.5, n.row:y.val, labels=color.names, cex=0.5, col=color.text)
}
par(op)
}
}

# ===

Ex.font <- Ex.fonts <- function()
{
oldpar <- par(mar=c(0,0,0,0))
plot(1, ylim=c(0,5), xlim=c(0,.7), axes=FALSE, type="n", xlab="", ylab="")
types <- c("plain text", "bold face", "italic", "bold italic")
nums <- c(1:4)
text(rep(.35,6), 5-(1:4), labels=types[1:4], font=nums[1:4], cex=3)
text(rep(.1,6), 5-(1:4), labels=nums[1:4], pos=4, cex=3)
par(oldpar)
}

# ===

Ex.plots <- Ex.types <- function()
{
oldpar <- par(mfrow=c(3,3))
types <- c("p", "l", "b", "c", "o", "h", "s", "S", "n")
labels <- paste(types, c("points", "lines", "both", "lines of both", "overplotted", "hist", "steps", "other steps", "no plotting"), sep="  ")
for (n in 1:9) plot(1:3, main=labels[n], xlab="", ylab="", type=types[n])
par(oldpar)
}

# ===

Ex.margins <- function()
{
oldpar <- par(oma=rep(3, 4), bg="gray80")
plot(c(0, 1), c(0, 1), type="n", ann=FALSE, axes=FALSE)
box("outer", col="gray")
par(xpd=TRUE)
rect(-1, -1, 2, 2, col="gray90")
box("figure")
par(xpd=FALSE)
rect(-1, -1, 2, 2, col="gray80")
box("plot", lty="dashed")
text(.5, .5, "Plot Region")
mtext("Figure Region", side=3, line=2)
for (i in 1:4) mtext(paste("Outer margin", i), side=i, line=1, outer=TRUE)
par(oldpar)
}

# ===

Ex.boxplot <- function(...)
{
set.seed(1); bxp <- boxplot(rnorm(60), axes=FALSE, ...)
box()
st <- bxp$stats
out <- bxp$out
text(1.15, st[1], "Minimum*", pos=4, offset=-1)
text(1.05, mean(st[1:2]), "Lower Tail", srt=90)
text(1.25, st[2], "Lower Quartile*", pos=4, offset=-1)
text(1.35, mean(st[2:4]), "IQR*\n(InterQuartile\nRange)", srt=90)
text(.7, st[3], "Median\n(3rd Quartile)")
text(1.25, st[4], "Upper Quartile*", pos=4, offset=-1)
text(1.05, mean(st[4:5]), "Upper Tail", srt=90)
text(1.15, st[5], "Maximum*", pos=4, offset=-1)
text(1.15, out, "Outlier", pos=4, offset=-1)
legend("bottomright", pch="*", legend="adjusted")
}

## ===

Saynodynamite <- function()
{
s.means <- with(datasets::sleep, tapply(extra, group, mean, na.rm=T))
s.sds <- with(datasets::sleep, tapply(extra, group, sd, na.rm=T))
s.sds.adj <- 1.96*s.sds/(sqrt(table(datasets::sleep$group)))
barx <- barplot(s.means, ylim=c(0, max(s.means)+max(s.sds.adj)), col=grey(.9))
arrows(barx, s.means+s.sds.adj, barx, s.means, angle=90, code=1, length=.1)
lines(c(0.2,2.4), c(3.4,0.05), lwd=12, col=2, lend="square")
lines(c(0.2,2.4), c(0.05,3.4), lwd=12, col=2, lend="square")
text(1.3, 3.4, "Say \"no\" to dynamite plots!", col=2, cex=2, font=2)
}

## ===

Gridmoon <- function(Skyres=50, Nightsky=TRUE, Daysky="deepskyblue", Moon=TRUE, Moonsize=0.05, Stars=TRUE, Hillcol="black", Text=c("Once upon a time..."), Textsize=22, Textpos=c(.15, .51), Textcol="white")
{
grid::pushViewport(grid::viewport(xscale=c(0, 1), yscale=c(0.5, 1), clip=TRUE))
##
sky <- function(res=Skyres)
{
for (i in 1:res) grid::grid.rect(y=1 - (i-1)/res, just="top", gp=grid::gpar(col=grey(0.5*i/res), fill=grey(0.5*i/res)))
}
##
if (Nightsky) sky() else grid::grid.rect(gp=grid::gpar(col=Daysky, fill=Daysky))
##
moon <- function(x, y, size)
{
 angle <- seq(-90, 90, length=50)/180*pi
 x1 <- x + size*cos(angle)
 y1 <- y + size*sin(angle)
 mod <- 0.8
 x2 <- x + mod*(x1 - x)
 grid::grid.polygon(c(x1, rev(x2)), c(y1, rev(y1)), default.unit="native", gp=grid::gpar(col=NULL, fill="white"))
}
if (Moon) moon(.1, .9, Moonsize)
##
star <- function(x, y, size)
{
 x1 <- c(x, x + size*.1, x + size*.5, x + size*.1, x, x - size*.1, x - size*.5, x - size*.1) + .05
 y1 <- c(y - size, y - size*.1, y, y + size*.1, y + size*.7, y + size*.1, y, y - size*.1) + .05
 grid::grid.polygon(x1, y1, default.unit="native", gp=grid::gpar(col=NULL, fill="white"))
}
##
if (Stars)
{
star(.5, .7, .02)
star(.8, .9, .02)
star(.72, .74, .02)
star(.62, .88, .02)
grid::grid.circle(runif(20, .2, 1), runif(20, .6, 1), r=.002, default.unit="native", gp=grid::gpar(col=NULL, fill="white"))
}
##
hill <- function(height=0.1, col=Hillcol)
{
 n <- 100
 x <- seq(0, 1, length=n)
 y1 <- sin(runif(1) + x*2*pi)
 y2 <- sin(runif(1) + x*4*pi)
 y3 <- sin(runif(1) + x*8*pi)
 y <- 0.6 + height*((y1 + y2 + y3)/3)
 grid::grid.polygon(c(x, rev(x)), c(y, rep(0, n)), default.unit="native", gp=grid::gpar(col=NULL, fill=col))
}
##
hill()
##
grid::grid.text(Text, Textpos[1], Textpos[2], just="bottom", default.unit="native", gp=grid::gpar(col=Textcol, fontface="italic", fontsize=Textsize))
##
grid::popViewport()
grid::grid.rect()
}

## ===

Ell <- function(x, y, width, height=width, theta=2*pi, npoints=100, plot=TRUE, ...)
{
a <- width/2
b <- height/2
xcoord <- seq(-a, a, length=npoints)
ycoord.neg <- sqrt(b^2 * (1-(xcoord)^2 / a^2))
ycoord.pos <- -sqrt(b^2 * (1-(xcoord)^2 / a^2))
xx <- c(xcoord, xcoord[npoints:1])
yy <- c(ycoord.neg, ycoord.pos)
x.theta <- xx*cos(2*pi-theta) + yy*sin(2*pi-theta) + x
y.theta <- yy*cos(2*pi-theta) - xx*sin(2*pi-theta) + y
if(plot)
 invisible(polygon(x.theta, y.theta, ...))
else
 invisible(list(coords=data.frame(x=x.theta, y=y.theta), center=c(x, y), theta=theta))
}

# ===

R <- function(x, y, col.e="#B8BABF", col.l="#1E63B5", cex=12)
{
coeff.x <- abs(diff(par("usr")[1:2])) / 0.864
coeff.y <- abs(diff(par("usr")[3:4])) / 0.864
e1.x <- x - (0.05 * cex/12 * coeff.x)
e1.y <- y + (0.07 * cex/12 * coeff.y)
e1.w <- cex/30 * coeff.x
e1.h <- cex/40 * coeff.y
e2.x <- e1.x + (0.03 * cex/12 * coeff.x)
e2.y <- e1.y - (0.02 * cex/12 * coeff.y)
e2.w <- cex/44 * coeff.x
e2.h <- cex/60 * coeff.y
Ell(e1.x, e1.y, width=e1.w, height=e1.h, col=col.e, border=NA)
Ell(e2.x, e2.y, width=e2.w, height=e2.h, col="white", border=NA)
text(x, y, "R", font=2, cex=cex, col=col.l)
}

## ===

Draw.arrow <- function(reverse=FALSE, horizontal=FALSE, vertical=FALSE, length=0.1, ...){
 x <- locator(2)
 if(reverse) x <- lapply(x, rev)
 if(horizontal) x[[2]] <- rep(mean(x[[2]]), 2)
 if(vertical) x[[1]] <- rep(mean(x[[1]]), 2)
 arrows(x[[1]][1], x[[2]][1], x[[1]][2], x[[2]][2], length=length, ...)
 x <- matrix(unlist(x), ncol=2)
 dimnames(x) <- list(c("tail","head"), c("x","y"))
 invisible(x)
}

## ===

Miney <- function(n, ucol="black", gcol="white", bcol="red") { # 'n' indicates the size of the matrix to play, i.e. n=5 results in n x n = 5 x 5 matrix.
 create.matrix <- function(n, proportion=0.15) { # creates a matrix on which the game is played
## n: numeric scalar  size of square matrix
## proportion: numeric scalar  proportion of bombs in relation to all fields
  bombs <- sample(x=1:(n * n), size=floor(proportion * n * n), replace = FALSE)
  elements <- rep(0, n * n)
  elements[bombs] <- 1
  A <- matrix(elements, ncol=n, nrow=n, byrow=TRUE)
  return(A)
 }
 count.neighbors <- function(amatrix) { # given a matrix of zeros ('0') and ones ('1'), this function
  # counts the number of neighbors
  amatrix2 <- rbind(0, cbind(0, amatrix, 0), 0)
  resultsmatrix <- matrix(0, ncol=ncol(amatrix2), nrow=nrow(amatrix2))
  for (i in 2:(nrow(amatrix2) - 1)) {
   for (j in 2:(ncol(amatrix2) - 1)) {
    resultsmatrix[i, j] <- sum(amatrix2[(i - 1) : (i + 1), (j - 1) : (j + 1)]) - amatrix2[i, j]
   }}
  resultsmatrix <- resultsmatrix[-(c(1,nrow(resultsmatrix))), -(c(1,ncol(resultsmatrix)))]
  return(resultsmatrix)
 }
 plot.initial.matrix <- function(n, the.offset=0.05) { # function to plot the initial matrix, parameters are:
  # n       numeric scalar  size of matrix
  # the.offset   numeric scalar  determines how much white space there is between matrix elements
  plot(x=1, y=1, type="n", xlab="", ylab="", axes=FALSE, xlim=c(0, n), ylim=c(0, n))
  for (i in 1:n) {
   for (j in 1:n) {
    rect(xleft=i-1+the.offset, xright=i-the.offset, ybottom=j-1+the.offset, ytop=j-the.offset, col=ucol)
   }}
 }
 plot.known.matrix <- function(n, the.offset=0.05) { # function to plot the currently known matrix, parameters are:
  # n       numeric scalar  size of matrix
  # the.offset   numeric scalar  determines how much white space there is between matrix elements
  plot(x=1, y=1, type="n", xlab="", ylab="", axes=FALSE, xlim=c(0,n), ylim=c(0,n))
  for (i in 1:n) {
   for (j in 1:n) {
    if (known.matrix[i,j]==1) { rect(xleft=i-1+the.offset, xright=i-the.offset, ybottom=j-1+the.offset, ytop=j-the.offset, col=gcol)
     text(x=i+0.5-1, y=j+0.5-1, labels=neighbors.matrix[i,j])
    } else {
     rect(xleft=i-1+the.offset, xright=i-the.offset, ybottom=j-1+the.offset, ytop=j-the.offset, col=ucol)
    }
   }}
 }
 the.offset <- 0.05
 tick <- as.numeric(Sys.time())
 plot.initial.matrix(n=n)
 base.matrix <- create.matrix(n)
 neighbors.matrix <- count.neighbors(base.matrix)
 known.matrix <- matrix(0, ncol=n, nrow=n)
 plot.known.matrix(n=n, the.offset=the.offset)
 game.over <- FALSE
 while(!game.over) {
  current.click <- locator(1)
  current.x <- floor(current.click$x) + 1
  current.y <- floor(current.click$y) + 1
  intermediate <- known.matrix
  intermediate[current.x,current.y] <- 1
  known.matrix <- intermediate
  if (base.matrix[current.x,current.y]==1) {
   tock <- as.numeric(Sys.time())
   time.elapsed <- tock - tick
   rect(xleft=current.x-1+the.offset, xright=current.x-the.offset, ybottom=current.y-1+the.offset, ytop=current.y-the.offset, col=bcol)
   title(main="FAILURE :(\nGame over...", sub=paste("It took you", round(time.elapsed), "seconds"))
   game.over <- TRUE
  } else {
   if (all(base.matrix+known.matrix==1)) {
    tock <- as.numeric(Sys.time())
    time.elapsed <- tock - tick
    plot.known.matrix(n=n, the.offset=the.offset)
    title(main="Congratulations! You won!", sub=paste("It took you", round(time.elapsed), "seconds"))
    game.over <- TRUE
   } else {
    plot.known.matrix(n=n, the.offset=the.offset)
   }
  }
 }
}

## ===

Life <- function(n.rows=100,
n.cols=100,
n.cycles=100,
sleep.time=0.1,
cols=c("#f0f0f0", "#2f81c1"),
rnd.threshold=0.3) # rnd_threshold 0 empty board; 1 all squares are filled
{
Shift.Matrix <- function(mx, dr, dc) { # shift the matrix by dr (delta r) rows and dc columns by adding e.g. dr rows of zeros and removing dr rows from the other side
nr <- nrow(mx)
nc <- ncol(mx)
## if the matrix is shifted by more than its nrow or ncol, we get a matrix of zeros
if (abs(dr) >= nr || abs(dc) >= nc) { mx <- matrix(0, nrow = nr, ncol = nc) ; return(mx) }
## rows:
if (dr > 0) {
 mx <- rbind(mat.or.vec(dr, nc), mx)
 mx <- mx[1:nr,]
} else if (dr < 0) {
 mx <- rbind(mx, mat.or.vec(-dr, nc))
 mx <- mx[(1 - dr):(nr - dr),]
}
## columns:
if (dc > 0) {
 mx <- cbind(mat.or.vec(nr, dc), mx)
 mx <- mx[,1:nc]
} else if (dc < 0) {
 mx <- cbind(mx, mat.or.vec(nr, -dc))
 mx <- mx[,(1 - dc):(nc - dc)]
}
return(mx)
}
##
Life.Cycle <- function(mx) { # move the board one generation forward
mx0 <- matrix(0, nrow = nrow(mx), ncol = ncol(mx))
## produce 8 "shifted" boards and add them up
for (n in (-1:1)) {
 for (m in (-1:1)) {
 if (n !=0 || m !=0) mx0 <- mx0 + Shift.Matrix(mx, n, m)
 }}
## Deaths and births
mx[mx0 > 3 | mx0 < 2] <- 0
mx[mx0 == 3] <- 1
return(mx)
}
##
## Create a board and plot it
Board <- matrix(0, nrow = n.rows, ncol = n.cols)
Board[runif(n.rows * n.cols, 0, 1) < rnd.threshold] <- 1
image(Board, axes=FALSE, col=cols)
##
## The main cycle
for (i in (1:n.cycles)) {
 Sys.sleep(sleep.time)
 Board <- Life.Cycle(Board)
 image(Board, axes=FALSE, col=cols)
}
}

## ===

Gradd <- function(model2var, data2var, spacing=75, trnsp=0.3, pch=20, cex=0.2, palette=NULL, type="ids", ...)
{
User.Predict <- function(model2var, X) {} # prototype
X <- expand.grid(data.frame(apply(data2var, 2, function(.x) seq(range(.x)[1], range(.x)[2], length.out=spacing))))
if (type == "ids") newids <- predict(model2var, X) else
if (type == "lda") newids <- predict(model2var, X)$class else
if (type == "neuralnet") newids <- apply(neuralnet::compute(model2var, X)$net.result, 1, function(.x) model2var[["model.list"]][["response"]][which.max(.x)]) else
if (type == "tree") newids <- predict(model2var, X, type="class") else
if (type == "user") newids <- User.Predict(model2var, X) else stop("Unknown model type")
if (!is.factor(newids)) newids <- as.factor(newids)
if (is.null(palette))
 {
 cols <- as.numeric(newids)
 }
else
 {
 cols <- palette[as.numeric(newids)]
 }
points(X, col=adjustcolor(cols, alpha.f=trnsp), pch=pch, cex=cex, ...)
}

## ===

plot.nnet <- function(x, ..., nid=TRUE, all.out=TRUE, all.in=TRUE, wts.only=FALSE, rel.rsc=5, circle.cex=5, node.labs=TRUE, line.stag=NULL, cex.val=1, alpha.val=1, circle.col="lightgrey", pos.col="black", neg.col="grey")
{
## gets weights for nnet neural network, output is list
## if rescaled argument is true, weights are returned but rescaled based on abs value
mod.in <- x
nnet.vals <- function(mod.in, nid, rel.rsc)
{
 layers <- mod.in$n
 wts <- mod.in$wts
 if(nid) wts <- scales::rescale(abs(wts), c(1, rel.rsc))
 indices <- matrix(seq(1, layers[1]*layers[2] + layers[2]), ncol=layers[2])
 out.ls <- list()
 for(i in 1:ncol(indices)) out.ls[[paste("hidden", i)]] <- wts[indices[, i]]
 if(layers[3]==1)
 {
 out.ls[["out 1"]] <- wts[(max(indices) + 1):length(wts)]
 }
 else
 {
 out.indices <- matrix(seq(max(indices)+1, length(wts)), ncol=layers[3])
 for(i in 1:ncol(out.indices)) out.ls[[paste("out", i)]] <- wts[out.indices[, i]]
 }
 out.ls
}
wts <- nnet.vals(mod.in, nid=FALSE)
if(wts.only) return(wts)
struct <- mod.in$n
x.range <- c(0, 100)
y.range <- c(0, 100)
## these are all proportions from 0-1
if(is.null(line.stag)) line.stag <- 0.011 * circle.cex/2
layer.x <- seq(0.17, 0.9, length=3)
bias.x <- c(mean(layer.x[1:2]), mean(layer.x[2:3]))
bias.y <- 0.95
in.col <- bord.col <- circle.col
circle.cex <- circle.cex
## get variable names from nnet object
if(is.null(mod.in$call$formula))
 {
 x.names <- colnames(eval(mod.in$call$x))
 y.names <- colnames(eval(mod.in$call$y))
 }
 else
 {
 forms <- eval(mod.in$call$formula)
 dat.names <- model.frame(forms, data=eval(mod.in$call$data))
 y.names <- as.character(forms)[2]
 x.names <- names(dat.names)[!names(dat.names) %in% y.names]
 }
## initiate plot
plot(x.range, y.range, type="n", axes=F, ylab="", xlab="", ...)
## function for getting y locations for input, hidden, output layers
## input is integer value from "struct"
get.ys <- function(lyr)
 {
 spacing <- diff(c(0*diff(y.range), 0.9*diff(y.range)))/max(struct)
 seq(0.5*(diff(y.range)+spacing*(lyr-1)), 0.5*(diff(y.range)-spacing*(lyr-1)), length=lyr)
 }
## function for plotting nodes
## "layer" specifies which layer, integer from "struct"
## "x.loc" indicates x location for layer, integer from "layer.x"
## "layer.name" is string indicating text to put in node
layer.points <- function(layer, x.loc, layer.name, cex=cex.val)
 {
 xx <- rep(x.loc*diff(x.range), layer)
 yy <- get.ys(layer)
 points(xx, yy, pch=21, cex=circle.cex, col=in.col, bg=bord.col)
 if(node.labs) text(xx, yy, paste(layer.name, 1:layer, sep=""), cex=cex.val)
 if(layer.name=="I" & node.labs)
 {
 text(xx - line.stag * diff(x.range), yy, x.names, pos=2, cex=cex.val)
 }
 if(layer.name=="O" & node.labs)
 text(xx + line.stag * diff(x.range), yy, y.names, pos=4, cex=cex.val)
 }
## function for plotting bias points
## "bias.x" is vector of values for x locations
## "bias.y" is vector for y location
## "layer.name" is string indicating text to put in node
bias.points <- function(bias.x, bias.y, layer.name, cex, ...)
 {
 for(val in 1:length(bias.x))
 {
 points(diff(x.range) * bias.x[val], bias.y * diff(y.range), pch=21, col=in.col, bg=bord.col, cex=circle.cex)
 if(node.labs) text(diff(x.range) * bias.x[val], bias.y * diff(y.range), paste(layer.name, val, sep=""), cex=cex.val)
 }
 }
## function creates lines colored by direction and width as proportion of magnitude
## use "all.in" argument if you want to plot connection lines for only a single input node
layer.lines <- function(mod.in, h.layer, layer1=1, layer2=2, out.layer=F, nid, rel.rsc, all.in, pos.col, neg.col, ...)
 {
 x0 <- rep(layer.x[layer1] * diff(x.range) + line.stag * diff(x.range), struct[layer1])
 x1 <- rep(layer.x[layer2] * diff(x.range) - line.stag * diff(x.range), struct[layer1])
 if(out.layer==TRUE)
 {
 y0 <- get.ys(struct[layer1])
 y1 <- rep(get.ys(struct[layer2])[h.layer], struct[layer1])
 src.str <- paste("out", h.layer)
 wts <- nnet.vals(mod.in, nid=FALSE, rel.rsc)
 wts <- wts[grep(src.str, names(wts))][[1]][-1]
 wts.rs <- nnet.vals(mod.in, nid=TRUE, rel.rsc)
 wts.rs <- wts.rs[grep(src.str, names(wts.rs))][[1]][-1]
 cols <- rep(pos.col, struct[layer1])
 cols[wts<0] <- neg.col
 if(nid) segments(x0, y0, x1, y1, col=cols, lwd=wts.rs) else segments(x0, y0, x1, y1)
 }
 else
 {
 if(is.logical(all.in)) all.in <- h.layer else all.in <- which(x.names==all.in)
 y0 <- rep(get.ys(struct[layer1])[all.in], struct[2])
 y1 <- get.ys(struct[layer2])
 src.str <- "hidden"
 wts <- nnet.vals(mod.in, nid=FALSE, rel.rsc)
 wts <- unlist(lapply(wts[grep(src.str, names(wts))], function(.x) .x[all.in+1]))
 wts.rs <- nnet.vals(mod.in, nid=TRUE, rel.rsc)
 wts.rs <- unlist(lapply(wts.rs[grep(src.str, names(wts.rs))], function(.x) .x[all.in+1]))
 cols <- rep(pos.col, struct[layer2])
 cols[wts<0] <- neg.col
 if(nid) segments(x0, y0, x1, y1, col=cols, lwd=wts.rs) else segments(x0, y0, x1, y1)
 }
 }
bias.lines <- function(bias.x, mod.in, nid, rel.rsc, all.out, pos.col, neg.col, ...)
 {
 if(is.logical(all.out)) all.out <- 1:struct[3]
 else all.out <- which(y.names==all.out)
 for(val in 1:length(bias.x))
 {
 wts <- nnet.vals(mod.in, nid=FALSE, rel.rsc)
 wts.rs <- nnet.vals(mod.in, nid=TRUE, rel.rsc)
 if(val==1)
 {
 wts <- wts[grep("out", names(wts), invert=TRUE)]
 wts.rs <- wts.rs[grep("out", names(wts.rs), invert=TRUE)]
 }
 if(val==2)
 {
 wts <- wts[grep("out", names(wts))]
 wts.rs <- wts.rs[grep("out", names(wts.rs))]
 }
 cols <- rep(pos.col, length(wts))
 cols[unlist(lapply(wts, function(.x) .x[1])) < 0] <- neg.col
 wts.rs <- unlist(lapply(wts.rs, function(.x) .x[1]))
 if(nid==FALSE)
 {
 wts.rs <- rep(1, struct[val+1])
 cols <- rep("black", struct[val+1])
 }
 if(val==1)
 {
 segments(
 rep(diff(x.range) * bias.x[val] + diff(x.range) * line.stag, struct[val+1]),
 rep(bias.y*diff(y.range), struct[val+1]),
 rep(diff(x.range) * layer.x[val+1] - diff(x.range) * line.stag, struct[val+1]),
 get.ys(struct[val+1]),
 lwd=wts.rs,
 col=cols
 )
 }
 if(val==2)
 {
 segments(
 rep(diff(x.range) * bias.x[val] + diff(x.range) * line.stag, struct[val+1]),
 rep(bias.y * diff(y.range), struct[val+1]),
 rep(diff(x.range) * layer.x[val+1] - diff(x.range) * line.stag, struct[val+1]),
 get.ys(struct[val+1])[all.out],
 lwd=wts.rs[all.out],
 col=cols[all.out]
 )
 }
 }
 }
## use functions to plot connections between layers
## bias lines
bias.lines(bias.x, mod.in, nid=nid, rel.rsc=rel.rsc, all.out=all.out, pos.col=scales::alpha(pos.col, alpha.val), neg.col=scales::alpha(neg.col, alpha.val))
## layer lines, makes use of arguments to plot all or for individual layers
## starts with input-hidden
## uses "all.in" argument to plot connection lines for all input nodes or a single node
if(is.logical(all.in)) {
 mapply(function(.x) layer.lines(mod.in, .x, layer1=1, layer2=2, nid=nid, rel.rsc=rel.rsc, all.in=all.in, pos.col=scales::alpha(pos.col, alpha.val), neg.col=scales::alpha(neg.col, alpha.val)), 1:struct[1])
 } else {
 node.in <- which(x.names==all.in)
 layer.lines(mod.in, node.in, layer1=1, layer2=2, nid=nid, rel.rsc=rel.rsc, all.in=all.in, pos.col=scales::alpha(pos.col, alpha.val), neg.col=scales::alpha(neg.col, alpha.val))
}
## lines for hidden-output
## uses "all.out" argument to plot connection lines for all output nodes or a single node
if(is.logical(all.out)) {
 mapply(
 function(.x) layer.lines(mod.in, .x, layer1=2, layer2=3, out.layer=TRUE, nid=nid, rel.rsc=rel.rsc,
 all.in=all.in, pos.col=scales::alpha(pos.col, alpha.val), neg.col=scales::alpha(neg.col, alpha.val)),
 1:struct[3]
 )
 } else {
 all.out <- which(y.names==all.out)
 layer.lines(mod.in, all.out, layer1=2, layer2=3, out.layer=TRUE, nid=nid, rel.rsc=rel.rsc, pos.col=pos.col, neg.col=neg.col)
}
## use functions to plot nodes
layer.points(struct[1], layer.x[1], "I")
layer.points(struct[2], layer.x[2], "H")
layer.points(struct[3], layer.x[3], "O")
bias.points(bias.x, bias.y, "B")
}
