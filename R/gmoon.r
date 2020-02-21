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

