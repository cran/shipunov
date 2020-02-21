Dotchart1 <- function (x, labels = NULL, groups = NULL, gdata = NULL, ann = par("ann"),
    xaxt = par("xaxt"), frame.plot = TRUE, log = "", cex = par("cex"),
    pt.cex = cex, pch = 21, gpch = 21, bg = par("bg"), color = par("fg"),
    gcolor = par("fg"), lcolor = "gray", xlim = range(x[is.finite(x)]),
    main = NULL, xlab = NULL, ylab = NULL, ...)
{
    opar <- par("mai", "mar", "cex", "yaxs")
    on.exit(par(opar))
    par(cex = cex, yaxs = "i")
    if (!is.numeric(x)) 
        stop("'x' must be a numeric vector or matrix")
    n <- length(x)
    if (is.matrix(x)) {
        if (is.null(labels)) 
            labels <- rownames(x)
        if (is.null(labels)) 
            labels <- as.character(seq_len(nrow(x)))
        labels <- rep_len(labels, n)
        if (is.null(groups)) 
            groups <- col(x, as.factor = TRUE)
        glabels <- levels(groups)
    }
    else {
        if (is.null(labels)) 
            labels <- names(x)
        glabels <- if (!is.null(groups)) 
            levels(groups)
        if (!is.vector(x)) {
            cat("convert plotting obj with as.numeric(obj)\n")             # changed!
            x <- as.numeric(x)
        }
    }
    plot.new()
    linch <- if (!is.null(labels)) 
        max(strwidth(labels, "inch"), na.rm = TRUE)
    else 0
    if (is.null(glabels)) {
        ginch <- 0
        goffset <- 0
    }
    else {
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- 0.4
    }
    yinch <- if (!is.null(ylab)) 0.4 else 0                                # inserted!
    if (!(is.null(labels) && is.null(glabels))) {
        nmai <- par("mai")
        nmai.2.new <- nmai[4L] + max(yinch + linch + goffset, ginch) + 0.1 # changed!
        if (nmai.2.new > nmai[2L]) {                                       # changed!
            nmai[2L] <- nmai.2.new                                         # changed!
        }
        par(mai = nmai)
    }
    if (is.null(groups)) {
        o <- seq_len(n)
        y <- o
        ylim <- c(0, n + 1)
    }
    else {
        o <- sort.list(as.numeric(groups), decreasing = TRUE)
        x <- x[o]
        groups <- groups[o]
        color <- rep_len(color, length(groups))[o]
        lcolor <- rep_len(lcolor, length(groups))[o]
        pch <- rep_len(pch, length(groups))[o]
        offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
        y <- seq_len(n) + 2 * offset
        ylim <- range(0, y + 2)
    }
    plot.window(xlim = xlim, ylim = ylim, log = log)
    lheight <- par("csi")
    if (!is.null(labels)) {
        linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
        loffset <- (linch + 0.1)/lheight
        labs <- labels[o]
        mtext(labs, side = 2, line = loffset, at = y, adj = 0, 
            col = color, las = 2, cex = cex, ...)
    }
    abline(h = y, lty = "dotted", col = lcolor)
    points(x, y, pch = pch, col = color, bg = bg, cex = pt.cex/cex)
    if (!is.null(groups)) {
        gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 
            2) - 1)
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
        mtext(glabels, side = 2, line = goffset, at = gpos, adj = 0, 
            col = gcolor, las = 2, cex = cex, ...)
        if (!is.null(gdata)) {
            abline(h = gpos, lty = "dotted")
            points(gdata, gpos, pch = gpch, col = gcolor, bg = bg, 
                cex = pt.cex/cex, ...)
        }
    }
    axis(1, xaxt = xaxt)
    if (frame.plot) 
        box()
    if (ann) 
        title(main = main, xlab = xlab, ylab = ylab, ...)
    invisible()
}

Dotchart <- function(...) {
 Dotchart1(lcolor="black", bg="white", pt.cex=1.2, ...)
}

Dotchart3 <- function(values, left, right, pch=21, bg="white", pt.cex=1.2, lty=1, lwd=2, gridcol="grey", ...) {
 Dotchart1(values, pch="", lcolor=0, xlim=range(c(values, left, right)), ...)
 grid(col=gridcol)
 for (i in 1:length(values)) {
  lines(x=c(left[i], right[i]), y=c(i, i), lty=lty, lwd=lwd)
  points(x=values[i], y=i, pch=pch, bg=bg, cex=pt.cex)
 }
}