symb.bxp <- function (z, notch = FALSE, width = NULL, varwidth = FALSE,
                      outline = TRUE, notch.frac = 0.5, border = par("fg"),
                      pars = NULL, frame.plot = FALSE, horizontal = FALSE,
                      at = NULL, xaxis = FALSE, yaxis = FALSE, ...) 
{
    pars <- c(list(...), pars)
    pars <- pars[unique(names(pars))]
    bplt <- function(x, wid, stats, out, conf, notch, xlog, i) {
        ok <- TRUE
        if (!any(is.na(stats))) {
            xP <- if (xlog) 
                function(x, w) x * exp(w)
            else function(x, w) x + w
            wid <- wid/2
            if (notch) {
                ok <- stats[2] <= conf[1] && conf[2] <= stats[4]
                xx <- xP(x, wid * c(-1, 1, 1, notch.frac, 1, 
                  1, -1, -1, -notch.frac, -1))
                yy <- c(stats[c(2, 2)], conf[1], stats[3], conf[2], 
                  stats[c(4, 4)], conf[2], stats[3], conf[1])
            }
            else {
                xx <- xP(x, wid * c(-1, 1, 1, -1))
                yy <- stats[c(2, 2, 4, 4)]
            }
            if (!notch) 
                notch.frac <- 1
            wntch <- notch.frac * wid
            xypolygon(xx, yy, gp = gpar(lty = "blank", fill = boxfill[i]), default.units = "native")
            xysegments(xP(x, -wntch), stats[3], xP(x, +wntch), 
                stats[3], gp = gpar(lty = medlty[i], lwd = medlwd[i], col = medcol[i]), 
                default.units = "native")
            xypoints(x, stats[3], pch = medpch[i], gp = gpar(cex = medcex[i], 
                col = medcol[i], fill = medbg[i]), default.units = "native", size=unit(0.3,"char"))
            xysegments(rep.int(x, 2), stats[c(1, 5)], rep.int(x, 
                2), stats[c(2, 4)], gp = gpar(lty = whisklty[i], lwd = whisklwd[i], 
                col = whiskcol[i]), default.units = "native")
            xysegments(rep.int(xP(x, -wid * staplewex[i]), 2), 
                stats[c(1, 5)], rep.int(xP(x, +wid * staplewex[i]), 
                  2), stats[c(1, 5)], gp = gpar(lty = staplelty[i], lwd = staplelwd[i], 
                col = staplecol[i]), default.units = "native")
            xypolygon(xx, yy, gp = gpar(lty = boxlty[i], lwd = boxlwd[i], 
                col = boxcol[i]), default.units = "native")
            if ((nout <- length(out)) > 0) {
                xysegments(rep(x - wid * outwex, nout), out, 
                  rep(x + wid * outwex, nout), out, gp = gpar(lty = outlty[i], 
                  lwd = outlwd[i], col = outcol[i]), default.units = "native")
                xypoints(rep.int(x, nout), out, pch = outpch[i], 
                  gp = gpar(cex = outcex[i], col = outcol[i], fill = outbg[i]), 
                  default.units = "native", size=unit(0.3,"char"))
            }
            if (any(inf <- !is.finite(out))) {
                warning(sprintf(ngettext(length(unique(out[inf])), 
                  "Outlier (%s) in boxplot %d is not drawn", 
                  "Outliers (%s) in boxplot %d are not drawn"), 
                  paste(unique(out[inf]), collapse = ", "), x), 
                  domain = NA)
            }
        }
        return(ok)
    }
    if (!is.list(z) || 0 == (n <- length(z$n))) 
        stop("invalid first argument")
    if (is.null(at)) 
        at <- 1:n
    else if (length(at) != n) 
        stop("'at' must have same length as 'z$n', i.e. ", n)
    if (is.null(z$out)) 
        z$out <- numeric()
    if (is.null(z$group) || !outline) 
        z$group <- integer()
    if (is.null(pars$ylim)) 
        ylim <- range(z$stats[is.finite(z$stats)], z$out[is.finite(z$out)], 
            if (notch) z$conf[is.finite(z$conf)])
    else {
        ylim <- pars$ylim
        pars$ylim <- NULL
    }
    if (is.null(pars$xlim)) 
        xlim <- c(0.5, n + 0.5)
    else {
        xlim <- pars$xlim
        pars$xlim <- NULL
    }
    if (length(border) == 0) 
        border <- par("fg")
        
    margin <- c(0,0,0,0)
    if ((xaxis) | (yaxis)) margin <- c(2,2,2,2)

    pushViewport(plotViewport(margin))    
    
    if (horizontal)
    {
      pushViewport(dataViewport(ylim,xlim))
    }
    else pushViewport(dataViewport(xlim,ylim))

    xlog <- (par("ylog") && horizontal) || (par("xlog") && !horizontal)
    pcycle <- function(p, def1, def2 = NULL) rep(if (length(p)) p else if (length(def1)) def1 else def2, 
        length.out = n)
    p <- function(sym) pars[[sym, exact = TRUE]]
    boxlty <- pcycle(pars$boxlty, p("lty"), par("lty"))
    boxlwd <- pcycle(pars$boxlwd, p("lwd"), par("lwd"))
    boxcol <- pcycle(pars$boxcol, border)
    boxfill <- pcycle(pars$boxfill, par("bg"))
    boxwex <- pcycle(pars$boxwex, 0.8 * {
        if (n <= 1) 
            1
        else stats::quantile(diff(sort(if (xlog) 
            log(at)
        else at)), 0.1)
    })
    medlty <- pcycle(pars$medlty, p("lty"), par("lty"))
    medlwd <- pcycle(pars$medlwd, 3 * p("lwd"), 3 * par("lwd"))
    medpch <- pcycle(pars$medpch, NA_integer_)
    medcex <- pcycle(pars$medcex, p("cex"), par("cex"))
    medcol <- pcycle(pars$medcol, border)
    medbg <- pcycle(pars$medbg, p("bg"), par("bg"))
    whisklty <- pcycle(pars$whisklty, p("lty"), "dashed")
    whisklwd <- pcycle(pars$whisklwd, p("lwd"), par("lwd"))
    whiskcol <- pcycle(pars$whiskcol, border)
    staplelty <- pcycle(pars$staplelty, p("lty"), par("lty"))
    staplelwd <- pcycle(pars$staplelwd, p("lwd"), par("lwd"))
    staplecol <- pcycle(pars$staplecol, border)
    staplewex <- pcycle(pars$staplewex, 0.5)
    outlty <- pcycle(pars$outlty, "blank")
    outlwd <- pcycle(pars$outlwd, p("lwd"), par("lwd"))
    outpch <- pcycle(pars$outpch, p("pch"), par("pch"))
    outcex <- pcycle(pars$outcex, p("cex"), par("cex"))
    outcol <- pcycle(pars$outcol, border)
    outbg <- pcycle(pars$outbg, p("bg"), par("bg"))
    outwex <- pcycle(pars$outwex, 0.5)
    width <- if (!is.null(width)) {
        if (length(width) != n | any(is.na(width)) | any(width <= 
            0)) 
            stop("invalid boxplot widths")
        boxwex * width/max(width)
    }
    else if (varwidth) 
        boxwex * sqrt(z$n/max(z$n))
    else if (n == 1) 
        0.5 * boxwex
    else rep.int(boxwex, n)
    if (horizontal) {
        xypoints <- function(x, y, ...) grid.points(y, x, ...)
        xypolygon <- function(x, y, ...) grid.polygon(y, x, ...)
        xysegments <- function(x0, y0, x1, y1, ...) grid.segments(y0, 
            x0, y1, x1, ...)
    }
    else {
        xypoints <- grid.points
        xypolygon <- grid.polygon
        xysegments <- grid.segments
    }
    ok <- TRUE
    for (i in 1:n) ok <- ok & bplt(at[i], wid = width[i], stats = z$stats[, 
        i], out = z$out[z$group == i], conf = z$conf[, i], notch = notch, 
        xlog = xlog, i = i)
    if (!ok) 
        warning("some notches went outside hinges ('box'): maybe set notch=FALSE")
    if (xaxis) grid.xaxis()
    if (yaxis) grid.yaxis()
    
#    do.call("title", pars[names(pars) %in% c("main", "cex.main", 
#        "col.main", "sub", "cex.sub", "col.sub", "xlab", "ylab", 
#        "cex.lab", "col.lab")])

    if (frame.plot) 
        grid.rect()
    popViewport()
    popViewport()
    invisible(at)
}
