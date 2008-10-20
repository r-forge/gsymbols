`grid.matplot` <- function (x, y, type = "p", lty = 1:5, lwd = 1,
									 pch = NULL, col = 1, cex = NULL, xlim = NULL,
									 ylim = NULL, ..., verbose = getOption("verbose")) 
{
    paste.ch <- function(chv) paste("\"", chv, "\"", sep = "", 
        collapse = " ")
    str2vec <- function(string) {
        if (nchar(string, type = "c")[1] > 1) 
            strsplit(string[1], NULL)[[1]]
        else string
    }
    xlabel <- if (!missing(x)) 
        deparse(substitute(x))
    ylabel <- if (!missing(y)) 
        deparse(substitute(y))
    if (missing(x)) {
        if (missing(y)) 
            stop("must specify at least one of 'x' and 'y'")
        else x <- 1:NROW(y)
    }
    else if (missing(y)) {
        y <- x
        ylabel <- xlabel
        x <- 1:NROW(y)
        xlabel <- ""
    }
    kx <- ncol(x <- as.matrix(x))
    ky <- ncol(y <- as.matrix(y))
    n <- nrow(x)
    if (n != nrow(y)) 
        stop("'x' and 'y' must have same number of rows")
    if (kx > 1 && ky > 1 && kx != ky) 
        stop("'x' and 'y' must have only 1 or the same number of columns")
    if (kx == 1) 
        x <- matrix(x, nrow = n, ncol = ky)
    if (ky == 1) 
        y <- matrix(y, nrow = n, ncol = kx)
    k <- max(kx, ky)
    type <- str2vec(type)
    if (is.null(pch)) {
        pch <- c(1:9, 0, letters, LETTERS)
        if (k > length(pch)) 
            warning("default 'pch' is smaller than number of columns and hence recycled")
    }
    else if (is.character(pch)) 
        pch <- str2vec(pch)
    if (verbose) 
        message("matplot: doing ", k, " plots with ", paste(" col= (", 
            paste.ch(col), ")", sep = ""), paste(" pch= (", paste.ch(pch), 
            ")", sep = ""), " ...\n", domain = NA)
    ii <- match("log", names(xargs <- list(...)), nomatch = 0)
    log <- if (ii != 0) 
        xargs[[ii]]
    xy <- xy.coords(x, y, xlabel, ylabel, log = log)
    xlim <- if (is.null(xlim)) 
        range(xy$x[is.finite(xy$x)])
    else xlim
    ylim <- if (is.null(ylim)) 
        range(xy$y[is.finite(xy$y)])
    else ylim
    if (length(type) < k) 
        type <- rep(type, length.out = k)
    if (length(lty) < k) 
        lty <- rep(lty, length.out = k)
    if (length(lwd) < k) 
        lwd <- rep(lwd, length.out = k)
    if (length(pch) < k) 
        pch <- rep(pch, length.out = k)
    if (length(col) < k) 
        col <- rep(col, length.out = k)
    if (length(cex) < k) 
        cex <- rep(cex, length.out = k)
    ii <- 1:k
	 if (is.null(xlim)) { xlim <- c(min(x), max(x)) }
	 if (is.null(ylim)) { ylim <- c(min(y), max(y)) }
    pushViewport(viewport(0.5,0.5,0.98,0.98))
	 if ( ( type == "p" ) || ( type == "o" ) )
	 {
      for (i in ii) {
			grid.points((x[, i]-xlim[1])/(xlim[2]-xlim[1]),
							(y[, i]-ylim[1])/(ylim[2]-ylim[1]),
						   pch = pch[i], gp = gpar(lty = lty[i],
						   lwd = lwd[i], col = col[i], cex = cex[i]), ...)
      }
    }
	 if ( ( type == "l" ) || ( type == "o" ) )
	 {
      for (i in ii) {
         grid.lines((x[, i]-xlim[1])/(xlim[2]-xlim[1]),
         			  (y[, i]-ylim[1])/(ylim[2]-ylim[1]),
						  gp = gpar(lty = lty[i], lwd = lwd[i],
						  col = col[i], cex = cex[i]), ...)
      }
    }
	 popViewport()
}

