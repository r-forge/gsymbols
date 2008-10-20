`grid.pie` <- function (x, labels = names(x), edges = 200, radius = 0.8,
							   clockwise = FALSE, init.angle = if (clockwise) 90 else 0,
							   col = NULL, border = NULL, lty = NULL)
{
    if (!is.numeric(x) || any(is.na(x) | x < 0)) 
        stop("'x' values must be positive.")
    if (is.null(labels)) 
        labels <- as.character(1:length(x))
    else labels <- as.graphicsAnnot(labels)
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    pin <- par("pin")
    ylim <- c(-1, 1)
    xlim <- ylim
    if (pin[1] > pin[2]) 
        xlim <- (pin[1]/pin[2]) * xlim
    else ylim <- (pin[2]/pin[1]) * ylim
    if (is.null(col)) 
        col <- c("white", "lightblue", "mistyrose", "lightcyan", 
                "lavender", "cornsilk")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    twopi <- if (clockwise) 
        -2 * pi
    else 2 * pi
    t2xy <- function(t) {
        t2p <- twopi * t + init.angle * pi/180
        list(x = radius * cos(t2p), y = radius * sin(t2p))
    }
    pushViewport(dataViewport(xlim,ylim))
      for (i in 1:nx) {
          n <- max(2, floor(edges * dx[i]))
          P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
          grid.polygon(c(P$x, 0), c(P$y, 0), default.units="native",
                       gp=gpar(fill=col[i], col=border[i], lty = lty[i]))
          P <- t2xy(mean(x[i + 0:1]))
          lab <- as.character(labels[i])
          if (!is.na(lab) && nzchar(lab)) {
             grid.lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y, default.units="native")
             grid.text(labels[i], 1.1 * P$x, 1.1 * P$y, default.units="native")
          }
      }
    popViewport()
    invisible(NULL)
}
