gsymbols <- function(x, y = NULL, circles, squares, rectangles, stars,
             thermometers, boxplots, fg = par("col"), bg = par("col"), ...)
{
    if (is.null(y)) {
        if (dim(x)[2] != 2)
            stop ("If y is not given, x must be a matrix with with two columns.")
            y = x[,2]
            x = x[,1]
    }
    if ((n=length(x)) != length(y)) stop ("x and y have different lengths.")
        count <- 0

    if (!missing(circles)) {
        count <- count + 1
        r <- circles
        grid.rect(gp = gpar(lty = "dashed"))
        pushViewport(plotViewport(c(2.1, 2.1, 2.1, 2.1)))
        pushViewport(dataViewport(x,y,extension = max( (max(r) / 
                                 (range(y)[2]-range(y)[1])), (max(r) /
                                 (range(x)[2]-range(x)[1])) ) + 0.05 ))
        grid.rect()
        grid.xaxis()
        grid.yaxis()
        grid.circle(unit(x,"native"), unit(y,"native"), unit(r,"native"),
                    default.units = "native", ...)
        popViewport()
        popViewport()
    }

    if (!missing(squares)) {
        count <- count + 1
        r <- squares
        grid.rect(gp = gpar(lty = "dashed"))
        pushViewport(plotViewport(c(2.1, 2.1, 2.1, 2.1)))
        pushViewport(dataViewport(x,y, extension = max( (max(r) / 
                                  (range(y)[2]-range(y)[1])), (max(r) / 
                                  (range(x)[2]-range(x)[1])) ) + 0.05 ))
        grid.rect()
        grid.xaxis()
        grid.yaxis()
        grid.rect(unit(x,"native"), unit(y,"native"), unit(r,"native"), 
                  unit(r * (range(y)[2]-range(y)[1]) / 
                  (range(x)[2]-range(x)[1]),"native"), 
                  just = "centre", default.units = "native", ...)
        popViewport()
        popViewport()
    }

    if (!missing(rectangles)) {
        count <- count + 1
        z <- rectangles
        rx <- z[,1]
        ry <- z[,2]
        grid.rect(gp = gpar(lty = "dashed"))
        pushViewport(plotViewport(c(2.1, 2.1, 2.1, 2.1)))
        pushViewport(dataViewport(x,y, extension = max( (max(rx,ry) / 
                                  (range(y)[2]-range(y)[1])), (max(rx,ry) / 
                                  (range(x)[2]-range(x)[1])) ) + 0.05 ))
        grid.rect()
        grid.xaxis()
        grid.yaxis()
        grid.rect(unit(x,"native"), unit(y,"native"), unit(rx,"native"), 
                  unit(ry * (range(y)[2]-range(y)[1]) / 
                  (range(x)[2]-range(x)[1]),"native"), just = "centre",
                  default.units = "native", ...)
        popViewport()
        popViewport()
    }

    if (!missing(stars)) {
        count <- count + 1
        z <- stars
        if (ncol(z) < 3) stop("need more columns (at least 3)")
        grid.rect(gp = gpar(lty = "dashed"))
        pushViewport(plotViewport(c(2.1, 2.1, 2.1, 2.1)))
        pushViewport(dataViewport(x,y,extension = 
                                 ( (max(z) / min((range(y)[2]-range(y)[1]), 
                                 (range(y)[2]-range(y)[1]) ) + 0.05) ) ) )
        grid.rect()
        grid.xaxis()
        grid.yaxis()
        for (j in 1:(dim(z)[2]))
        {
            for (i in 1:dim(z)[1] )
            {
                if ( i == dim(z)[1] ) k <- 1 else k <- i+1
                grid.segments(unit( x[j] + (z[i,j] * (cos((i-1) * (2 * pi) / 
                              (dim(z)[1])))),"native"),
                              unit( y[j] + (z[i,j] * (sin((i-1) * (2 * pi) / 
                              (dim(z)[1])))),"native"),
                              unit( x[j] + (z[k,j] * (cos((i) * (2 * pi) / 
                              (dim(z)[1])))),"native"),
                              unit( y[j] + (z[k,j] * (sin((i) * (2 * pi) / 
                              (dim(z)[1])))),"native"), ...)
            }
        }
        popViewport()
        popViewport()
    }

    if (!missing(thermometers)) {
        count <- count + 1
        z <- thermometers
        if ( length(x) > 1 ) {
            grid.rect(gp = gpar(lty = "dashed"))
            pushViewport(plotViewport(c(2.1, 2.1, 2.1, 2.1)))
            pushViewport(dataViewport(x,y,extension = max( (max(z[,2])/2)/
                         (range(y)[2]-range(y)[1]),(max(z[,1])/2)/
                         (range(x)[2]-range(x)[1]) ) + 0.05 ))
            grid.rect()
            grid.xaxis()
            grid.yaxis()
        }
        else {
            x <- z[1]/2
            y <- z[2]/2
            pushViewport(plotViewport(c(0, 0, 0, 0)))
            pushViewport(dataViewport(0:z[1],0:z[2]))
        }
        if (ncol(z) == 3)
        {
            grid.rect(unit(x,"native"), unit(y - (( z[,2] / 2 ) - 
                      (( z[,3] * z[,2] ) / 2) ),"native"), unit(z[,1],"native"),
                      unit( z[,3] * z[,2], "native"),just = "centre",
                      gp = gpar(col=fg,fill=fg), ...)
        }
        grid.rect(unit(x,"native"), unit(y,"native"), unit(z[,1],"native"), 
                  unit(z[,2],"native"), just = "centre", gp = gpar(col=bg), ...)
        grid.segments(unit( x - ( 3 * z[,1] / 4),"native"), unit(y,"native"), 
                      unit(x - ( z[,1] / 2),"native"), unit(y,"native"), 
                      default.units = "native", gp = gpar(col=bg), ...)
        grid.segments(unit( x + ( z[,1] / 2),"native"), unit(y,"native"), 
                      unit(x + ( 3 * z[,1] / 4),"native"), unit(y,"native"), 
                      default.units = "native", gp = gpar(col=bg), ...)
        popViewport()
        popViewport()
    }

    if (!missing(boxplots)) {
        count <- count + 1
        z <- boxplots
        if ( (min(z[,5]) < 0) || (max(z[,5]) > 1) ) { 
            warning("boxplots[,5] outside [0,1]") }
        grid.rect(gp = gpar(lty = "dashed"))
        pushViewport(plotViewport(c(2.1, 2.1, 2.1, 2.1)))
        pushViewport(dataViewport(x,y,extension = max( ( (max(z[,4]) + 
                     (max(z[,2]) / 2) ) / (range(y)[2]-range(y)[1]) ),
                     max(z[,1])/(range(x)[2]-range(x)[1]) ) + 0.05  ))
        grid.rect()
        grid.xaxis()
        grid.yaxis()
        grid.rect(unit(x,"native"), unit(y,"native"), unit(z[,1],"native"), 
                  unit(z[,2],"native"), just = "centre", ...)
        grid.segments(unit( x,"native"), unit(y - (z[,2]/2),"native"), 
                      unit(x,"native"), unit(y - z[,3] - (z[,2]/2),"native"),
                      ...)
        grid.segments(unit( x,"native"), unit(y + (z[,2]/2),"native"), 
                      unit(x,"native"), unit(y + z[,4] + (z[,2]/2),"native"),
                      ...)
        grid.segments(unit( x - (z[,1]/2),"native"), 
                      unit(y - (z[,2]/2) + (z[,5] * z[,2]),"native"), 
                      unit(x + (z[,1]/2),"native"), 
                      unit(y - (z[,2]/2) + (z[,5] * z[,2]),"native"), ...)
        popViewport()
        popViewport()
    }

    if (count != 1) 
        stop("exactly one symbol type must be specified")
}
