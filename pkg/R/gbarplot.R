gbarplot <- function(x, col = 1:4, legend = "NA", lsize = unit(0.5,"inches"),
                     ylim = NULL)
{
    if (is.vector(x) || (is.array(x) && (length(dim(x)) == 1)))
    {
        x <- t(cbind(x))
        col <- "grey"
    }

    else if (!is.matrix(x))
        stop("'x' must be a vector or a matrix")

    nbars <- dim(x)[2]
    nmeasures <- dim(x)[1]
    barTotals <- rbind(rep(0, nbars), apply(x, 2, cumsum))
    if (is.null(ylim)) 
        barYscale <- c(0, max(barTotals) * 1.05)
    else barYscale <- ylim * 1.05

    if (legend[1] != "NA")
        rightmargin = 10
    else rightmargin = 1

    pushViewport(plotViewport(c(5, 4, 4, rightmargin), yscale = barYscale,
                              layout = grid.layout(1, nbars)))
    grid.rect()
    grid.yaxis()
    for (i in 1:nbars) {
    pushViewport(viewport(layout.pos.col = i, yscale = barYscale))
    grid.rect(x = rep(0.5, nmeasures), y = unit(barTotals[1:nmeasures, i], "native"),
              height = unit(diff(barTotals[, i]), "native"), width = 0.8,
              just = "bottom", gp = gpar(fill = col))
    popViewport()
    }

    popViewport()
    if (legend[1] != "NA") {
        nlabels <- length(legend)
        pushViewport(viewport(x = 0.87,
                     layout = grid.layout(nlabels, 1)))
        for (i in 1:nlabels) {
        pushViewport(viewport(layout.pos.row = i))
        grid.rect(width = unit(lsize,"inches"), height = unit(lsize,"inches"),
                  just = "bottom", gp = gpar(fill = col[i]))
        grid.text(legend[i], y = unit(0.5, "npc") - unit(1, "lines"))
        popViewport()
        }
    popViewport()
    }
}
