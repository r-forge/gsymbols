`grid.barplot` <- function(barData, boxColours = 1:4, legLabels = "NA",
									boxSize = unit(0.5,"inches"),
									ylim = NULL)
{
 if (is.vector(barData) || (is.array(barData) && (length(dim(barData)) == 
        1))) {
  barData <- t(cbind(barData))
  boxColours = "grey"
 }
 else if (!is.matrix(barData))
  stop("'barData' must be a vector or a matrix")
 nbars <- dim(barData)[2]
 nmeasures <- dim(barData)[1]
 barTotals <- rbind(rep(0, nbars), apply(barData, 2, cumsum))
 if (is.null(ylim)) barYscale <- c(0, max(barTotals) * 1.05)
 else barYscale <- ylim * 1.05
 if (legLabels[1] != "NA") {
  rightmargin = 10 }
 else {
  rightmargin = 1 }
 pushViewport(plotViewport(c(5, 4, 4, rightmargin), yscale = barYscale,
 				  layout = grid.layout(1, nbars)))
 grid.rect()
 grid.yaxis()
 for (i in 1:nbars) {
 pushViewport(viewport(layout.pos.col = i, yscale = barYscale))
 grid.rect(x = rep(0.5, nmeasures), y = unit(barTotals[1:nmeasures, i], "native"),
           height = unit(diff(barTotals[, i]), "native"), width = 0.8,
           just = "bottom", gp = gpar(fill = boxColours))
 popViewport()
 }
 popViewport()
 if (legLabels[1] != "NA") {
  nlabels <- length(legLabels)
  pushViewport(viewport(x = 0.87,
  					layout = grid.layout(nlabels, 1)))
  for (i in 1:nlabels) {
   pushViewport(viewport(layout.pos.row = i))
   grid.rect(width = unit(boxSize,"inches"), height = unit(boxSize,"inches"),
             just = "bottom", gp = gpar(fill = boxColours[i]))
   grid.text(legLabels[i], y = unit(0.5, "npc") - unit(1, "lines"))
   popViewport()
  }
  popViewport()
 }
}
