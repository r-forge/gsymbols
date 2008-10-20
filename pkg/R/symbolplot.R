setGeneric("symbolplot", function(object, ...)
        standardGeneric("symbolplot"))

setMethod("symbolplot", signature(object="kcca"),
function (object, x = NULL, y = NULL, project = NULL, which=1:2,
          filt= 0.1, node.function = NodeIsCircle,
          edge.function = EdgeIsLineWidth,
          bgdata = NULL, xlim = NULL, ylim = NULL,
          asize = NULL, rsize = NULL, axes = FALSE,
          keepAspectRatio = TRUE, ...)
{
   require(flexclust)
   clsim <- clusterSim(object)

	# check argument object
	if (class(object) != "kcca") 
	   stop ("object has to be of class 'kcca'")
	
	# check argument which
   if(length(which)!=2)
      stop(sQuote("which"), " must have length 2")
	# check arguments: asize
	if (!is.null(asize))
	{
		if (is.vector(asize))
		{
			if (length (asize) != 2) stop ("vector asize has wrong length")
			# for simplicity, asize is always a nx2 matrix internal
			asize = matrix (asize, nrow = n, ncol = 2, byrow=TRUE)
		}
		else if (is.matrix(asize))
		{
			if (!all(dim(asize) == c(n,2)))
			   stop ("matrix asize has wrong size of dimensions")
		}
		else stop ("invalid asize argument")
	}
	
	# check argument rsize
	if (!is.null(rsize))
	{
		if (is.vector(rsize))
		{
			if (length(rsize) != n) stop ("vector rsize has wrong length")
		}
		else stop ("argument rsize has to be a vector of length n")
	}

	# fire up graphics
   plot.new()
	#grid.newpage()
	
   y <- NULL
   
   if (is.null(x))
   {
	   # compute 2d-Data from centers
	   if (is.null(project))
	   {
	      project <- function(x) x
	   }
	   else
      {
	      if(!is(project, "function"))
	      {
            lyt <- project
            project <- function(x) Predict(lyt, x)
         }
	   }
	   x <- project(object@centers)[,which]
	   y <- x[,2]
	   x <- x[,1]
	}
	else
	{
	   if (is.null(y))
	   {
	      if (dim(x)[2] != 2)
	         stop("If y is not given, x must be a matrix with two columns.")
	      y <- x[,2]
	      x <- x[,1]
	   }
	}
	
	# check and ev update the size for the nodes' viewports
	nsize = GetViewportSizes (x, y, asize, rsize, xlim, ylim)
	
	# place for labels etc
	if (axes) pushViewport (plotViewport (c(2.5,2.5,0.5,0.5)))
	
	corners = GetCorners (x, y, nsize, xlim, ylim)
	
	if (keepAspectRatio)
	{
	   xrange <- (corners[2] - corners[1])
	   yrange <- (corners[4] - corners[3])
	   if (xrange > yrange)
	   {
	      diffrange <- (xrange - yrange)
	      corners[3] <- (corners[3] - (diffrange/2))
	      corners[4] <- (corners[4] + (diffrange/2))
	   }
	   else
	   {
	      diffrange <- (yrange - xrange)
	      corners[1] <- (corners[1] - (diffrange/2))
	      corners[2] <- (corners[2] + (diffrange/2))
	   }
	}
   

	# now the viewport for the real data
	pushViewport (dataViewport (xData=corners[1:2],
                               yData=corners[3:4],
                               default.units="native"))
	if (axes)
	{
		grid.xaxis()
		grid.yaxis()
		grid.rect()
	}
   DrawEdges (x, y, edge.function=edge.function, filt=filt, 
              object=object, clsim=clsim)
	
   # DrawNode - places a viewport with correct dimension 'round (x,y) and calls
   # NodeFunction
	if (!is.null(node.function))
	{
      if (is.null(bgdata)) bgdata <- flexclust:::getData(object, error=TRUE)
	   # add cluster index to bgdata

      for (i in 1:length(x))
      {
         pushViewport(viewport(unit(x[i], "native"), unit(y[i], "native"), 
                      unit(nsize[i,1], "native"), 
                      unit(nsize[i,2], "native")))
         node.function (object = object, cluster = i, bgdata = bgdata, ...)
         popViewport(1)
      }
   } 
	
	# plotViewport and dataViewport
	if (axes) popViewport(2)
})

# a simple edge drawing function
                                                                                                                
EdgeIsLineWidth <- function (LineWidth=1) {
        grid.move.to (0, 0)
        grid.line.to (1, 1, gp=gpar(lwd=LineWidth))
}

EdgeIsLine <- function (LineWidth=1) {
        grid.move.to (0, 0)
        grid.line.to (1, 1)
}

# default node drawing function

NodeIsCircle <- function (object, cluster, bgdata){
        grid.circle (0.5, 0.5, r=unit(0.15,"inches"), gp=gpar(fill="white"))
        if (!is.null(bgdata)) grid.text (cluster, 0.5, 0.5, just="center", gp=gpar(fontface="bold"))
}

# euclidean distance

DEuklid <- function (x1, y1, x2, y2) {
	sqrt ((x1 - x2)^2 + (y1 - y2)^2)
}

# maximum distance

DMax <- function (x1, y1, x2, y2) {
	max (abs(x2-x1), abs(y2-y1))
}

# returns a two dimensional vector with a[i,j] and a[j,i].

GetSwappedIndex <- function (a, i, j) {
	c(a[i,j], a[j,i])
}

# returns true if matrix a is a nxn matrix

CorrectDim <- function (a, n) {
	all(dim(a) == c(n,n))
}

# NearestNeighbours (x,y) - x and y are arrays representing coordinates of
# a set of points in the plane. This functions determines the indices of the
# two nearest neighbours.
# O(n^2)

NearestNeighbours <- function (x, y) {
	n = length(x)
	mind = minx = miny = +Inf
	thisd = 0
	for (i in 1:(n-1)) {
		for (j in (i+1):n) {
			thisd = DMax (x[i], y[i], x[j], y[j])
			if (thisd < mind) {
				mind = thisd
				mini = i
				minj = j
			}
		}
	}
	c (mini, minj)
}

# GetSquare - dx and dy are the distances of the two nearest neighbours.
# Depending on these distances and depending on xlim and ylim the final
# size of the biggest possible square is computed.

GetSquare <- function (xsorted, ysorted, xlim, ylim, dx, dy) {
	if (!missing(xlim)) {
		xmin = xlim[1]
		xmax = xlim[2]
	} else {
		xmin = -Inf
		xmax = +Inf
	}
	if (!missing(ylim)) {
		ymin = ylim[1]
		ymax = ylim[2]
	} else {
		ymin = -Inf
		ymax = +Inf
	}
	n = length (xsorted)
	if (dx > dy) d = dx
	else d = dy
	a = min (d, 2*(xsorted[1]-xmin), 2*(xmax-xsorted[n]), 2*(ysorted[1]-ymin), 2*(ymax-ysorted[n]))
	a
}

# DrawEdges - place viewports between all the nodes and call EdgeFunction, which should
# draw an edge (or whatever) then.
# The nodes are at (0,0) and (1,1)

DrawEdges <- function (x, y, edge.function, filt=0, object, clsim) {
	n=length (x)
	SymSimMat <- (clsim+t(clsim))/2
	for (i in 1:(n-1)) {
		for (j in (i+1):n) {
			if (SymSimMat[i,j] >= filt)
			{
				# make the viewport as big as possible. the nodes
				width = DEuklid (x[i], y[i], x[j], y[j])
				dx = x[j] - x[i]
				dy = y[j] - y[i]
				# center of viewport is half between the two nodes 
				vx = x[i] + dx/2
				vy = y[i] + dy/2
				# and finally create the viewport, call the function and pop the viewport
				pushViewport (viewport (unit(vx, "native"), unit(vy, "native"), 
					unit(dx, "native"), unit(dy, "native")))
				do.call ("edge.function", list(LineWidth=SymSimMat[i,j]*10))
				popViewport (1)
			}
		}
	}
}

# GetViewportSizes - gets the two nearest neighbours and calls GetSquare
# to get the final viewport size.

GetViewportSizes <- function (x, y, NodeSize, rsize, xlim, ylim) {
	n = length(x)
	if (is.null(NodeSize)) {
		xsorted = sort (x, index.return=FALSE)
		ysorted = sort (y, index.return=FALSE)
		# do it yourself
		# at first, get the two nearest neighbours
		MinIndices <- NearestNeighbours (x,y)
		# get the distance of the projection towards x resp y
		dx = abs(x[MinIndices[1]] - x[MinIndices[2]])
		dy = abs(y[MinIndices[1]] - y[MinIndices[2]])
		# the compute a good rectangle
		NodeSize = matrix (GetSquare (xsorted, ysorted, xlim, ylim, dx, dy), 
			nrow=n, ncol=2)
	}
	# apply factors
	if (!is.null(rsize)) for (i in (1:n)) NodeSize[i,] = NodeSize[i,]*rsize[i]
	NodeSize
}

# GetCorners - at the beginning, a dataViewport is put onto the stack. dataViewport
# takes a set of points as arguments. GetCorners computes depending on its arguments
# two points in the lower left and upper right corner to take advantage of dataViewport

GetCorners <- function (x, y, NodeSize, xlim, ylim)
{
	if (is.null(xlim)) {
		xlim[1] = min (x - NodeSize[,1]/2)
		xlim[2] = max (x + NodeSize[,1]/2)
	}
	if (is.null(ylim)) {
		ylim[1] = min (y - NodeSize[,2]/2)
		ylim[2] = max (y + NodeSize[,2]/2)
	}	
	c(xlim, ylim)
}

### A wrapper for predict to allow special cases like lda
Predict <- function(object, newdata)
{
    if(inherits(object, "lda"))
        return(predict(object, newdata)$x)
    else
        return(predict(object, newdata))
}
