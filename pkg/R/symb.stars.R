`symb.stars` <- function(x, radius=TRUE, locations="grid",
                         draw.segments=FALSE, scale=TRUE, full=TRUE, 
                         qx=apply(x,2,quantile,probs=seq(0,1,0.1)),
                         quantile=FALSE, col.segments=TRUE, col.stars=FALSE,
                         draw.labels=TRUE, key=FALSE, lwd=0.3, lty=1,
                         cex=1)
{
  if (full) modpi <- (2 * pi) else modpi <- pi
  ## check argument x
  if (is.null(dim(x)))
  {
    x <- as.matrix(t(x))
    scale = FALSE
  }
  if (draw.segments) radius = TRUE
  else col.segments = FALSE

  ## check argument col.stars
  if (class(col.stars) == "logical")
  {  
    if (col.stars) col.stars <- 1:nrow(x)
  }
  else col.stars <- rep(col.stars,length.out=nrow(x))
  
  ## check argument col.segments
  if (class(col.segments) == "logical")
  {
    if (col.segments) col.segments <- 1:ncol(x)
  }
  else col.segments <- rep(col.segments,length.out=ncol(x))

  if (key) {
    pushViewport(dataViewport(0:1,0:1,extension = 0.02 ))
      for (i in 1:ncol(x)) {
        if (!draw.segments) {
          if (full) {
             if (radius)
             {
                grid.polygon(c(0.5,(cos( modpi * c(i-1,i)/ncol(x))/3)+0.5),
                             c(0.5,(sin( modpi * c(i-1,i)/ncol(x))/3)+0.5),
                             default.units="native", 
                             gp=gpar(fill=col.segments[i],lwd=lwd,lty=lty))
             }
             else
             {
                grid.polygon(c((cos( modpi * c(i-1,i)/ncol(x))/3)+0.5),
                             c((sin( modpi * c(i-1,i)/ncol(x))/3)+0.5),
                             default.units="native", 
                             gp=gpar(fill=col.segments[i],lwd=lwd,lty=lty))
             }
          }
          else {
             grid.polygon(c(0.5,(cos( modpi * c(i-1)/(ncol(x)-1))/3)+0.5),
                          c(0.5,(sin( modpi * c(i-1)/(ncol(x)-1))/3)+0.5),
                          default.units="native", 
                          gp=gpar(fill=col.segments[i],lwd=lwd,lty=lty))
             if (i != ncol(x))
                grid.polygon(c((cos( modpi * c(i-1,i)/(ncol(x)-1))/3)+0.5),
                             c((sin( modpi * c(i-1,i)/(ncol(x)-1))/3)+0.5),
                             default.units="native", 
                             gp=gpar(fill=col.segments[i],lwd=lwd,lty=lty))
          }
        }
        else {
          grid.polygon(c(0.5, cos( modpi * c(round((i-1)*360/ncol(x)):
                         round(i*360/ncol(x)))/360)/3+0.5),
                       c(0.5, sin( modpi * c(round((i-1)*360/ncol(x)):
                         round(i*360/ncol(x)))/360)/3+0.5),
                       default.units="native", 
                       gp=gpar(fill=col.segments[i],lwd=lwd,lty=lty))
        }
        if (draw.segments) {
          grid.text(dimnames(x)[[2]][i],
		              cos((modpi * c(i-1)/ncol(x)) + (pi /ncol(x)))/2.5+0.5,
		              sin((modpi * c(i-1)/ncol(x)) + (pi /ncol(x)))/2.5+0.5,
		              default.units="native", gp=gpar(cex=cex))
        }
        else {
           if (!full)
              grid.text(dimnames(x)[[2]][i],
		                  cos((modpi * c(i-1)/(ncol(x)-1)))/2.5+0.5,
		                  sin((modpi * c(i-1)/(ncol(x)-1)))/2.5+0.5,
		                  default.units="native", gp=gpar(cex=cex))
		     else
		        grid.text(dimnames(x)[[2]][i],
		                  cos((modpi * c(i-1)/ncol(x)))/2.5+0.5,
		                  sin((modpi * c(i-1)/ncol(x)))/2.5+0.5,
		                  default.units="native", gp=gpar(cex=cex))
        }
      }
    popViewport()
  }
  else
  {
  if (locations == "grid") {
    pushViewport(dataViewport(0:(ceiling(sqrt(nrow(x)))),
                  0:(ceiling(sqrt(nrow(x)))),extension = 0.02 ))
  }
  if (locations == "spider") {
    if (draw.labels) {
      pushViewport(dataViewport(0:1,0:1,extension = 0.10 ))
    }
    else {
      pushViewport(dataViewport(0:1,0:1,extension = 0.02 ))
    }
  }
  x <- as.matrix(x)
  if (scale) {
    if ( nrow(x) == 1 )
    {
      scaledx <- ((x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)))
    }
    else
    {
      scaledx <- apply(x, 2, function(x) (x - min(x, na.rm = TRUE))
                       /diff(range(x, na.rm = TRUE)))
    }
  }
  else scaledx <- x/max(x)
  posX = 0.5
  posY = 0.5
  if ((quantile == TRUE) && (locations == "spider")){
    qx <- as.matrix(qx)
    scaledqx <- apply(qx, 2, function(qx) (qx - min(qx, na.rm = TRUE))
                      /diff(range(qx, na.rm = TRUE)))
    for (i in nrow(scaledqx):1) {
      grid.polygon(scaledqx[i,] * cos( -modpi * c(ncol(scaledqx):1)
                     /ncol(scaledqx))/2 + posX,
                   scaledqx[i,] * sin( -modpi * c(ncol(scaledqx):1)
                     /ncol(scaledqx))/2 + posY,
                   default.units="native",
                   gp=gpar(col=heat.colors(nrow(scaledqx))[i],
                           fill=heat.colors(nrow(scaledqx))[i],
                           lwd=lwd,lty=lty))
    }
  }
  for (i in 1:nrow(scaledx)) {
    if (locations == "grid") {
      posX=( (i-1) %% round(sqrt(nrow(x))) ) + 1
      if ( round(sqrt(nrow(x))) == ceiling(sqrt(nrow(x))) ) posX = posX - 0.5
      posY=((ceiling(sqrt(nrow(x)))))-((ceiling(i/round(sqrt(nrow(x)))))) + 1/2
    }
    if (!radius) {
      grid.polygon(scaledx[i,] * cos( -modpi * c(ncol(scaledx):1)
                     /ncol(scaledx))/2 + posX,
                   scaledx[i,] * sin( -modpi * c(ncol(scaledx):1)
                     /ncol(scaledx))/2 + posY,
                   default.units="native", gp=gpar(fill=col.stars[i],
                   lwd=lwd,lty=lty))
    }
    else {
      if (!draw.segments) {
        for (j in 1:ncol(scaledx)) {
          if ( j == ncol(scaledx) ) k <- 1 else k <- j+1
          if (full) {
            grid.polygon(c(posX,c(scaledx[i,j],scaledx[i,k])
                           * (cos( modpi * c(j-1,j)/ncol(scaledx))/2)+posX),
                         c(posY,c(scaledx[i,j],scaledx[i,k])
                           * (sin( modpi * c(j-1,j)/ncol(scaledx))/2)+posY),
                         default.units="native", 
                         gp=gpar(fill=col.stars[i],lwd=lwd,lty=lty))
          }
          else {
            grid.polygon(c(c(scaledx[i,j],scaledx[i,k])
                     * (cos( modpi * c(j-1)/ncol(scaledx))/2)+posX),
                   c(c(scaledx[i,j],scaledx[i,k])
                     * (sin( modpi * c(j-1)/ncol(scaledx))/2)+posY),
                   default.units="native", 
                   gp=gpar(fill=col.stars[i],lwd=lwd,lty=lty))
            if (k != 1)
               grid.polygon(c(posX,c(scaledx[i,j],scaledx[i,k])
                              * (cos( modpi * c(j-1,j)/ncol(scaledx))/2)+posX),
                            c(posY,c(scaledx[i,j],scaledx[i,k])
                              * (sin( modpi * c(j-1,j)/ncol(scaledx))/2)+posY),
                            default.units="native", 
                            gp=gpar(fill=col.stars[i],lwd=lwd,lty=lty))
          }
        }
      }
      else {
        for (j in 1:ncol(scaledx)) {
          grid.polygon(c(posX, scaledx[i,j] * cos( modpi * c(round((j-1)*360
                         /ncol(scaledx)):round(j*360/ncol(scaledx)))/360)/2.2
                         +posX),
                       c(posY, scaledx[i,j] * sin( modpi * c(round((j-1)*360
                         /ncol(scaledx)):round(j*360/ncol(scaledx)))/360)/2.2
                         +posY),
                       default.units="native", gp=gpar(fill=col.segments[j],
                       lwd=lwd,lty=lty))
        }
      }
    }
    if ((draw.labels) && (locations == "grid")) grid.text(dimnames(x)[[1]][i],
			posX, posY-0.35-(0.15 * (i %% 2)), default.units="native", 
			gp=gpar(cex=cex))
  }
  if ((draw.labels) && (locations == "spider")) {
     if (draw.segments)
        for (i in 1:ncol(x)) {
          grid.text(dimnames(x)[[2]][i],
            1.39*cos((modpi * c(i-0.5)/ncol(x)))/2.5+0.5,
            1.39*sin((modpi * c(i-0.5)/ncol(x)))/2.5+0.5,
            default.units="native", gp=gpar(cex=cex))
        }
     else
        if (full)
           for (i in 1:ncol(x)) {
             grid.text(dimnames(x)[[2]][i],
               1.39*cos((modpi * c(i-1)/ncol(x)))/2.5+0.5,
               1.39*sin((modpi * c(i-1)/ncol(x)))/2.5+0.5,
               default.units="native", gp=gpar(cex=cex))
        }
        else
           for (i in 1:ncol(x)) {
             grid.text(dimnames(x)[[2]][i],
               1.39*cos((modpi * c(i-1)/(ncol(x)-1)))/2.5+0.5,
               1.39*sin((modpi * c(i-1)/(ncol(x)-1)))/2.5+0.5,
               default.units="native", gp=gpar(cex=cex))
            }
  }
  popViewport()
  }
}

