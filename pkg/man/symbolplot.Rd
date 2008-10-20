\name{symbolplot}
\docType{methods}
\alias{symbolplot}
\alias{plotclust}
\alias{symbolplot,kcca-method}
\title{ symbolplot }
\description{
  Visualization of \code{flexclust} (\code{kcca}-)based cluster solutions.
}
\usage{
\S4method{symbolplot}{kcca}(object, x = NULL, y = NULL, layout = NULL, 
         filt = 0.1, node.function = NodeIsCircle, 
         edge.function = EdgeIsLineWidth, data = NULL, 
         xlim = NULL, ylim = NULL, asize = NULL, 
         rsize = NULL, axes = FALSE, 
         keepAspectRatio = TRUE, ...)
}
\arguments{
  \item{object}{ An object of class \code{kcca} }
  \item{x}{ either the x coordinates for the graph's nodes or a nx2 matrix
    with the coordinates for all the nodes of the graph. }
  \item{y}{ if \code{x} is a vector a vector \code{y} must be specified, 
    giving the y coordinates for the nodes. }
  \item{layout}{ Not used yet, leave \code{NULL} }
  \item{filt}{ Filt out edges with weigth >= \code{filt} }
  \item{node.function}{ Grid-based function, plotting nodes.
    To work correctly, the function will take 4 arguments:
    
    \code{object} is an object of class \code{kcca}.
    
    \code{cluster} is an integer giving the node/cluster number.
    
    \code{data} is a data.frame of external data. }
  \item{edge.function}{ Grid-based function for plotting edges. 
    Choose \code{EdgeIsLine} for simple lines, \code{EdgeIsLineWidth}
    for visualizing edge weights, or give user--defined function.}
  \item{data}{ data frame. external data for node drawing.
    (passed to \code{node.function}). }
  \item{xlim}{ the range to be encompassed by the x axis (vector of length two) }
  \item{ylim}{ the range to be encompassed by the y axis (vector of length two) }
  \item{asize}{ sets the size of the nodes' viewports absolutely.
    If \code{asize} is a vector (x,y) of length two, all viewports are x wide
    and y high. If asize is a nx2 matrix, every row gives the size for
    the corresponding node's viewport. If not given, a "good solution"
    is computed, where the viewports are squares as big as possible.
    See also argument rsize. }
  \item{rsize}{ this argument is a vector of length n, giving a scale
    factor for every viewport's size.
    The i-th element scales the i-th viewport. }
  \item{axes}{ if \code{TRUE}, axes are drawn }
  \item{keepAspectRatio}{ logical. If \code{TRUE} xscale and yscale ist set
    equal to keep aspect ratio of nodes.}
  \item{\dots}{ is passed to \code{node.function} }
}
\author{ Original code by Simon Floery. Modified by Ingo Voglhuber. }
\examples{
require(flexclust)

## create object of class kcca
cl <- kcca(USJudgeRatings, k = 5, save.data=TRUE)

# plot simple rectangles as nodes
testplot1 <- function (object, cluster, data, ...) {
   grid.rect()
}
symbolplot(cl,node.function=testplot1)

## plot simple circles as nodes
testplot2 <- function (object, cluster, data, ...) {
   grid.circle()
}
symbolplot(cl,node.function=testplot2)

## spiderplots as nodes
gspiderplot1 <- function (object, cluster, data, ...) {
   index <- (object@cluster == cluster)
   nodedata <- object@data@get("designMatrix")[index,]
   grid.stars(nodedata,radius=FALSE,locations="spider",
              draw.labels=FALSE)
}
symbolplot(cl,node.function=gspiderplot1)

gspiderplot2 <- function (object, cluster, data, ...) {
   index <- (object@cluster == cluster)
   nodedata <- object@data@get("designMatrix")[index,]
   grid.stars(nodedata,radius=FALSE,locations="spider",
              quantile=TRUE,draw.labels=FALSE)
}
symbolplot(cl,node.function=gspiderplot2)

gspiderplot3 <- function (object, cluster, data, ...) {
   index <- (object@cluster == cluster)
   nodedata <- object@data@get("designMatrix")[index,]
   grid.stars(nodedata,radius=FALSE,locations="spider",
              quantile=TRUE,draw.labels=TRUE)
}
symbolplot(cl,node.function=gspiderplot3)

## barplots as nodes
gbarplot1 <- function (object, cluster, data, ...) {
   index <- (object@cluster == cluster)
   nodedata <- object@data@get("designMatrix")[index,]
   grid.barplot(t(as.matrix(mean(as.data.frame(nodedata)))))
}
symbolplot(cl,node.function=gbarplot1)

## thermometer plots as nodes
gthermometers1 <- function (object, cluster, data, ...) {
   index <- (object@cluster == cluster)
   nodedata <- object@data@get("designMatrix")[index,]
   x <- 1
   y <- 1
   z <- cbind(1,1,mean(nodedata[,1])/max(USJudgeRatings[,1]))
   grid.symbols(x,y,thermometers=z)
}
symbolplot(cl,node.function=gthermometers1)
}
\keyword{hplot}
\keyword{methods}
