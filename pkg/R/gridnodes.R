`gridnodes` <- function (node.function, graph, object, bgdata)
{
   # check argument "bgdata"
   if (!is.null(bgdata))
   {
      if (!is.data.frame(bgdata))
      {
         warning("bgata ist not a data frame. bgata is converted to data frame!")
         bgdata <- as.data.frame(bgdata)
      }
      if ( length(object@cluster) != nrow(bgdata) )
      {
         warning("Wrong dimension of 'bgdata'!")
      }
   }
   # get bottom left x and y coordinates of bound box for graph 
   vp.bl.x <- graph@boundBox@botLeft@x
   vp.bl.y <- graph@boundBox@botLeft@y
   # get upper right x and y coordinates of bound box for graph 
   vp.ur.x <- graph@boundBox@upRight@x
   vp.ur.y <- graph@boundBox@upRight@y
   # get heights of nodes
   vp.heights <- getNodeHeight(graph)
   vp.heights <- vp.heights/(vp.ur.y-vp.bl.y)
   # get widhts of nodes
   vp.widths.r <- getNodeRW(graph)
   vp.widths.l <- getNodeLW(graph)
   vp.widths <- vp.widths.r + vp.widths.l
   vp.widths <- vp.widths/(vp.ur.x-vp.bl.x)
   # get node x and y coordinates of node centers
   vp.pos.x <- getNodeXY(graph)$x
   vp.pos.x <- (vp.pos.x-vp.bl.x)/(vp.ur.x-vp.bl.x)
   vp.pos.y <- getNodeXY(graph)$y
   vp.pos.y <- (vp.pos.y-vp.bl.y)/(vp.ur.y-vp.bl.y)
   # hardcoded borders as in Rgraphviz.R
   pushViewport(viewport(0.5,0.5,0.925,0.925))
      # create one viewport for each node and call node.function
      for (i in seq(along=AgNode(graph))) {
         vp <- viewport(x = vp.pos.x[i], y = vp.pos.y[i],
                        width = vp.widths[i], height = vp.heights[i])
         pushViewport(vp)
           node.function(object = object, cluster = i, bgdata = bgdata)
         popViewport()
      }
   popViewport()
}
