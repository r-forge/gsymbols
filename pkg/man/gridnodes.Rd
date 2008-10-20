\name{gridnodes}
\alias{gridnodes}
\title{ Create Grid Viewports }
\description{
  Creates grid viewports on node positions out of an object of
  class \code{Ragraph}.
}
\usage{
gridnodes(node.function, graph, object, bgdata)
}
\arguments{
  \item{node.function}{ Grid-based function, plotting nodes.
    To work correctly, the function will take four arguments:
    \code{object} is an object of class \code{kcca}.
    \code{cluster} is an integer giving the node/cluster number.
    \code{bgdata} is a data.frame of external data. }
  \item{graph}{ Object of class \code{Ragraph}. }
  \item{object}{ Object of class \code{kcca}
    (passed to \code{node.function}). }
  \item{bgdata}{ data.frame. external data for node drawing.
    (passed to \code{node.function}). }
}
\details{
  \code{gridnodes} creates viewports on node positions of a graph,
    defined by an object of class \code{Ragraph}, and calls the grid
    based functions.
}
\author{ Ingo Voglhuber }
\seealso{ \code{\link{symbolplot}} }
\keyword{dplot}
