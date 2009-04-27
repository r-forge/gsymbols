\name{symb.stars}
\alias{symb.stars}
\title{ Grid-Based 'stars' Function }
\description{
  Similar to \code{stars} function, but drawn in grid viewport:
  Draw star plots or segment diagrams of a multivariate data set.
  With one single location, also draws 'spider' (or 'radar') plots.
}
\usage{
symb.stars(x, radius = TRUE, locations = "grid", 
           draw.segments = FALSE, scale = TRUE, full = TRUE, 
           qx = apply(x, 2, quantile, probs = seq(0, 1, 0.1)), 
           quantile=FALSE, col.segments=TRUE, col.stars=FALSE, 
           draw.labels=TRUE, key=FALSE, lwd=0.3, lty=1, cex=1)
}
\arguments{
  \item{x}{ vector, matrix or data frame of data. One star or segment plot 
    will be produced for each row of \code{x}. Missing values (\code{NA}) are
    allowed, but they are treated as if they were 0 (after
    scaling, if relevant). }
  \item{radius}{ logical flag: in \code{TRUE}, the radii corresponding to each
    variable in the data will be drawn. }
  \item{locations}{ Either two column matrix with the x and y coordinates used
    to place each of the segment plots; or numeric of length 2
    when all plots should be superimposed (for a \code{spider plot}).
    By default, \code{locations = NULL}, the segment plots will be
    placed in a rectangular grid. }
  \item{draw.segments}{ logical. If \code{TRUE} draws a segment diagram. }
  \item{scale}{ logical flag: if \code{TRUE}, the columns of the data matrix
    are scaled independently so that the maximum value in each column
    is 1 and the minimum is 0. If \code{FALSE}, the presumption is
    that the data have been scaled by some other algorithm to the
    range [0,1]. If \code{x} is a vector scale is forced \code{TRUE}. }
  \item{full}{ logical flag: if \code{TRUE}, the segment plots will occupy a
    full circle.  Otherwise, they occupy the (upper) semicircle only. }
  \item{qx}{ matrix or data frame of data for quantiles in background }
  \item{quantile}{ logical. If \code{TRUE}, draws quantiles in background. }
  \item{col.segments}{ colour vector (integer or character, see \code{par}),
    each specifying a colour for one of the segments (variables).
    If \code{FALSE} no colour is used.
    If \code{TRUE} vector of length \code{col(x)} is used.
    Ignored if \code{draw.segments = FALSE}. }
  \item{col.stars}{ colour vector (integer or character, see \code{par}), each
    specifying a colour for one of the stars (cases).
    If \code{FALSE} no colour is used.
    If \code{TRUE} vector of length \code{col(x)} is used.
    Ignored if \code{draw.segments = TRUE}. }
  \item{draw.labels}{ logical. If \code{TRUE} draws labels. }
  \item{key}{ logical. If \code{TRUE} draws only labels (e.g. caption) }
  \item{lwd}{ line width, passed to \code{gp} for Grid functions }
  \item{lty}{ line type, passed to \code{gp} for Grid functions }
  \item{cex}{ numerical value, giving the amount by which plotting text
          and symbols should be magnified relative to the default,
          passed to \code{gp} for Grid functions}
}
\details{
  Missing values are treated as 0.

  Each star plot or segment diagram represents one row of the input
  \code{x}.  Variables (columns) start on the right and wind
  counter-clockwise around the circle.  The size of the (scaled)
  column is shown by the distance from the center to the point on
  the star or the radius of the segment representing the variable.

  Only one page of output is produced.
}
\author{ Ingo Voglhuber }
\seealso{ \code{\link{symb.barplot}}, \code{\link{symb.matplot}},
  \code{\link{symb.pie}}, \code{\link{symb.symbols}}, \code{\link{symb.bxp}}
}
\examples{
data(USJudgeRatings)
grid.newpage()

## example

symb.stars(USJudgeRatings)

grid.newpage()

## no radii
symb.stars(USJudgeRatings,radius=FALSE)

grid.newpage()

## no labels
symb.stars(USJudgeRatings, draw.labels=FALSE)

grid.newpage()

## segments of a circle
symb.stars(USJudgeRatings,draw.segments=TRUE)

grid.newpage()

## spiderplot, no labels
symb.stars(USJudgeRatings,radius=FALSE, locations="spider", 
           draw.labels=FALSE)

grid.newpage()

## spiderplot with quantiles in background
symb.stars(USJudgeRatings,radius=FALSE, locations="spider", 
           quantile=TRUE)

grid.newpage()

## spiderplot with quantiles in background
symb.stars(USJudgeRatings,radius=FALSE, locations="spider",
  qx=apply(USJudgeRatings,2,quantile,probs=seq(0,1,0.25)) , 
           quantile=TRUE,
  draw.labels=FALSE)

grid.newpage()

## draw only labels
symb.stars(USJudgeRatings, key=TRUE)
}
\keyword{hplot}
