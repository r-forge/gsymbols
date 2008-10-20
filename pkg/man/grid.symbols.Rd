\name{grid.symbols}
\alias{grid.symbols}
\title{ Grid-Based 'symbols' Function }
\description{
  Similar to \code{symbols} function, but drawn in grid viewport:
  This function draws symbols on a plot.  One of six symbols;
  \code{circles}, \code{squares}, \code{rectangles}, \code{stars}, 
  \code{thermometers}, and \code{boxplots}, can be plotted at a 
  specified set of \code{x} and \code{y} coordinates.
  Specific aspects of the symbols, such as relative size,
  can be customized by additional parameters.
}
\usage{
grid.symbols(x, y = NULL, circles, squares, rectangles,
             stars, thermometers, boxplots, 
             fg = par("col"), bg = par("col"), ...)
}
\arguments{
  \item{x, y}{ x and y co-ordinates for the centres of the symbols. }
  \item{circles}{ a vector giving the radii of the circles. }
  \item{squares}{ a vector giving the length of the sides of the squares. }
  \item{rectangles}{ a matrix with two columns.  The first column gives widths
    and the second the heights of rectangles. }
  \item{stars}{ a matrix with three or more columns giving the lengths of the
    rays from the center of the stars.  \code{NA} values are replaced
    by zeroes. }
  \item{thermometers}{ a matrix with three or four columns.  The first two
    columns give the width and height of the thermometer symbols.
    If there are three columns, the third is taken as a
    proportion: the thermometers are filled (using colour \code{fg})
    from their base to this proportion of their height.  If there
    are four columns, the third and fourth columns are taken as
    proportions and the thermometers are filled between these two
    proportions of their heights.  The part of the box not filled
    in \code{fg} will be filled in the background colour (default
    transparent) given by \code{bg}. }
  \item{boxplots}{ a matrix with five columns.  The first two columns give the
    width and height of the boxes, the next two columns give the
    lengths of the lower and upper whiskers and the fifth the
    proportion (with a warning if not in [0,1]) of the way up the
    box that the median line is drawn. }
  \item{fg}{ colour(s) the symbols are to be drawn in. }
  \item{bg}{ if specified, the symbols are filled with colour(s), the
		vector \code{bg} being recycled to the number of symbols. The
		default is to leave the symbols unfilled. }
  \item{\dots}{ arguments, passed to basic grid functions (e.g. \code{gpar()} }
}
\author{ Ingo Voglhuber }
\seealso{ \code{\link{grid.barplot}}, \code{\link{grid.matplot}},
  \code{\link{grid.pie}}, \code{\link{grid.stars}}, \code{\link{grid.bxp}}
}
\examples{
grid.newpage()

## circles.
data(trees)
attach(trees)
grid.symbols(Height,Volume,circles=Girth/8)

grid.newpage()

N <- nrow(trees)
palette(rainbow(N, end = 0.9))
grid.symbols(Height,Volume,circles=Girth/8,gp=gpar(fill=1:N))

grid.newpage()

## squares.
data(trees)
attach(trees)
grid.symbols(Height,Volume,squares=Girth/8)

grid.newpage()

N <- nrow(trees)
palette(rainbow(N, end = 0.9))
grid.symbols(Height,Volume,squares=Girth/8,gp=gpar(fill=1:N))

grid.newpage()

## rectangles.
data(trees)
attach(trees)
grid.symbols(Height,Volume,rectangles=cbind(Girth/6,Girth/8))
grid.newpage()
N <- nrow(trees)
palette(rainbow(N, end = 0.9))
grid.symbols(Height,Volume,rectangles=cbind(Girth/6,Girth/8),
             gp=gpar(fill=1:N))

grid.newpage()

## thermometers.
x <- 1:10
y <- sort(10 * runif(10))
z <- runif(10)
z <- cbind(z, 2 * runif(10), runif(10))
grid.symbols(x,y,thermometers=z)

grid.newpage()

grid.symbols(x,y,thermometers=z,fg=1:10)
grid.newpage()
grid.symbols(x,y,thermometers=z,fg=1:10,bg=1:10)

grid.newpage()

## boxplots.
x <- 1:5
y <- sort(10 * runif(5))
z <- cbind(1.5*runif(5),1.5*runif(5),runif(5)/2,
           runif(5)/2,runif(5))
grid.symbols(x,y,boxplots=z,gp = gpar(col="blue"))

grid.newpage()

## stars.
x <- 1:5
y <- 1:5
z <- cbind(1.5*runif(3),1.5*runif(3),runif(3)/2,
           runif(3)/2,runif(3),runif(3))
grid.symbols(x,y,stars=z,gp = gpar(col="blue"))

grid.newpage()

x <- 1:3
y <- 1:3
z <- cbind(runif(15),runif(15),runif(15),runif(15))
grid.symbols(x,y,stars=z,gp = gpar(col="blue"))
}
\keyword{hplot}
