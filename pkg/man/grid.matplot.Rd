\name{grid.matplot}
\alias{grid.matplot}
\title{ Plot Columns of Matrices in Grid Viewport }
\description{
  Similar to \code{matplot} function, but drawn in grid viewport:
  Plot the columns of one matrix against the columns of another.
}
\usage{
grid.matplot(x, y, type = "p", lty = 1:5, lwd = 1, pch = NULL, 
             col = 1, cex = NULL, xlim = NULL, ylim = NULL, 
             ..., verbose = getOption("verbose"))
}
\arguments{
  \item{x,y}{ vectors or matrices of data for plotting.  The number of rows
    should match.  If one of them are missing, the other is taken
    as \code{y} and an \code{x} vector of \code{1:n} is used. Missing values
    (\code{NA}s) are allowed. }
  \item{type}{ \code{type} what type of plot should be drawn.
    Possible types are

             *  \code{"p"} for *p*oints,

             *  \code{"l"} for *l*ines,

             *  \code{"o"} for both \code{*o*verplotted}.
 }
  \item{lty,lwd}{ vector of line types and widths. The first element is for the
    first column, the second element for the second column, etc.,
    even if lines are not plotted for all columns. Line types
    will be used cyclically until all plots are drawn. }
  \item{pch}{ character string or vector of 1-characters or integers for
    plotting characters, see \code{points}. The first character is the
    plotting-character for the first plot, the second for the
    second, etc.  The default is digits (1 through 9, 0) then
    the lowercase and uppercase letters. }
  \item{col}{ vector of colours.  Colours are used cyclically. }
  \item{cex}{ vector of character expansion sizes, used cyclically. This
    works as a multiple of \code{gpar("cex")}.  \code{NULL} is equivalent to
    \code{1.0}. }
  \item{xlim,ylim}{ ranges of x and y axes, as in \code{plot}. }
  \item{\dots}{ Graphical parameters (see \code{gpar}).
    Hence, the high-level graphics control arguments described
    under \code{gpar}. }
  \item{verbose}{ logical.  If \code{TRUE}, write one line of what is done. }
}
\details{
  Points involving missing values are not plotted.

  The first column of \code{x} is plotted against the first column of
  \code{y}, the second column of \code{x} against the second column of \code{y},
  etc.  If one matrix has fewer columns, plotting will cycle back
  through the columns again.  (In particular, either \code{x} or \code{y} may
  be a vector, against which all columns of the other argument will
  be plotted.)

  Because plotting symbols are drawn with lines and because these
  functions may be changing the line style, you should probably
  specify \code{lty=1} when using plotting symbols.
}
\author{ Original code by the R Core development team.
         Modified by Ingo Voglhuber. }
\seealso{ \code{\link{grid.barplot}}, \code{\link{grid.pie}},
  \code{\link{grid.stars}}, \code{\link{grid.symbols}}, 
  \code{\link{grid.bxp}}
}
\examples{
require(grDevices)

grid.newpage()

## create viewport
vp <- viewport(0.25,0.75,0.45,0.45)
pushViewport(vp)
 grid.rect()
 grid.matplot((-4:5)^2)
popViewport()
## create viewport
vp <- viewport(0.25,0.25,0.45,0.45)
pushViewport(vp)
 sines <- outer(1:20, 1:4, function(x, y) sin(x / 20 * pi * y))
 grid.rect()
 grid.matplot(sines, pch = 1:4, type = "o", 
              col = rainbow(ncol(sines)))
popViewport()
## create viewport
vp <- viewport(0.75,0.5,0.45,0.45)
pushViewport(vp)
 grid.rect()
 grid.matplot(sines, type = "l", pch = 21:23, col = 2:5)
popViewport()
}
\keyword{hplot}
