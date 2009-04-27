\name{symb.pie}
\alias{symb.pie}
\title{ Grid-Based Pie Charts }
\description{
  Draw a grid-based pie chart.
}
\usage{
symb.pie(x, labels = names(x), edges = 200, radius = 0.8,
         clockwise = FALSE,
         init.angle = if (clockwise) 90 else 0,
         col = NULL, border = NULL, 
         lty = NULL)
}
\arguments{
  \item{x}{ a vector of non-negative numerical quantities. The values in
    \code{x} are displayed as the areas of pie slices. }
  \item{labels}{ one or more expressions or character strings giving names for
    the slices.  Other objects are coerced by \code{as.graphicsAnnot}.
    For empty or \code{NA} (after coercion to character) labels, no
    label nor pointing line is drawn. }
  \item{edges}{ the circular outline of the pie is approximated by a polygon
    with this many edges. }
  \item{radius}{ the pie is drawn centred in a square box whose sides range
    from -1 to 1.  If the character strings labelling the slices
    are long it may be necessary to use a smaller radius. }
  \item{clockwise}{ logical indicating if slices are drawn clockwise or 
    counter-clockwise (i.e., mathematically positive direction), the
    latter is default. }
  \item{init.angle}{ number specifying the starting angle (in degrees)
    for the slices. Defaults to 0 (i.e., '3 o'clock') unless \code{clockwise}
    is true where 'init.angle' defaults to 90 (degrees), (i.e.,
    12 o'clock'). }
  \item{col}{ a vector of colours to be used in filling or shading the
    slices. If missing a set of 6 pastel colours is used, unless
    \code{density} is specified when \code{par("fg")} is used. }
  \item{border}{ a vector of colours to be used in drawing the borders of
    the slices.  }
  \item{lty}{ argument linetype passed to \code{grid.polygon} }
}
\author{ Original code by Thomas S. Dye. Modified by Ingo Voglhuber. }
\seealso{ \code{\link{symb.barplot}}, \code{\link{symb.matplot}},
  \code{\link{symb.stars}}, \code{\link{symb.symbols}},
  \code{\link{symb.bxp}}
}
\examples{
grid.newpage()

symb.pie(rep(1, 24), col = rainbow(24), radius = 0.9)

grid.newpage()

pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry",
    "Apple", "Boston Cream", "Other", "Vanilla Cream")
symb.pie(pie.sales) # default colours

grid.newpage()

symb.pie(pie.sales,
    col = c("purple", "violetred1", "green3", 
            "cornsilk", "cyan", "white"))

grid.newpage()

symb.pie(pie.sales, col = gray(seq(0.4,1.0,length=6)))

grid.newpage()

symb.pie(pie.sales, clockwise=TRUE)
}
\keyword{hplot}
