\name{symb.bxp}
\alias{symb.bxp}
\title{ Draw Grid-Based Box Plots from Summaries }
\description{
  \code{bxp} draws grid-based box plots based on the given summaries in
  \code{z}.
}
\usage{
symb.bxp(z, notch = FALSE, width = NULL, varwidth = FALSE, 
         outline = TRUE, notch.frac = 0.5, border = par("fg"), 
         pars = NULL, frame.plot = FALSE, horizontal = FALSE, 
         at = NULL, xaxis = FALSE, yaxis = FALSE, ...)
}
\arguments{
  \item{z}{a list containing data summaries to be used in constructing
    the plots.  These are usually the result of a call to
    \code{boxplot}, but can be generated in any fashion.}
  \item{notch}{if \code{notch} is \code{TRUE}, a notch is drawn in each
    side of the boxes.  If the notches of two plots do not overlap, then the
    medians are significantly different at the 5 percent level.}
  \item{width}{a vector giving the relative widths of the boxes making up
    the plot.}
  \item{varwidth}{if \code{varwidth} is \code{TRUE}, the boxes are drawn
    with widths proportional to the square-roots of the number of
    observations in the groups.}
  \item{outline}{if \code{outline} is not true, the outliers are not drawn.}
  \item{notch.frac}{numeric in \code{(0,1)}. When \code{notch=TRUE}, 
    the fraction of the box width that the notches should use.}
  \item{border}{character or numeric (vector), the colour of the box borders. 
    Is recycled for multiple boxes.  Is used as default for the
    \code{boxcol}, \code{medcol}, \code{whiskcol}, \code{staplecol},
    and \code{outcol} options (see below).}
  \item{frame.plot}{logical, indicating if a \code{frame} (\code{box}) 
    should be drawn; defaults to \code{TRUE}, unless \code{axes = FALSE}
    is specified.}
  \item{horizontal}{logical indicating if the  boxplots should be horizontal;
    default \code{FALSE} means vertical boxes.}
  \item{at}{numeric vector giving the locations where the boxplots should
    be drawn, particularly when \code{add = TRUE}; defaults to \code{1:n}
    where \code{n} is the number of boxes.}
  \item{xaxis}{logical. If \code{TRUE} x-axis is drawn.}
  \item{yaxis}{logical. If \code{TRUE} y-axis is drawn.}
  \item{pars,\dots}{graphical parameters (etc) can be passed as arguments
    to this function, either as a list (\code{pars}) or normally (\code{...}),
    see the following.  (Those in \code{...} take precedence over those in
    \code{pars}.)

    Currently, \code{yaxs} and \code{ylim} are used \code{along the boxplot},
    i.e., vertically, when \code{horizontal} is false, and \code{xlim}
    horizontally. \code{xaxt}, \code{yaxt}, \code{las}, \code{cex.axis}, and
    \code{col.axis} are passed to \code{axis}, and \code{main},
    \code{cex.main}, \code{col.main}, \code{sub}, \code{cex.sub},
    \code{col.sub}, \code{xlab}, \code{ylab}, \code{cex.lab},
    and \code{col.lab} are passed to \code{title}.

    The following arguments (or \code{pars} components) allow further
    customization of the boxplot graphics.  Their defaults are
    typically determined from the non-prefixed version (e.g.,
    \code{boxlty} from \code{lty}), either from the specified argument or
    \code{pars} component or the corresponding \code{par} one.

    \code{\link[graphics]{boxwex}}: a scale factor to be applied to all boxes.
        When there are only a few groups, the appearance of the plot
        can be improved by making the boxes narrower.  The
        default depends on \code{at} and typically is 0.8.

    \code{\link[graphics]{staplewex}}, \code{\link[graphics]{outwex}}: 
        staple and outlier
        line width expansion, proportional to box width;
        both default to 0.5.

    \code{\link[graphics]{boxlty}}, \code{\link[graphics]{boxlwd}}, 
    \code{\link[graphics]{boxcol}}, \code{\link[graphics]{boxfill}}: 
        box outline type, width,
        colour, and fill colour (which currently defaults to \code{col}
        and will in future default to \code{par("bg")}).

    \code{\link[graphics]{medlty}}, \code{\link[graphics]{medlwd}}, 
    \code{\link[graphics]{medpch}}, \code{\link[graphics]{medcex}}, 
    \code{\link[graphics]{medcol}}, \code{\link[graphics]{medbg}}:
        median line type, line width, point character, point size expansion,
        colour, and background colour.  The default \code{medpch= NA}
        suppresses the point, and \code{medlty="blank"} does so for
        the line. Note that \code{medlwd} defaults to 3x the default
        \code{lwd}.

    \code{\link[graphics]{whisklty}}, \code{\link[graphics]{whisklwd}}, 
    \code{\link[graphics]{whiskcol}}:
        whisker line type (default: \code{"dashed"}), width, and colour.

    \code{\link[graphics]{staplelty}}, \code{\link[graphics]{staplelwd}}, 
    \code{\link[graphics]{staplecol}}:
        staple (= end of whisker) line type, width, and colour.}
}
\value{
     An invisible vector, actually identical to the \code{at} argument, with
     the coordinates ("x" if horizontal is false, "y" otherwise) of box
     centers, useful for adding to the plot.
  }
\author{ Original code by the R Core development team and Arni Magnusson.
         Modified by Ingo Voglhuber. }
\seealso{ \code{\link{symb.barplot}}, \code{\link{symb.matplot}},
  \code{\link{symb.pie}}, \code{\link{symb.symbols}},
  \code{\link{symb.bxp}}
}
\examples{
require(stats)
set.seed(753)
(bx.p <- boxplot(split(rt(100, 4), gl(5,20)), plot=FALSE))

grid.newpage()

symb.bxp(bx.p, yaxis=TRUE, frame.plot=TRUE)

grid.newpage()

symb.bxp(bx.p, notch = TRUE, pch = 4, boxfill=1:5)

grid.newpage()

symb.bxp(bx.p, notch = TRUE, boxfill= "lightblue",
  frame= FALSE, outl= FALSE,xaxis=TRUE,yaxis=TRUE, frame.plot=TRUE)

grid.newpage()

symb.bxp(bx.p, notch = TRUE, boxfill= "lightblue",
  border= 2:6, ylim = c(-4,4), pch = 22, bg = "green", log = "x")
}
\keyword{ hplot }
