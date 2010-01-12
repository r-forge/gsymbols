\name{gbarplot}
\alias{gbarplot}
\title{ Grid-Based Bar Plots }
\description{
  Creates a bar plot with vertical bars.
}
\usage{
gbarplot(x, col = 1:4, legend = "NA",
         lsize = unit(0.5, "inches"), ylim = NULL)
}
\arguments{
  \item{x}{Either a vector or matrix of values describing the bars which
     make up the plot.  If \code{x} is a vector, the plot consists
     of a sequence of rectangular bars with heights given by the
     values in the vector.  If \code{height} is a matrix each bar of 
     the plot corresponds to a column
     of \code{x}, with the values in the column giving the heights
     of stacked sub-bars making up the bar. }
  \item{col}{A vector of colours for the bars or bar components. }
  \item{legend}{A vector of characters for the bar descriptions. }
  \item{lsize}{An integer giving the size of the legend in inches. }
  \item{ylim}{Limits for the y axis. }
}
\author{Ingo Voglhuber }
\seealso{ \code{\link{gmatplot}}, \code{\link{gpie}},
  \code{\link{gstars}}, \code{\link{gsymbols}}, 
  \code{\link{gbxp}}
}
\examples{
 grid.newpage()

 gbarplot(c(10,20,50,40), legend=c("black item"))

 grid.newpage()

 gbarplot(cbind(c(10,30),c(40,20)), 
              legend=c("black item","red item"))
}
\keyword{hplot}
