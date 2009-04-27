\name{symb.barplot}
\alias{symb.barplot}
\title{ Grid-Based Bar Plots }
\description{
  Creates a bar plot with vertical bars.
}
\usage{
symb.barplot(barData, boxColours = 1:4, legLabels = "NA",
             boxSize = unit(0.5, "inches"), ylim = NULL)
}
\arguments{
  \item{barData}{ either a vector or matrix of values describing the bars which
     make up the plot.  If \code{barData} is a vector, the plot consists
     of a sequence of rectangular bars with heights given by the
     values in the vector.  Each bar of the plot corresponds to a column
     of \code{height}, with the values in the column giving the heights
     of stacked sub-bars making up the bar. }
  \item{boxColours}{ a vector of colours for the bars or bar components. }
  \item{legLabels}{ a vector of characters for the bar descriptions. }
  \item{boxSize}{ a integer giving the size of the box for the labelling 
     in inches. }
  \item{ylim}{ limits for the y axis }
}
\details{
  Simple grid-based barplot.
}
\author{ Ingo Voglhuber }
\seealso{ \code{\link{symb.matplot}}, \code{\link{symb.pie}},
  \code{\link{symb.stars}}, \code{\link{symb.symbols}}, 
  \code{\link{symb.bxp}}
}
\examples{
 grid.newpage()

 symb.barplot(c(10,20,50,40), legLabels=c("black item"))

 grid.newpage()

 symb.barplot(cbind(c(10,30),c(40,20)), 
              legLabels=c("black item","red item"))
}
\keyword{hplot}
