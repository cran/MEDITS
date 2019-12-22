\name{bubbleplot}
\alias{bubbleplot}

\title{Bubble plot (generic)}
\description{The function allows to generate a bubble plot of a given variable. If the parameter \code{range} is not provided, the function estimates the coordinate range of the plot using the points' coordinates extension.}
\usage{
bubbleplot(x, y, data, variable, range = NA,inches=0.1, interval = "equal",
land=countries, d1 = depth_1, d2= depth_2, d3 = depth_3)
}

\arguments{
  \item{x}{the x coordinates (longitude) of points in the plot.}
  \item{y}{the y coordinates (latitude) of points in the plot.}
  \item{data}{data frame containing thae variable to be plotted}
  \item{variable}{character value of the variable name in \code{data}}
  \item{range}{vector of the coordinates limits of the plot in the form of \code{c(xmin,xmax, ymin,ymax)}. }
  \item{inches}{maximum dimension of the circles expressed in inches.}
  \item{interval}{character variable indicating the type of breaks to be computed for the five value levels. Allowed values: \code{"equal"} and \code{"quantiles"}. }
  \item{land}{ shapefile of the world's land}
  \item{d1}{ shapefile of the 50m bathymetrical line}
  \item{d2}{ shapefile of the 200m bathymetrical line}
  \item{d3}{ shapefile of the 800m bathymetrical line}
}
\value{The function returns the bubble plot map of a given variable in a selected range of coordinates.}
\author{Walter Zupa}
\examples{
library(MEDITS)
m <- MEDITS.to.dd(TA)
bubbleplot(x=m$SHOOTING_LONGITUDE, y=m$SHOOTING_LATITUDE,
m, "SHOOTING_DEPTH",interval="quantiles", range=c(12,17,38,42))
}

\keyword{Bubble plot}
\keyword{map}
