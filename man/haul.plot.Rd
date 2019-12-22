\name{haul.plot}
\alias{haul.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot of hauls position
}
\description{
The function plots the haul positions starting from table A file (TA, hauls data).}
\usage{
haul.plot(TA, GSA = NA, country=NA, year = NA, type = "m",
labels = FALSE, land = countries,format="MEDITS")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TA}{
data frame containing the hauls data (TA, table A).
}
  \item{GSA}{
numeric value of the GSA code. The default value is NA.
}
  \item{country}{
string value indicating the selected country for the analysis in case the analysis should be performed by country}
  \item{year}{
numeric value or vector containing the years to be selected for coordinate plotting. The default value is NA.
}
  \item{type}{
string value used to indicate whether ("b") both shooting and hauling coordinates or ("m") the mean coordinate values should be plotted.
}
  \item{labels}{
logic value used to indicate whether (T) haul's code should be used as labels or not (F).
}
\item{land}{shapefile of countries. Made with Natural Earth. Free vector and raster map data @ naturalearthdata.com.}
\item{format}{character value that define the format of the used cordinates. The two allowed values are: "MEDITS" for for MEDITS coordinates format; "degrees" for coordinates reported in decimal degrees format (dd.ddd). The default value is "MEDITS"}
}

\value{The function returns the plot whith the positions of the selected hauls.}

\author{
Walter Zupa
}

\examples{
\donttest{
haul.plot(TA, GSA = 10, year = c(2007, 2008), type = "b", labels = TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{plot}% use one of  RShowDoc("KEYWORDS")
\keyword{TA}% __ONLY ONE__ keyword per line
