\name{bubble.rs}
\alias{bubble.rs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot of abundance indices
}
\description{
The function, using TA (table A, hauls data), TB (table B, catches data), and TC (table C, biological data) tables estimates the abundance indices per hauls for recruits and spawners specimens or for all the cougth specimens togheter. Recruits and spawners specimens are selected using the body length cutoff parameter defined by the user. Bathymetrical lines (class: "SpatialLinesDataFrame", "sp") refers respectively to 50m, 200m, and 800m of depth.
}
\usage{
bubble.rs(TA, TB, TC, GSA=NA, sspp =NA, stage ="recruits",
cutoff=NA, buffer =NA, country=NA,wm=wmap,
land=countries, d1=depth_1, d2=depth_2, d3=depth_3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TA}{
data frame containing the hauls data (TA, table A).
}
  \item{TB}{
data frame containing the catches data (TB, table B).
}
  \item{TC}{
data frame containing the aggregated biological data (TC, table C).
}
  \item{GSA}{
reference Geographical Sub-Area (GSA) for the analysis.}
  \item{country}{
string value the country selected for the analysis in case the latter should be performed by country.}
  \item{sspp}{
string of the species code (MEDITS format). The default value is NA and, in case in the data there is only one species, the species code is determined by the merge data frame. If there are more than one species, the species code is mandartory.
}
  \item{stage}{
fraction of the population that is selected for the analysis: "recruits", "spawners" or "all.stages". "all.stages" allows to plot the abundance index by haul, considering all the specimens.
}
  \item{cutoff}{
numeric value (threshold) of maturity length to be used for selecting the fraction of the population for the analysis.
}
  \item{buffer}{
numeric value to define the extension (in meters) of the buffer around the study area (haul positions range) in the map.
}

\item{wm}{ shapefile of the land's contours of the world map}
\item{land}{ shapefile of the world's land}
\item{d1}{ shapefile of the 50m bathymetrical line}
\item{d2}{ shapefile of the 200m bathymetrical line}
\item{d3}{ shapefile of the 800m bathymetrical line}
}
\details{
The function performs the analysis for one stage at time. The abundance indices per haul are estimated selecting either the total number of specimens with Total Length (TL) >= cutoff value for spawners or with TL <= cutoff for recruits.
}
\value{
The function returns the bubble plot of the abundance indices of the selected life stage.
}
\author{
Walter Zupa
}

\examples{
\donttest{
library(MEDITS)
bubble.rs(TA, TB, TC, GSA=10, sspp ="ARISFOL", stage ="recruits",
cutoff=29, buffer =5000, country ="ITA")
}
}

\keyword{ bubble plot }% use one of  RShowDoc("KEYWORDS")
\keyword{ abundance }% __ONLY ONE__ keyword per line
\keyword{ recruits }
\keyword{ spawners }
