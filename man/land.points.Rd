\name{land.points}
\alias{land.points}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check of hauls position
}
\description{
The function checks whether the hauls coordinates is erroneously on the land.
}
\usage{
land.points(TA, land=countries, verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TA}{
data frame of the hauls data (TA, table A)
}
  \item{land}{
shapefile of countries. Made with Natural Earth. Free vector and raster map data @ naturalearthdata.com.
}
\item{verbose}{bulean parameter, if TRUE returns messages about the progress of the elaboration}
}
\details{
The function uses a polygon shapefile of lands as reference to assess whether the points of the hauls fall either on the land or on the sea.
}

\value{The function returns the data frame of the hauls coordinates that fall on the land}
\author{
Walter Zupa
}

\examples{
land.points(TA, land=countries, verbose=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ points }% use one of  RShowDoc("KEYWORDS")
\keyword{ land }% __ONLY ONE__ keyword per line
\keyword{ polygons }% __ONLY ONE__ keyword per line
