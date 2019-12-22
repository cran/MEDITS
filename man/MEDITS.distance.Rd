\name{MEDITS.distance}
\alias{MEDITS.distance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimation of haul distance
}
\description{
Function to estimate the hauls length in meters, kilometers and nautical miles, using hauls data (TA, table A) with coordinates in the MEDITS format
}
\usage{
MEDITS.distance(data, unit = "m", verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data frame containing the hauls data (TA, table A).
}
 \item{unit}{
string value indicating the measure unit of the distance. Allowed values: "m" for meters, "km" for kilometers and "NM" for nautical miles.
}
\item{verbose}{give verbose output reporting in the output the selected measure unit of the distance.}
}
\details{
Use data frame containing coordinates in MEDITS format
}
\value{
The function returns the vector of the distances expressed in the selected measure unit.
}

\author{
Walter Zupa
}

\examples{
MEDITS.distance(TA, unit="km", verbose=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ coordinates }
\keyword{ distance }
