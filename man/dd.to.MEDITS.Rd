\name{dd.to.MEDITS}
\alias{dd.to.MEDITS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Conversion of decimal degrees coordinates in MEDITS format
}
\description{
Conversion of decimal degrees coordinates in MEDITS format
}
\usage{
dd.to.MEDITS(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data frame of the hauls data (TA, table A) in MEDITS format
}
}
\value{
The function returns the data frame of the TA (table A) reporting the coordinates in MEDITS format.
}
\author{
Walter Zupa
}

\examples{
TA.dd <- MEDITS.to.dd(TA)
dd.to.MEDITS(TA.dd)
}

\keyword{coordinates}% use one of  RShowDoc("KEYWORDS")
\keyword{TA}
