\name{TATC.grid}
\alias{TATC.grid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Overlay of TA and TC tables merge with the statistical grid
}
\description{
Overlay of TA and TC tables merge with the statistical grid. A data frame is produced in which to each haul is associated the code of the corresponding grid cell.
}
\usage{
TATC.grid(TA, TC, sspp, grid.sf=cgpmgrid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{TA}{data frame of the TA table}
\item{TC}{data frame of the TC table}
\item{sspp}{string value containing the selected species code for merging. The code should be reported in the MEDITS format (rubin code), without spaces between genus and species names.}
\item{grid.sf}{shapefile of the statistical grid used by the function. It is already loaded in the package and should not be defined by the user.}
}

\author{
Walter Zupa
}

\examples{
\donttest{
TATC.grid(TA,TC,"ARISFOL", grid.sf=cgpmgrid)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ GRID }
\keyword{ TA }
\keyword{ TC }
