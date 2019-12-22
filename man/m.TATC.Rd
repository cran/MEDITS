\name{m.TATC}
\alias{m.TATC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
merge of TA and TC tables
}
\description{
The function merges the hauls data (TA, table A) and biological data (TC,  table C) for a selected species. The abundance, and biomass indices standardized per \out{km<sup>2</sup>} and hour are also estimated.
}
\usage{
m.TATC(TA, TC, sspp, str.scheme=strata_scheme, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TA}{
data frame containing the hauls data (TA, table A).
}
  \item{TC}{
data frame containing the biological data (TC, table C).
}
  \item{sspp}{
string value of the selected species in the MEDITS format.
}
\item{str.scheme}{data frame of the strata_scheme}
\item{verbose}{bulean parameter, if TRUE returns messages about the progress of the elaboration}
}
\details{
Using TA and TC table as input, the function returns a data frame containing the merge table for the selected species (sspp)
}
\value{
the function returns the data frame of the resulting merge between TA (table A) and TC (table C).
}
\author{
Walter Zupa
}

\examples{
\donttest{
m.TATC(TA,TC,"ARISFOL")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ merging tables }% use one of  RShowDoc("KEYWORDS")
\keyword{ TA }% __ONLY ONE__ keyword per line
\keyword{ TC }
