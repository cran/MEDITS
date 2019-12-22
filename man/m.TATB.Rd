\name{m.TATB}
\alias{m.TATB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
merge of TA and TB tables
}
\description{
The function merges the hauls data (TA, table A) and catches data (TB,  table B) for a selected species. The abundance, and biomass indices standardized per \out{km<sup>2</sup>} and hour are also estimated.
}
\usage{
m.TATB(TA, TB, sspp, str.scheme=strata_scheme, verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TA}{
data frame containing the hauls data (TA, table A).
}
  \item{TB}{
data frame containing the catches data (TB, table B).
}
  \item{sspp}{
string value of the selected species in the MEDITS format.
}
\item{str.scheme}{data frame of the strata_scheme}
\item{verbose}{bulean parameter, if TRUE returns messages about the progress of the elaboration}
}
\details{
Using TA (table A) and TB (table B) table as input, tha function returns a data frame containing the merge table for the selected species (sspp)
}

\value{
the function returns the data frame of the resulting merge between TA table and TB table.
}
\author{
Walter Zupa
}

\examples{
m.TATB(TA,TB,"ARISFOL")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ merge table }% use one of  RShowDoc("KEYWORDS")
\keyword{ TA }% __ONLY ONE__ keyword per line
\keyword{ TB }
