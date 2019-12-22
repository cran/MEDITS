\name{LFD}
\alias{LFD}
\title{
Length Frequency Distribution
}
\description{
The function estimates for the selected sex the time series of the length frecency distribution (LFD)
}
\usage{
LFD(merge_TATC, indices, sex, LC, depth_range, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{merge_TATC}{
data frame cantaining the hauls data (TA, table A) and the biological data (TC, table C) merged together
}
  \item{indices}{
data frame with the time series of the abundance indices as estimated by the function index.ts()
}
  \item{sex}{
string code of the selected sex. Permitted values: "c" (sex combined), "f" (females), and "m" (males).
}
  \item{LC}{
numerical value of the minimum length class interval (expressed in mm)
}
  \item{depth_range}{
vector of the depth range for the analysis
}
  \item{type}{
string value to select the type of graph to be plotted. Allowed values: "indices", "density" and "boxplot"}
}

\value{
the function returns a data frame in which the first column is the list of the lenght classes reported in the TC file and in the other columns are reported the abundance indices (\out{n/km<sup>2</sup>}) for each year of the time series by length class.
}

\author{
Walter Zupa
}
\examples{
\donttest{
library(MEDITS)
merge_TATB <-  m.TATB(TA,TB,"ARISFOL")
merge_TATC <-  m.TATC(TA,TC,"ARISFOL")
GSA <- unique(TA$AREA)
indices <- index.ts(merge_TATB,GSA,"ARISFOL",index = "abundance",
depth_range=c(200,800),sex="f", sampling = "RSS",plot=FALSE)
LFD(merge_TATC,indices,sex="f",LC=1,depth_range=c(200,800), type = "indices")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ LFD }% use one of  RShowDoc("KEYWORDS")
\keyword{ sex }% __ONLY ONE__ keyword per line
