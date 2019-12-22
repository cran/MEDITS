\name{index.rs}
\alias{index.rs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimation of the time series of aggregated indices for recruits or spawners.
}
\description{
Estimation of the time series of the aggregated indices of abundance (n/km^2) for recruits speciments. The standard deviations are also estimated.The analysis is performed by GSA.
}
\usage{
index.rs(TA, TB, TC, GSA = NA, sspp, stage="recruits", cutoff,
depth_range, str.scheme=strata_scheme, surf=stratification, country=NA, plot=TRUE)
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
data frame containing the biological data (TC, table C).
}
  \item{GSA}{
reference GSA for the analysis}
  \item{sspp}{
string of the species code (MEDITS format). The default value is NA and the species is determined by the merge data frame.
}
  \item{stage}{
fraction of the population that is selected for the analysis: "recruits" or "spawners".
}
  \item{cutoff}{
threshold value of the maturity length (expressed in mm) to be used for selecting the fraction of the population for the analysis (<= for recruits; >= for spawners).
}
  \item{depth_range}{
numeric vector of the extreme values of the depth range.
}
  \item{str.scheme}{
...
}
  \item{surf}{
...
}
  \item{country}{
string value indicating the selected country for the analysis in case the analysis should be performed by country}
  \item{plot}{
boolean variable to indicate if a plot should be generated.
}
}

\details{
\bold{Index of recruits \out{(number/km<sup>2</sup>)}}. Recruits are often measured as the individuals belonging to the first component of the length frequency distributions, or as the individuals of the first age class, according to the recruitment mode, population structure and species. Thresholds to split the recruits from the whole population index can be also obtained from different areas or from literature.
For the indices calculation see mean biomass in function \code{index.ts}.

\bold{Index of spawners \out{(number/km<sup>2</sup>)}}. As individuals in spawning phase are not always intercepted by surveys, spawners can be approximated using the indices of adult individuals, i.e. those larger
than the size at first maturity. For the indices calculation see mean biomass in function \code{index.ts}.
}

\value{
the function returns a data frame with the time series of the abundance indices for the selected life stage:
\item{year}{reference years of the estimated abundance indices}
\item{abundance}{time series of the estimated abundance indices}
\item{sd}{standard deviation of the estimated abundance indices}
}

\author{
Walter Zupa
}

\examples{
\donttest{
library(MEDITS)
sp <- "ARISFOL"
depth <- c(500,800)
st <- strata_scheme
st.sch <- stratification_scheme
index.rs (TA,TB,TC,GSA=10,sspp=sp,stage ="recruits",cutoff=45,
depth_range=depth,str.scheme=st,surf=st.sch,plot=TRUE)
}
}

\keyword{ timeseries }% use one of  RShowDoc("KEYWORDS")
\keyword{ abundance }% __ONLY ONE__ keyword per line
\keyword{ biomass }

