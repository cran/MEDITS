\name{index.ts}
\alias{index.ts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimation of the time series of aggregated indices .
}
\description{
Estimation of the time series of the aggregated indices of abundance (n/km^2), biomass (kg/km^2) and mean individual weight (kg). The standard deviation and the standard error are also estimated.
}
\usage{
index.ts(merge, GSA = NA, sspp = NA, index = "abundance", depth_range, sex="c",
str.scheme=strata_scheme, surf=stratification_scheme, sampling="RSS",
country=NA, plot=TRUE,verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{merge}{
data frame containing the merge between table A (TA, hauls data) and table B (TB, catches data) tables.
}
  \item{GSA}{
integer value corresponding to the GSA number
}
  \item{sspp}{
string of the species code (MEDITS format). The default value is NA and the species is determined by the merge data frame.
}
  \item{index}{
string with the selected index wich the time series would be estimated. The permitted values are: "abundance", "biomass", and "MIW".
}
  \item{depth_range}{
numeric vector of the extreme values of the depth range.
}
  \item{sex}{
string variable used to define the sex of the specimens to be selected for the analysis. The sex selection is allowed only for the estimation of abundance indices, in the other cases the default value is "c". Allowed values are: "c" for combined sexes, "f" for females and "m" for males.
}
  \item{str.scheme}{
...
}
  \item{surf}{
...
}
  \item{sampling}{
string identifing the type of sampling strategy used in the survey}
  \item{country}{
string value indicating the selected country for the analysis in case the analysis should be performed by country}
  \item{plot}{
boolean variable to indicate if a plot should be generated.
}
  \item{verbose}{
bulean parameter, if TRUE returns messages about the progress of the elaboration
}
}

\details{
\bold{Number of positive hauls to the species}. \cr
It is assumed that the size of the spreading area of a stock is mainly dependent on its abundance. A trend analysis can provide insight into the
evolution of the occupied area in the medium term. This indicator can be also affected by
environmental changes.
It is computed as: (Positive hauls/Total hauls)*100 \cr

\bold{Mean biomass index \out{(kg/km<sup>2</sup>)}}. \cr
This index measures the total biomass of a species per unit area. Changes (decrease) in this indices can be caused by an excessive fishing pressure. The index is calculated as (Souplet, 1996):\cr
\if{html}{\figure{Eq001.png}{options: width="15\%"}}
\if{latex}{\figure{Eq001.png}}\cr
% \if{html}{\figure{biomass_index.png}{options: width="15\%"}}
% \if{latex}{\figure{biomass_index.png}}\cr
where I is the index, \out{W<sub>i</sub>} is the weight of the stratum i, and \out{x<sub>i</sub>} is given by:\cr
\if{html}{\figure{Eq002.png}{options: width="15\%"}}
\if{latex}{\figure{Eq002.png}}\cr
% \if{html}{\figure{X_biomass.png}{options: width="15\%"}}
% \if{latex}{\figure{X_biomass.png}}\cr
where \out{x<sub>i,j</sub>} is the weight of the individuals in the haul j of the stratum i and \out{A<sub>i,j</sub>} is the area trawled in the haul j of the stratum i; \out{n<sub>i</sub>} is the number of hauls in the stratum i. Within stratum variance is
calculated as:\cr
\if{html}{\figure{Eq003.png}{options: width="40\%"}}
\if{latex}{\figure{Eq003.png}}\cr
% \if{html}{\figure{Variance_in_stratum.png}{options: width="40\%"}}
% \if{latex}{\figure{Variance_in_stratum.png}}\cr
and the variance in the survey area as:\cr
\if{html}{\figure{Eq004.png}{options: width="30\%"}}
\if{latex}{\figure{Eq004.png}}\cr
% \if{html}{\figure{Variance_in_survey_area.png}{options: width="30\%"}}
% \if{latex}{\figure{Variance_in_survey_area.png}}\cr
where \out{f<sub>i</sub>} is the ratio between the area trawled in the stratum i and the stratum area (finite
population correction factor, generally negligible).\cr
\bold{Mean abundance index \out{(number/km<sup>2</sup>)}}. Abundance indices, like the biomass ones, can change
(decrease) for the effect of an excessive fishing pressure, however more than the biomass ones, are likely to be substantially affected by large recruitment pulses in the stock, particularly if numbers of adults are low. For the analysis of time series of such index the ln transformation of the variable \eqn{ln(x)+1} is usually applied. For computation see mean biomass index.\cr
\bold{Inverse of mean abundance Coefficient of Variation (CV)}. The reciprocal of the coefficient of
variation could be seen as a descriptor of the stability of the variable under investigation (higher
is the metrics, more stable is the variable).
It is the square root of the variance, as obtained for the biomass index, divided by the mean abundance index.\cr
\bold{Mean individual weight (MIW). Mean Individual Weight (MIW)} is generally considered an indicator that synthesizes the structure of the population (Piet and Jennings, 2005) and its changes in time are likely linked to changes in fishing pressure, though it can be also influenced by the recruitment peaks. This influence is expected to be less pronounced if older individuals in the population are well represented. Mean weight is particularly useful for those species caught in the trawl surveys for which no data on individual size is collected. It is computed as the ratio between overall biomass by haul i and overall number of individuals
by haul i: \out{B<sub>i</sub>}/\out{N<sub>i</sub>}
}


\value{
the function returns a data frame with the time series of the selected index:
\item{year}{reference years of the estimated indices}
\item{abundance}{time series of the estimated abundance indices}
\item{biomass}{time series of the estimated biomass indices}
\item{MIW}{time series of the estimated mean individual weight (MIW) indices}
\item{sd}{standard deviation of the estimated indices}
\item{se}{standard error of the estimated indices}
\item{CV}{coefficient of variation}
\item{invCV}{inverse value of the coefficient of variation}
\item{positive_hauls_perc}{percentage of the hauls positive to the species cought}
}

\author{
Walter Zupa
}
\references{
\itemize{\item Piet G.J. and Jennings S. (2005). Response of potential fish community indicators to fishing, ICES Journal of Marine Science, V.62-2, 214:225. Available on line at: http://icesjms.oxfordjournals.org/content/ 62/2/214.full
\item Souplet A. (1996). Calculation of abundance indices and length frequencies in the MEDITS survey. In: J. A. Bertrand et al. (eds), Campagne internationale du chalutage demersal en Mediterranee. Campagne 1995. EU Final Report, Vol. III.}
}

\examples{
library(MEDITS)
merge_TATB <- m.TATB(TA,TB,"ARISFOL")
ind <- index.ts(merge_TATB, index="abundance", depth_range = c(500,800), sex="c",
str.scheme=strata_scheme, surf=stratification_scheme)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ time series }% use one of  RShowDoc("KEYWORDS")
\keyword{ abundance indices }% __ONLY ONE__ keyword per line
\keyword{ biomass indices }
\keyword{ Mean Individual Weight indices }
