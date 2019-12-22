\name{sexratio.ts}
\alias{sexratio.ts}
\title{
Sex ratio time series
}
\description{
Estimation of the time series of the sex ration annual indices. }
\usage{
sexratio.ts(merge, GSA = NA, sspp = NA, depth_range,
strata_scheme, stratification, country=NA, plot=TRUE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{merge}{
data frame cantaining the hauls data (TA, table A) and the catches data (TB, table B) merged together}
  \item{GSA}{
integer value corresponding to the GSA number. If not provided is determined by the merge file. Use merge file with only one GSA.
}
  \item{sspp}{
string of the species code (MEDITS format). The default value is NA and the species is determined by the merge data frame. If the merge file contains more than one species, the species should be indicated.
}
  \item{depth_range}{
numeric vector of the extreme values of the depth range.
}
  \item{strata_scheme}{
data frame containing the schema of the strata used in the survey.
}
  \item{stratification}{
data frame containing the specification of the strata surface area.
}
  \item{country}{
string value indicating the selected country for the analysis in case the analysis should be performed by country}
  \item{plot}{
boolean variable to indicate if a plot should be generated.
}
}

\details{
\bold{Sex-ratio}. Sex ratio provides information on the distribution of female and male individuals present in a population. It represents the proportion of females in a population and indicates the level of sex dominance (Adebiyi, 2013). Generally this is a peculiar trait of the population. The sex-ratio, as the proportion of the females on the overall number of individuals, can be considered correlated with the stock productivity and renewal. It is computed as:\cr
\if{html}{\figure{Eq008.png}{options: width="20\%"}}
\if{latex}{\figure{Eq008.png}}\cr
% \if{html}{\figure{sex_ratio.png}{options: width="20\%"}}
% \if{latex}{\figure{sex_ratio.png}}\cr
where FE are females, MA males, i is the haul and n the total number of hauls.
Variance is estimated as:\cr
\if{html}{\figure{Eq009.png}{options: width="30\%"}}
\if{latex}{\figure{Eq009.png}}\cr
}
\value{
the function returns a data frame with the sex ratio time series:
\item{year}{reference years of the estimated indices}
\item{Indices_F}{time series of the abundance indices of females specimens}
\item{Indices_FM}{time series of the estimated abundance indices for males and females poolled together}
\item{sex.ratio}{time series of the estimated sex ratio}
\item{variance}{variance of the estimated sex ratio}
\item{sd}{standard error of the estimated sex ratio}
\item{positive_hauls_perc}{percentage of the hauls positive to the species cought}
}
\author{
Walter Zupa
}
\references{Adebiyi F.A. (2013). The sex ratio, gonadosomatic index, stages of gonadal development and fecundity of sompat grunt Pomadasys jubelini (Cuvier, 1830). Pakistan J. Zool., vol. 45(1), pp. 41-46.}
\examples{
library(MEDITS)
m <- m.TATB(TA,TB,"ARISFOL")
sexratio.ts(merge =m, GSA = NA, sspp = NA, depth_range=c(500,800),
strata_scheme=strata_scheme, stratification=stratification_scheme, plot=FALSE )
}

\keyword{ sex ratio }
\keyword{ time series }
