\name{spearman.test}
\alias{spearman.test}
\title{
Spearman test for time series trends
}
\description{
The function uses the non parametric Spearman test to assess the presence of significant trends in the time series}
\usage{
spearman.test(timeseries, years)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timeseries}{
data frame of the indices time series as it is estimated by the \code{index.ts} function}
  \item{years}{
vector of the time series year range}
}
\details{
In presence of a short trawl survey time series the significance of the trend can be estimated using nonparametric statistical tests as Spearman rho (Cotter, 2009). Spearman's rho is the product-moment correlation between the ranks of paired data. To test for trend, one member of the pair is the time of observation, the other is the observed variable. This is also known as the Hotelling-Pabst test. It appears to be sensitive to monotonic trends.
}
\value{The function returns the statistics of the Spearman's test.
\item{r}{Spearman's correlation coefficient.}
\item{p}{t statistics.}
\item{p}{the p-value for the test.} }
\references{
Cotter J. (2009) A selection of nonparametric statistical methods for assessing trends in trawl survey indicators as part of an ecosystem approach to fisheries management (EAFM). Aquat. Living Resour. 22, 173-185.
}
\author{
Walter Zupa
}

\examples{
library(MEDITS)
merge_TATB <- m.TATB(TA,TB,"ARISFOL")
timeseries <- index.ts(merge_TATB, index="abundance", depth_range = c(500,800),
str.scheme=strata_scheme,surf=stratification_scheme, plot=FALSE)
spearman.test(timeseries, years=c(2007,2016))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spearman test }% use one of  RShowDoc("KEYWORDS")
\keyword{ trend }% __ONLY ONE__ keyword per line
