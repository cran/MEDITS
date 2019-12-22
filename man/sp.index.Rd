\name{sp.index}
\alias{sp.index}

\title{
Sparial indices
}
\description{
Estimation of abundance, biomass, mean individual weight (MIW), and sex ratio indices over a statistical grid
}
\usage{
sp.index(TA, TB, sspp, type, range=NA, GSA=NA,
country=NA, threshold=NA, grid.sf=cgpmgrid,land=countries)
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
MEDITS code of the reference species
}
  \item{type}{
type of index to estimate: "abundance", "invCV", "biomass", "MIW", "sex ratio"
}
  \item{range}{
coordinate range of the study area in the form of c(xmin,xmax, ymin,ymax)
}

  \item{GSA}{
integer value corresponding to the GSA number
}
  \item{country}{
string value indicating the selected country for the analysis in case the analysis should be performed by country}
  \item{threshold}{
numeric value indicating the minimum number of specimens per haul to be considered in the sex ratio estimation. If the threshold parameter is not defined all the available specimens are considered in the estimation of the sex ratio
}
\item{grid.sf}{...}
\item{land}{...}

}
\details{
\bold{Mean biomass and mean abundance}\cr
The mean abundance (likewise the mean biomass) in the GFCM grid cells (\out{D<sub>cell</sub>}) is calculated as the average of the standardized numbers of individuals (\out{number/km<sup>2</sup>}) over the most recent 10 years of the time series (if the time series is shorter than 10 years, all the available year data are considered):\cr
\if{html}{\figure{Eq010.png}{options: width="30\%"}}
\if{latex}{\figure{Eq010.png}}\cr
% \if{html}{\figure{mean_abundance_GRID.png}{options: width="30\%"}}
% \if{latex}{\figure{mean_abundance_GRID.png}}\cr
where n is the count of the combinations year-haul in all the last 10 years. The variance of the mean
abundance in the cells is calculated as:\cr
\if{html}{\figure{Eq011.png}{options: width="35\%"}}
\if{latex}{\figure{Eq011.png}}\cr
% \if{html}{\figure{Variance_abundance_grid.png}{options: width="35\%"}}
% \if{latex}{\figure{Variance_abundance_grid.png}}\cr
The CV is calculated as the ratio between the standard deviation of the mean annual value by haul and year (numerator) and the mean biomass (or abundance) in the cell (denominator).\cr
\if{html}{\figure{Eq012.png}{options: width="20\%"}}
\if{latex}{\figure{Eq012.png}}\cr\if{html}{\figure{CV_grid.png}{options: width="20\%"}}
% \if{latex}{\figure{CV_grid.png}}\cr
% \bold{Mean individual weight}\cr
For each GFCM cell the mean individual weight is calculated by year y and haul h as ratio between the total weight W in the haul and the total number N in the haul (from the MEDITS samples data) as follows:\cr
\if{html}{\figure{Eq013.png}{options: width="30\%"}}
\if{latex}{\figure{Eq013.png}}\cr
% \if{html}{\figure{MIW_grid.png}{options: width="30\%"}}
% \if{latex}{\figure{MIW_grid.png}}\cr
where n is the count of the combinations year-haul in all the last 10 years.
The variance of the MIW in the cells is calculated using the following formula:\cr
\if{html}{\figure{Eq014.png}{options: width="35\%"}}
\if{latex}{\figure{Eq014.png}}\cr
% \if{html}{\figure{Variance_MIW_grid.png}{options: width="35\%"}}
% \if{latex}{\figure{Variance_MIW_grid.png}}\cr
Then, the Coefficient of Variation (CV) of the MIW is calculated as:\cr
\if{html}{\figure{Eq015.png}{options: width="20\%"}}
\if{latex}{\figure{Eq015.png}}\cr
% \if{html}{\figure{CV_MIW_grid.png}{options: width="20\%"}}
% \if{latex}{\figure{CV_MIW_grid.png}}\cr
\bold{Sex ratio}\cr
The sex ratio in each GFCM cell is calculated as the ratio between the sum of the standardized number of females and the sum of the standardized number of males and females over the hauls of the last 10 years:\cr
\if{html}{\figure{Eq016.png}{options: width="45\%"}}
\if{latex}{\figure{Eq016.png}}\cr
% \if{html}{\figure{Sex_ratio_GRID.png}{options: width="45\%"}}
% \if{latex}{\figure{Sex_ratio_GRID.png}}\cr
where NF and NM are the standardized number of the females and of males in the haul h and year y.
The variance of the sex ratio in the cell is calculated using the following formula:
\if{html}{\figure{Eq017.png}{options: width="55\%"}}
\if{latex}{\figure{Eq017.png}}\cr
% \if{html}{\figure{Variance_sex_rario_grid.png}{options: width="55\%"}}
% \if{latex}{\figure{Variance_sex_rario_grid.png}}\cr
where n is the count of the combinations year-haul in all the last pooled 10 years.
The CV is calculated as the ratio between the standard deviation of the sex ratio by haul and year to
the sex ratio in the cell.\cr
\if{html}{\figure{Eq018.png}{options: width="20\%"}}
\if{latex}{\figure{Eq018.png}}\cr
% \if{html}{\figure{CV_sex_ratio_GRID.png}{options: width="20\%"}}
% \if{latex}{\figure{CV_sex_ratio_GRID.png}}\cr
\bold{Inverse of mean abundance Coefficient of Variation (CV)}\cr
The inverse of the coefficient of variation of the mean abundance by GFCM cell is plotted.\cr
}
\value{the function returns the plot of the selected indices over the GFCM (General Fisheries Commission for the Mediterranean) statistical grid. Moreover, the data frame containing the values of the estimated indices is returned
\item{cgpmgridlevel}{identification number value of the corresponding grid cell.}
\item{GSA}{the corresponding geographical sub-area (GSA) of the relative cell.}
\item{meanNkm2}{mean value of the indices.}
\item{sdNkm2}{standard deviation of the mean value of the indices.}
\item{cvNkm2}{Coefficient of variation of the mean indices.}
\item{inverse_cvNkm2}{Inverse value of the mean indices coefficient of variation.}
\item{nhauls}{number of hauls in any given cell.}
\item{positive_hauls}{number of positive hauls used for the estimation of the indices.}
\item{lon}{longitude coordinate of the cell's centroid}
\item{lat}{latitude coordinate of the cell's centroid}
}
\author{
Walter Zupa
}
\examples{
\donttest{
library(MEDITS)
sp.index(TA,TB,sspp="ARISFOL",type="abundance",range = c(10, 20, 38, 42))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spatial indicators }% use one of  RShowDoc("KEYWORDS")
\keyword{ grid }% __ONLY ONE__ keyword per line
