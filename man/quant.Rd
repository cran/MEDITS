\name{quant}
\alias{quant}
\title{
\out{q<sup>th</sup>} percentile of LFD (Length Frequency Distribution)
}
\description{
The \code{quant} function estimates the length class corresponding to the selected percentile of the annual length frequency distribution (LFD) of a time series.}
\usage{
quant(freq, quantile)
}
\arguments{
  \item{freq}{
data frame of the time series of the LFD, as estimated by \code{LFD} function}
  \item{quantile}{
percentile value}
}
\details{
\bold{Length at the \out{q<sup>th</sup>} percentile (Lq)}\cr
The different percentiles of a length frequency distribution (LFD) are expected to respond differently to fishing, recruitment pulses, and loss of spawning stock. It is computed from the standardised LFD that is:\cr
\if{html}{\figure{Eq005.png}{options: width="20\%"}}
\if{latex}{\figure{Eq005.png}}\cr
% \if{html}{\figure{lfd.png}{options: width="40\%"}}
% \if{latex}{\figure{lfd.png}}\cr
where \out{fq<sub>j,l</sub>} is the number of individuals in the length class l from the haul j standardised to the \out{km<sup>2</sup>}, and \out{A<sub>j</sub>} is the surface trawled in the haul j.
The length at the \out{q<sup>th</sup>} percentile (Lq) is computed as:\cr
\if{html}{\figure{Eq006.png}{options: width="30\%"}}
\if{latex}{\figure{Eq006.png}}\cr
% \if{html}{\figure{q95.png}{options: width="40\%"}}
% \if{latex}{\figure{q95.png}}\cr
Where l is the length class corresponding to the \out{q<sup>th</sup>} percentile (0 < q < 1) for the species i, and \out{y<sub>l,i</sub>} is the value of the catch for the length class l.
The variance of the length at the \out{95<sup>th</sup>} percentile is computed as:\cr
\if{html}{\figure{Eq007.png}{options: width="30\%"}}
\if{latex}{\figure{Eq007.png}}\cr
% \if{html}{\figure{Variance_q95.png}{options: width="40\%"}}
% \if{latex}{\figure{Variance_q95.png}}\cr

}
\value{The function returns a data frame containing the time series of the selected percentiles and the relative variance values.}
\author{
Walter Zupa}
\examples{
\donttest{
library(MEDITS)
merge_TATB <-  m.TATB(TA,TB,"ARISFOL")
merge_TATC <-  m.TATC(TA,TC,"ARISFOL")
GSA <- unique(TA$AREA)
indices <- index.ts(merge_TATB,GSA,"ARISFOL",index = "abundance",
depth_range=c(500,800), sampling = "RSS",plot=FALSE)
freq <- LFD(merge_TATC,indices,sex="m",LC=1,depth_range=c(500,800))
quant(freq,0.95)
}
}

\keyword{ percentile of the LFD}% use one of  RShowDoc("KEYWORDS")
\keyword{ LFD }% __ONLY ONE__ keyword per line
