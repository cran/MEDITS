\name{spear}
\alias{spear}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Spearman's test
}
\description{
Spearman test function used by the function \code{spearman.test}}
\usage{
spear(x)
}
\arguments{
  \item{x}{
indices time series
}
}
\value{statistics of the Spearman's test.
\item{r}{Spearman's correlation coefficient.}
\item{p}{t statistics.}
\item{p}{the p-value for the test.}
}
\author{
Walter Zupa}
\examples{
library(MEDITS)
x <- c(126.12,115.46,164.32,79.85,68.43,72.94, 54.23)
spear(x)
}

\keyword{ Spearman test }% use one of  RShowDoc("KEYWORDS")
\keyword{ trend }% __ONLY ONE__ keyword per line
