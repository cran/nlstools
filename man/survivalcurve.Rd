\name{survivalcurve}
\alias{survivalcurve}
\docType{data}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Bacterial survival data sets }

\description{
  A list of 3 bacterial kinetics data sets
}

\usage{
data(survivalcurve)
}
%- maybe also 'usage' for other objects documented here.

\format{
A list containing 3 dataframes with 2 columns (t: time, LOG10N: decimal logarithm of bacterial density)
}

\source{ Data obtained by Florent Baty \email{florent.baty@unibas.ch} and Marie-Laure Delignette-Muller \email{ml.delignette@vet-lyon.fr}}

%\references{  }

\examples{
data(survivalcurve)
def.par <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
lapply(survivalcurve, plot, type = "b")
par(def.par)
}

\keyword{ datasets }% at least one, from doc/KEYWORDS
