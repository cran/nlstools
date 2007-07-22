\name{growthcurve}
\alias{growthcurve}
\docType{data}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Bacterial kinetics data sets }

\description{
A list of 4 bacterial kinetics data sets
}

\usage{
data(growthcurve)}
%- maybe also 'usage' for other objects documented here.

\format{ 
A list containing 4 data frames with 2 columns (t: time, LOG10N: decimal logarithm of bacterial density)
}

\source{ Data obtained by Florent Baty \email{florent.baty@unibas.ch} and Marie-Laure Delignette-Muller \email{ml.delignette@vet-lyon.fr}}

%\references{  }

\examples{
data(growthcurve)
def.par <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
plot(growthcurve$df1)
plot(growthcurve$df2)
plot(growthcurve$df3)
plot(growthcurve$df4)
par(def.par)
}

\keyword{ datasets }% at least one, from doc/KEYWORDS
