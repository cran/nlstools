\name{nlsJack}
\alias{nlsJack}
\alias{plot.nlsJack}
\alias{print.nlsJack}
\alias{summary.nlsJack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Jackknife resampling }
\description{
  Jackknife resampling
  }
\usage{
nlsJack (nls)
\method{plot}{nlsJack} (x, mfr = c(nrow(x$reldif),1), ask = FALSE, \dots)
\method{print}{nlsJack} (x, \dots)
\method{summary}{nlsJack} (object, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{nls}{ an object of class 'nls'  }
	\item{x, object}{ an object of class 'nlsJack' }
	\item{mfr}{ layout definition, default is k rows (k: number of parameters) and 1 column }
	\item{ask}{ if TRUE, draw plot interactively }
	\item{...}{ further arguments passed to or from other methods }
}

\value{ 
	\code{nlsJack} returns a list with 7 objects:
	\item{ estijack }{ a vector with jackknife estimates }
	\item{ coefjack }{ a dataframe with the parameter estimates for each jackknife sample }
	\item{ reldif }{ a dataframe with the absolute relative difference (in percent of the estimate) of the estimates with and without each observation }
	\item{ dfb }{ a dataframe with dfbetas for each parameter and each observation }
	\item{ jackCI }{ a dataframe with jackknife confidence intervals }
	\item{ rse }{ a vector with residual standard error for each jackknife sample }
	\item{ rss }{ residual a vector with residual sum of squares for each jackknife sample }
}

\details{

	A jackknife resampling procedure is performed. Each observation is sequentially removed from the initial data set using a leave-one-out strategy. A data set with \emph{n} observations provides thus \emph{n} resampled data sets of \emph{n-1} observations. The jackknife estimates with confidence intervals are calculated as described by Seber and Wild (1989) from the results of \emph{n} new fits of the model on the \emph{n} jackknife resampled data sets. The leave-one-out procedure is also employed to assess the influence of each observation on each parameter estimate. An observation is empirically defined as influential for one parameter if the difference between the estimate of this parameter with and without the observation exceeds twice the standard error of the estimate divided by \emph{sqrt(n)}. This empirical method assumes a small curvature of the nonlinear model. For each parameter, the absolute relative difference (in percent of the estimate) of the estimates with and without each observation is plotted. An asterisk is plotted for each influential observation.
}

\references{ 
	Seber GAF, Wild CJ (1989) Nonlinear regression. Wiley, New York.\cr\cr
}
\author{ 
Florent Baty \email{florent.baty@unibas.ch}\cr
Marie-Laure Delignette-Muller \email{ml.delignette@vet-lyon.fr}
}

%\note{  }

\examples{
data(growthcurve)
bacterialKinetics <- growthcurve$df4
nls1 <- nls(gompertzm, bacterialKinetics,
	list(lag = 10, mumax = 0.1, LOG10N0 = 6, LOG10Nmax = 9))
lj <- nlsJack(nls1)
plot(lj)
summary(lj)
}

\keyword{ nonlinear }% at least one, from doc/KEYWORDS
