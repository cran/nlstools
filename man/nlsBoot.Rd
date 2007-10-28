\name{nlsBoot}
\alias{nlsBoot}
\alias{plot.nlsBoot}
\alias{print.nlsBoot}
\alias{summary.nlsBoot}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Bootstrap resampling }

\description{
Bootstrap resampling
}

\usage{
nlsBoot (nls, niter = 999)
\method{plot}{nlsBoot} (x, type = c("pairs", "boxplot"), 
	mfr = c(ceiling(sqrt(ncol(x$coefboot))), 
	ceiling(sqrt(ncol(x$coefboot)))), 
	ask = FALSE, \dots)
\method{print}{nlsBoot} (x, \dots)
\method{summary}{nlsBoot} (object, \dots)
}

%- maybe also 'usage' for other objects documented here.

\arguments{
	\item{nls}{ an object of class 'nls' }
	\item{niter}{ number of iterations  }
	\item{x, object}{ an object of class 'nlsBoot' }
	\item{type}{ type of representation (options are "pairs" or "boxplot")}
	\item{mfr}{ layout definition (number of rows and columns in the graphics device) }
	\item{ask}{ if TRUE, draw plot interactively }
	\item{...}{ further arguments passed to or from other methods }
}

\details{
	Non-parametric bootstraping is used. Mean centered residuals are bootstraped. By default, 999 resampled data sets are created from which a bootstrap sample of estimates is built by fitting the model on each of these data sets. The function \code{summary} returns the bootstrap estimates and the 95 percent confidence intervals which are simply defined by the median and the 2.5 and 97.5 percentiles of the bootstrap sample of estimates. The bootstrap estimates distributions can be visualized using the function \code{plot.nlsBoot} either by plotting the bootstrap sample for each pair of parameters or by displaying the boxplot representation of the bootstrap sample for each parameter.
}

\value{ 
	\code{nlsBoot} returns a list of three objects:
	\item{ coefboot }{ contains the bootstrap parameter estimates }
	\item{ bootCI }{ contains the bootstrap medians and the bootstrap 95\% confidence intervals }
	\item{ rse }{ is the vector of bootstrap residual errors }
}

\references{ 
Bates DM and Watts DG (1988) Nonlinear regression analysis and its applications. Wiley, Chichester, UK.\cr\cr
Huet S, Bouvier A, Poursat M-A, Jolivet E (2003) Statistical tools for nonlinear regression: a practical guide with S-PLUS and R examples. Springer, Berlin, Heidelberg, New York.
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
boo <- nlsBoot(nls1)
plot(boo)
plot(boo, type = "boxplot", ask=FALSE)
summary.nlsBoot(boo)
  }

\keyword{ nonlinear }% at least one, from doc/KEYWORDS
