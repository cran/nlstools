## ---- setup, include=FALSE----------------------------------------------------
options(prompt = 'R> ', continue = '+ ')

## ---- fig.height=5, fig.width=5, out.width='60%', fig.cap="\\label{fig:preview}Graphically assessing the starting values prior the fit of a nonlinear model."----

library("nlstools")
formulaExp <- as.formula(VO2 ~ (t <= 5.883) * VO2rest + (t > 5.883) * 
			 (VO2rest + (VO2peak - VO2rest) * 
			  (1 - exp(-(t - 5.883) / mu))))
preview(formulaExp, data = O2K, 
	start = list(VO2rest = 400, VO2peak = 1600, mu = 1))


## -----------------------------------------------------------------------------

O2K.nls1 <- nls(formulaExp, start = list(VO2rest = 400, VO2peak = 1600, 
                  mu = 1), data = O2K)


## -----------------------------------------------------------------------------

overview(O2K.nls1)


## ---- fig.height=5, fig.width=5, out.width='60%', fig.cap="\\label{fig:plotfit}Plot of the data (dependent vs.\ independent variable) with the fitted model superimposed."----

plotfit(O2K.nls1, smooth = TRUE)


## ---- fig.width=8, fig.height=8, out.width='80%', fig.cap="\\label{fig:residuals}Plot of residuals. The top left panel shows the raw residuals vs.\ the fitted values. The top right panel shows the standardized residuals (with mean $\\mu = 0$ and standard deviation $\\sigma = 1$) vs. the fitted values. The bottom left panel shows the autocorrelation plot and the bottom right panel the QQ plot of the standardized residuals."----

O2K.res1 <- nlsResiduals(O2K.nls1)
plot(O2K.res1)


## -----------------------------------------------------------------------------

test.nlsResiduals(O2K.res1)


## ---- fig.show="hold", fig.width=8, fig.height=8, out.width='45%', fig.cap="\\label{fig:confRegions}The left panel displays the contours based on the residual sum of squares. The contours represented by a red dotted line correspond to the section of the Beale's 95\\% confidence region. The right panel shows the projections of the confidence region according to the Beale's criterion. The dashed red frames around the confidence regions correspond to the limits of the sampling regions.", warning=FALSE, comment=FALSE, results=FALSE----

O2K.cont1 <- nlsContourRSS(O2K.nls1)
plot(O2K.cont1, col = FALSE, nlev = 5)
O2K.conf1 <- nlsConfRegions(O2K.nls1, exp = 2, length = 2000)
plot(O2K.conf1, bounds = TRUE)


## -----------------------------------------------------------------------------

O2K.jack1 <- nlsJack(O2K.nls1)
summary(O2K.jack1)


## -----------------------------------------------------------------------------

O2K.boot1 <- nlsBoot(O2K.nls1)
summary(O2K.boot1)


## ---- fig.show="hold", fig.width=8, fig.height=8, out.width='45%', fig.cap="\\label{fig:resampling}Resampling procedures. The left panel shows the influence of each observation on each parameter estimate according to a jackknife procedure using \\code{nlsJack()}. The right panel shows the boxplot distribution of the bootstrapped parameter estimates obtained using \\code{nlsBoot()}.", warning=FALSE, comment=FALSE, results=FALSE----

plot(O2K.jack1)
plot(O2K.boot1, type = "boxplot")


## ---- fig.width=9, fig.height=4.5, out.width='90%', fig.cap="\\label{fig:comparisonCI}Comparison of parameter confidence intervals obtained by $t$-based and resampling (jackknife/bootstrap) methods.", warning=FALSE, comment=FALSE, results=FALSE, echo=FALSE----

esti <- summary(O2K.nls1)$parameters[, "Estimate"]
ster <- summary(O2K.nls1)$parameters[, "Std. Error"]
t95 <- qt(0.975, df = (nrow(O2K) - 3))
binf <- esti - t95 * ster
bsup <- esti + t95 * ster
par(mfrow = c(1, 3), mar = c(10,5,5,1))
plot(1:3, c(coef(O2K.nls1)[1], O2K.jack1$estijack[1, 1], O2K.boot1$bootCI[1,1]), ylim = c(O2K.boot1$bootCI[1,2]*.96, O2K.boot1$bootCI[1,3]*1.04), xlim = c(0,4), xaxt = "n", xlab = "", ylab = expression(paste(VO[2], "rest (L)")), main = expression(paste(VO[2], "rest")), pch = 19, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5); segments(x0 = 1:3, y0 = c(binf[1], O2K.jack1$jackCI[1,2], O2K.boot1$bootCI[1,2]), x1 = 1:3, y1 = c(bsup[1], O2K.jack1$jackCI[1,3], O2K.boot1$bootCI[1,3]), lty = 1:3); axis(1, at = 1:3, labels = c("t-based", "Jackknife", "Bootstrap"), las = 3, cex.axis = 1.6)
plot(1:3, c(coef(O2K.nls1)[2], O2K.jack1$estijack[2, 1], O2K.boot1$bootCI[2,1]), ylim = c(O2K.boot1$bootCI[2,2]*.96, O2K.boot1$bootCI[2,3]*1.04), xlim = c(0,4), xaxt = "n", xlab = "", ylab = expression(paste(VO[2], "peak (L)")), main = expression(paste(VO[2], "peak")), pch = 19, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5); segments(x0 = 1:3, y0 = c(binf[2], O2K.jack1$jackCI[2,2], O2K.boot1$bootCI[2,2]), x1 = 1:3, y1 = c(bsup[2], O2K.jack1$jackCI[2,3], O2K.boot1$bootCI[2,3]), lty = 1:3); axis(1, at = 1:3, labels = c("t-based", "Jackknife", "Bootstrap"), las = 3, cex.axis = 1.6)
plot(1:3, c(coef(O2K.nls1)[3], O2K.jack1$estijack[3, 1], O2K.boot1$bootCI[3,1]), ylim = c(O2K.boot1$bootCI[3,2]*.92, O2K.boot1$bootCI[3,3]*1.06), xlim = c(0,4), xaxt = "n", xlab = "", ylab = expression(paste(mu, " (L/min)")), main = expression(paste(mu, "")), pch = 19, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5); segments(x0 = 1:3, y0 = c(binf[3], O2K.jack1$jackCI[3,2], O2K.boot1$bootCI[3,2]), x1 = 1:3, y1 = c(bsup[3], O2K.jack1$jackCI[3,3], O2K.boot1$bootCI[3,3]), lty = 1:3); axis(1, at = 1:3, labels = c("t-based", "Jackknife", "Bootstrap"), las = 3, cex.axis = 1.6)


## ---- fig.show="hold", fig.width=5, fig.height=5, out.width='60%', fig.cap="\\label{fig:predCI}Plot of the data with the model superimposed and 95 percent bootstrap conifdence intervals represented as a confidence band", warning=FALSE, comment=FALSE, results=FALSE----

newdata <- data.frame(t = seq(0, 12, length.out = 50))
pred.clim <- nlsBootPredict(O2K.boot1, newdata = newdata, interval = "confidence")
plotfit(O2K.nls1, smooth = TRUE)
lines(newdata$t, pred.clim[, 2], col = "red", lty = 2)
lines(newdata$t, pred.clim[, 3], col = "red", lty = 2)


