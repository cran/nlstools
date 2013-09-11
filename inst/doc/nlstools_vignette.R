### R code from vignette source 'nlstools_vignette.Rnw'

###################################################
### code chunk number 1: nlstools_vignette.Rnw:22-23
###################################################
library(nlstools)


###################################################
### code chunk number 2: nlstools_vignette.Rnw:39-42
###################################################
data(survivalcurve2)
preview(formula=mafart, data=survivalcurve2,
start=list(p = 1, delta = 1, LOG10N0 = 7))


###################################################
### code chunk number 3: nlstools_vignette.Rnw:47-49
###################################################
preview(formula=mafart, data=survivalcurve2,
start=list(p = 1, delta = 10, LOG10N0 = 7))


###################################################
### code chunk number 4: nlstools_vignette.Rnw:54-56
###################################################
preview(formula=mafart, data=survivalcurve2,
start=list(p = 2, delta = 10, LOG10N0 = 7.5))


###################################################
### code chunk number 5: nlstools_vignette.Rnw:65-67
###################################################
nlsmaf <- nls(mafart, survivalcurve2, list(p = 2, delta = 10, LOG10N0 = 7.5))
plotfit(nlsmaf,smooth=TRUE)


###################################################
### code chunk number 6: nlstools_vignette.Rnw:72-74
###################################################
model <- LOG10N ~ LOG10N0 - (t/delta)^p
nlsmaf <- nls(model, survivalcurve2, list(p = 2, delta = 10, LOG10N0 = 7.5))


###################################################
### code chunk number 7: nlstools_vignette.Rnw:79-85
###################################################
data(ross)
d6<-subset(ross, select = c(T, pH, aw, sqrtmumax))
nls6 <- nls(cpm_T_pH_aw, d6, list(muopt = 2, Tmin = 4,
Topt = 40, Tmax = 49,pHmin = 4, pHopt = 6.5, pHmax = 9,
awmin = 0.95, awopt = 0.995))
plotfit(nls6, variable = 1)


###################################################
### code chunk number 8: nlstools_vignette.Rnw:87-88
###################################################
plotfit(nls6, variable = 2)


###################################################
### code chunk number 9: nlstools_vignette.Rnw:93-94
###################################################
overview(nlsmaf)


###################################################
### code chunk number 10: nlstools_vignette.Rnw:103-105
###################################################
resmaf<-nlsResiduals(nlsmaf)
plot(resmaf)


###################################################
### code chunk number 11: nlstools_vignette.Rnw:112-113
###################################################
test.nlsResiduals(resmaf)


###################################################
### code chunk number 12: nlstools_vignette.Rnw:125-126
###################################################
contmaf <- nlsContourRSS(nlsmaf)


###################################################
### code chunk number 13: nlstools_vignette.Rnw:129-130
###################################################
plot(contmaf, col=FALSE, nlev=10)


###################################################
### code chunk number 14: nlstools_vignette.Rnw:144-145
###################################################
rcmaf <- nlsConfRegions(nlsmaf, length=500, exp=1)


###################################################
### code chunk number 15: nlstools_vignette.Rnw:147-148
###################################################
plot(rcmaf, bounds=T)


###################################################
### code chunk number 16: nlstools_vignette.Rnw:153-154
###################################################
rcmaf <- nlsConfRegions(nlsmaf, length=500, exp=2)


###################################################
### code chunk number 17: nlstools_vignette.Rnw:157-158
###################################################
plot(rcmaf,bounds=T)


###################################################
### code chunk number 18: nlstools_vignette.Rnw:163-164
###################################################
rcmaf <- nlsConfRegions(nlsmaf, length=2000, exp=2)


###################################################
### code chunk number 19: nlstools_vignette.Rnw:167-168
###################################################
plot(rcmaf, bounds=F)


###################################################
### code chunk number 20: nlstools_vignette.Rnw:175-178
###################################################
plot(rcmaf$cr[,1], rcmaf$cr[,3], pch=16, xlab='p', ylab='LOG10N')
contour(contmaf$seqPara[, 1], contmaf$seqPara[, 3], contmaf$lrss[[2]], 
                labels = "", levels = contmaf$lrss95, lty = 1, col = "red",add=T,lwd=5)


###################################################
### code chunk number 21: nlstools_vignette.Rnw:188-189
###################################################
jackmaf <- nlsJack(nlsmaf)


###################################################
### code chunk number 22: nlstools_vignette.Rnw:191-192
###################################################
summary(jackmaf)


###################################################
### code chunk number 23: nlstools_vignette.Rnw:197-198
###################################################
plot(jackmaf)


###################################################
### code chunk number 24: nlstools_vignette.Rnw:205-206
###################################################
boomaf <- nlsBoot(nlsmaf, niter=2000) 


###################################################
### code chunk number 25: nlstools_vignette.Rnw:209-210
###################################################
summary(boomaf)


###################################################
### code chunk number 26: nlstools_vignette.Rnw:215-216
###################################################
plot(boomaf, type="pairs")


