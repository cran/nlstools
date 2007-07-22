"nlsBoot"<-function(nls, niter=999){
	if (!inherits(nls, "nls"))
		stop("Use only with 'nls' objects")

	c1		<- nls$call
	data	<- eval(c1$data, sys.frame(0))
	c1$start <- as.list(coef(nls))
	np		<- length(coef(nls))
	tabboot	<- matrix(nrow=0,ncol=np)
	rseboot	<- vector()
	var1	<- all.vars(formula(nls)[[2]])
	cat("  ")

	for(i in 1:niter){
		tenth	<-floor(100*nrow(tabboot)/niter)

		data2		<- data
		data2[,var1]	<- as.vector(fitted(nls) + sample((resid(nls)-mean(resid(nls))), replace=TRUE))
		c1$data		<- data2
		nls2		<- eval(c1)
		tabboot		<- rbind(tabboot, coef(nls2))
		rseboot		<- c(rseboot, summary(nls2)$sigma)

		if(tenth!=floor(100*nrow(tabboot)/niter)){
				if(tenth<11){cat("\b\b",tenth,"%",sep="")}
				else{cat("\b\b\b",tenth,"%",sep="")}
			}

	}
	
	cat("\b\b\b100%\a")
	cat("\n Bootstrap procedure finished \n")
	invisible(tabboot)

	recapboot <- cbind(apply(tabboot,2,median),apply(tabboot,2,quantile,0.025),apply(tabboot,2,quantile,0.975))
	colnames(recapboot) <- c("Median","2.5%","97.5%")
	
	listboot	<-list(coefboot=tabboot, rse=rseboot, bootCI=recapboot)
	class(listboot) <- "nlsBoot"
	return(listboot)

}


"plot.nlsBoot"<-function(x, type=c("pairs","boxplot"), mfr=c(ceiling(sqrt(ncol(x$coefboot))),ceiling(sqrt(ncol(x$coefboot)))),ask=FALSE, ...){
	if (!inherits(x, "nlsBoot"))
		stop("Use only with 'nlsBoot' objects")
	tab <- x$coefboot
	np <- ncol(tab)
 	def.par <- par(no.readonly = TRUE)	
	if(type[1] == "pairs"){
		if(ask) par(ask=TRUE, mar=c(4,4,3,1))
		if(!ask){
			lay <- lower.tri(matrix(0,(np-1),(np-1)), TRUE)
			lay[which(lay, TRUE)] <- 1:choose(np,2)
			layout(lay)
			par(mar=c(5,4,0.2,0.2))
		}
		for(i in 1:(np-1))
			for(j in (i+1):np)
				plot(tab[,i], tab[,j], xlab=colnames(tab)[i], ylab=colnames(tab)[j], pch="+")
	}
	if(type[1] == "boxplot"){ 
		if(ask) par(ask=TRUE, mar=c(4,4,3,1))
		if(!ask) par(mfrow=mfr, mar=c(4,4,3,1))
		for(i in 1:np){
			boxplot(tab[,i],main=colnames(tab)[i])
		}
	}
	par(def.par)
}


"print.nlsBoot" <- function (x, ...) {
	if (!inherits(x, "nlsBoot"))
		stop("Use only with 'nlsBoot' objects")
	cat("Bootstrap resampling\n")
	cat("\n")
	sumry <- array("", c(1, 4), list(1:1, c("vector", "length", "mode", "content")))
	sumry[1, ] <- c("$rse", length(x$rse), mode(x$rse), "Bootstrap residual errors")
	class(sumry) <- "table"
	print(sumry)
	cat("\n")
	sumry <- array("", c(2, 4), list(1:2, c("data.frame", "nrow", "ncol", "content")))
	sumry[1, ] <- c("$coefboot", nrow(x$coefboot), ncol(x$coefboot), "Bootstrap parameter estimates")
	sumry[2, ] <- c("$bootCI", nrow(x$bootCI), ncol(x$bootCI), "Bootstrap medians and 95% CI")
	class(sumry) <- "table"
	print(sumry)
	cat("\n")
}

"summary.nlsBoot" <- function (object, ...) {
	if (!inherits(object, "nlsBoot"))
		stop("Use only with 'nlsBoot' objects")
	cat("\n------\n")
	cat("Bootstrap estimates\n")
	print(object$bootCI[,1])
	cat("\n------\n")
	cat("Bootstrap confidence intervals\n")
	print(object$bootCI[,2:3])
	cat("\n")
}

