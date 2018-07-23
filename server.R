
# install required packages not yet installed
pckgs <- c('shiny','reshape2','ggplot2','plyr','arm','pwr')
todo <- pckgs[!is.element(pckgs, names(installed.packages()[,"Package"]))]
if(length(todo) > 0) install.packages(todo)  #,repos="http://cran.us.r-project.org")


# load in the packages
library(shiny)
library(ggplot2)  # ggplot2 -> plotting
library(dplyr)     # ddply -> data manipulation
library(pwr)

# server file
shinyServer(function(input,output,session){
	
	# calculate effect sizes per time point
	processChange <- reactive({
		# post hoc
		
		sldBeta <- as.numeric(as.character((input$sldPwr)))
		sldNtotal <- as.numeric(as.character((input$sldNtotal)))
		sldEff <- as.numeric(as.character((input$sldEff)))
		dbAlpha <- as.numeric(as.character(input$dbAlpha))
		dbPred <- as.numeric(as.character(input$dbPred))
		
		if(input$dbFree=="b"){
			sldBeta <- pwr.anova.test(k = dbPred, n = round(sldNtotal/dbPred), f = sldEff, sig.level = dbAlpha, power = )$power
		}
		# apriori
		if(input$dbFree=="es"){
			sldEff <- pwr.anova.test(k = dbPred, n = round(sldNtotal/dbPred), f = , sig.level = dbAlpha, power = sldBeta)$f
		}
		# sensitivity
		if(input$dbFree=="ss"){
			sldNtotal <- dbPred*pwr.anova.test(k = dbPred, n = , f = sldEff, sig.level = dbAlpha, power = sldBeta)$n		
		}
		dfNum <- dbPred - 1
		dfDen <- sldNtotal - dfNum - 1
		outCritF <- qf(1-dbAlpha,dfNum,dfDen)
		outNcp <- sldEff^2 * sldNtotal

		list(sldBeta=sldBeta,sldEff=sldEff,sldNtotal=sldNtotal,dbAlpha=dbAlpha,outCritF=outCritF,outNcp=outNcp,dfNum=dfNum,dfDen=dfDen,dbPred=dbPred)
	})

	
	# output ncp critical F ----------------------------------------------------
	output$txt.out.1 <- renderText({
		inx <- processChange()
		out <- ""
		out <- paste0(out,"<strong>power</strong> <b>",1-round(inx$sldBeta,4),"</b> or type II error (beta): ",round(inx$sldBeta,2),"<br>")
		out <- paste0(out,"total <strong>sample size</strong> (n): ",round(inx$sldNtotal),"<br>")
		out <- paste0(out,"<strong>effect size</strong> (f): ",round(inx$sldEff,2),"<br>")
		out <- paste0(out,"type I error (alpha): ",round(inx$dbAlpha,4),"<br>")
		out
	})
	# output ncp critical F ----------------------------------------------------
	output$txt.out.2 <- renderText({
		inx <- processChange()
		out <- ""
		out <- paste0(out,"<u>critical F value</u> with df ",round(inx$dfNum)," and ",round(inx$dfDen),": ",round(inx$outCritF,3),"<br>")
		out <- paste0(out,"<u>non-centrality parameter</u> (ncp): ",round(inx$outNcp,2),"<br>")
		out <- paste0(out,"ratio between / within variance ",round(inx$sldEff^2,3),"<br>")
		out
	})
	# total sample size
	output$sld.ntotal <- renderUI({
		inx <- processChange()
		txt <- "sample size:"
		if(input$dbFree=="ss") txt <- "sample size SELECTED"
		sldNtotal <- isolate(inx$sldNtotal) 
		if(all(inx$sldBeta==0 & inx$sldEff==.01 & inx$sldNtotal==3)) sldNtotal <- 190
		sliderInput("sldNtotal", txt, min = 3, max = 256, value = sldNtotal)
	})
	# effect size f
	output$sld.eff <- renderUI({
		inx <- processChange()
		txt <- "effect size:"
		if(input$dbFree=="es") txt <- "effect size SELECTED"
		sldEff <- isolate(inx$sldEff) 
		if(all(inx$sldBeta==0 & inx$sldEff==.01 & inx$sldNtotal==3)) sldEff <- .25
		sliderInput("sldEff", txt, min = 0.01, max = 2, value = sldEff, step=.001)
	})
	# type II error, beta
	output$sld.pwr <- renderUI({
		inx <- processChange()
		txt <- "power (type II error ~"
		if(input$dbFree=="b") txt <- "power SELECTED (type II error ~ "
		sldBeta <- isolate(inx$sldBeta)
		if(all(inx$sldBeta==0 & inx$sldEff==.01 & inx$sldNtotal==3)) sldBeta <- .8
		sliderInput("sldPwr", paste0(txt,round(1-sldBeta,2),")"), min = 0.05, max = 1, value = sldBeta,step=.001)
	})
	
	# ----- #
	
	# permanent plot, two rows of F distributions Ho and Ha 
	output$plotHoHa2 <- renderPlot({
		validate(
			need(input$dbFree!="NA", "")
		)
		plotOutput(getHoHa2(), height = 100, width = 450)
	})
	
	# remaining plots (possibly remove some of them, extend others)
	

	# power curve
	output$checkPlotPowercurve <- renderUI({
		checkboxInput("showPowerCurve","Power Curve", FALSE)
	})
	output$plotPowerCurve <- renderPlot({
		validate(
			need(input$dbFree!="NA", "first select free parameter")
		)
		plotOutput(getPowerCurve(), height = 100, width = 450)
	})
	
	# null distribution with rejection area
	output$checkPlotHoF <- renderUI({
		checkboxInput("showPlotHoF","F Ho + rejection area", FALSE)
	})
	output$plotHoF <- renderPlot({
		# if(input$showPlotHoF) 
		validate(
			need(input$dbFree!="NA", "")
		)
		plotOutput(getHoF(), height = 100, width = 450)
	})	
	
	# null and alternative distribution
	output$checkPlotHoHaF <- renderUI({
		validate(
			need(input$dbFree!="NA", "")
		)
		checkboxInput("showPlotHoHaF","F Ho + Ha", FALSE)
	})
	output$plotHoHaF <- renderPlot({
		# if(input$showPlotHoHaF) 
		validate(
			need(input$dbFree!="NA", "")
		)
		plotOutput(getHoHaF(), height = 100, width = 450)
	})

	# ----------------------------------- #
	
	# function to get power, given type I error, number of observations, and predictors and eta-squared as effect size
	fpi <- function(eta2,n,alpha,a){
		f.crit=qf(1-alpha,a-1,n-a)
		lambda=eta2/(1-eta2)*n
		fpi = 1-pf(f.crit,a-1,n-a,lambda)
		return(fpi)
	}
	f2eta2 <- function(f){
		f^2/(1+f^2)
	}
	eta2f <- function(eta2){
		sqrt(eta2/(1-eta2))
	}

	# F-null and alternative distributions on top of each other with rejection areas
	getHoHa2 <- function(){
		inx <- processChange()
		# F-null and alternative distributions - rows
		xmax=15 
		xmax.xx = 30
		xx=seq(0.01,xmax.xx,.001) 
		fxx=df(xx,inx$dfNum,inx$dfDen) 
		mnx=max(fxx)+.2
		par(mar=c(2.5,0,0,0))
		plot(xx,fxx,type="n",xlab="",ylab="",xlim=c(0,xmax),ylim=c(0,2*mnx),xaxt="n",yaxt="n",axes=FALSE)
		lines(xx,fxx,lwd=2) 
		cc=qf(1-inx$dbAlpha,inx$dfNum,inx$dfDen) 
		text((cc+1.5),.2,"reject H0",col="red")  
		text(1.3,.2,"do not reject H0") 
		xxsub=xx[xx>cc] 
		fxxsub=fxx[xx>cc]   
		polygon(x=c(cc,xxsub,rev(xxsub),cc),y=c(df(cc,inx$dfNum,inx$dfDen),fxxsub,0*fxxsub,0),col="red") 
		segments(cc,0,cc,.15) 
		segments(0,0,0,1.1) 
		segments(0,0,xmax,0)
		fxx2=df(xx,inx$dfNum,inx$dfDen,ncp=inx$outNcp) 
		lines(xx,fxx2+mnx,lwd=2) 
		segments(0,mnx,0,mnx+1.1) 
		segments(0,mnx,xmax,mnx) 
		segments(cc,0,cc,1.5*mnx) 
		xs=xx[xx>cc] 
		ys=fxx2[xx>cc] 
		polygon(x=c(cc,xs,rev(xs),cc),y=mnx+c(df(cc,inx$dfNum,inx$dfDen,inx$outNcp),ys,0*ys,0),col="red")
		text(cc,.6+mnx,"true state of nature",cex=1.5,pos=4) 
		text(cc,.6,"decision",cex=1.5,pos=4)
		text(cc-2.9,.3+mnx,substitute(italic(F)[list(dfn,dfd,ncp)],list(dfn=inx$dfNum,dfd=inx$dfDen,ncp=inx$outNcp)),cex=1.3,pos=4) 
		text(cc-2.9,.8,substitute(italic(F)[list(dfn,dfd,ncp)],list(dfn=inx$dfNum,dfd=inx$dfDen,ncp=0)),cex=1.3,pos=4) 
	}
	
	# depends on df num/den, alpha and critical F
	getHoF <- function(){
		inx <- processChange()
		dfNum <- inx$dfNum
		dfDen <- inx$dfDen
		outCritF <- inx$outCritF
		dbAlpha <- inx$dbAlpha
		# F-null distribution with area of rejection
		fVal=seq(0,6,.001)
		fDens=df(fVal,dfNum,dfDen)
		plot(fVal,fDens,type="n",xlab=expression(paste(italic(F)," value")),ylab="Density",xlim=c(0,5.5),ylim=c(0,1),xaxt="n",yaxt="n",axes=FALSE)
		axis(side=1,pos=0,at=c(0,1,2,4,5))
		axis(side=2,pos=0)
		lines(fVal,fDens,lwd=2)
		lines(c(outCritF,outCritF),c(-.05,.0),col="red",lwd=2)
		mtext(substitute(italic(F)[list(dfn,dfd)]^{cfd}==es,list(dfn=dfNum,dfd=dfDen,cfd=1-dbAlpha,es=round(outCritF,2))),side=1,adj=.6,padj=.5,col="red")
		text((outCritF+1),.2,"reject H0",col="red") 
		text(1.3,.2,"do not reject H0")
		fValReject=fVal[fVal>outCritF]
		fDensReject=fDens[fVal>outCritF]  
		polygon(x=c(outCritF,fValReject,rev(fValReject),outCritF),y=c(df(outCritF,dfNum,dfDen),fDensReject,0*fDensReject,0),col="red")
		segments(outCritF,0,outCritF,.15)
	}	
	getPowerCurve <- function(){
		# f = sqrt(eta2/(1-eta2))
		inx <- processChange()
		sldNtotal <- inx$sldNtotal
		dbAlpha <- inx$dbAlpha
		dbPred <- inx$dbPred
		sldBeta <- inx$sldBeta
		sldEff <- inx$sldEff
		outEta2 <- sldEff^2/(1+sldEff^2)

		# eta - probability curve
		cexlab=1.25 
		cexaxis=1.25 
		par(mgp=c(2.3,1,0)) 
		eta2seq=seq(0,.3,.001) 
		alpha=.05 
		pp=fpi(eta2=eta2seq,n=sldNtotal,alpha=dbAlpha,a=dbPred) 
		plot(eta2seq,pp,type="l",xlab=expression(eta^2),ylab=expression(pi),ylim=c(0,1),yaxt="n",axes=FALSE, cex.lab=cexlab,cex.axis=cexaxis,lwd=3) 
		pp0=fpi(eta2=outEta2,n=sldNtotal,alpha=dbAlpha,a=dbPred) 
		points(outEta2,pp0,cex=2,pch=18)
		axis(1,pos=0,cex.lab=cexlab,cex.axis=cexaxis) 
		axis(2,pos=0,at=c(0,dbAlpha,.2,.4,.6,.8,1),labels=c(0,sldBeta,.2,.4,.6,.8,1),cex.lab=cexlab,cex.axis=cexaxis) 
		abline(h=0.05,col="gray80",lwd=2) 
	}

	getHoHaF <- function(){
		# inx <- adjustInput()
		inx <- processChange()
		xx=seq(0,20,.001)   
		fxx=df(xx,inx$dfNum,inx$dfDen)
		plot(xx,fxx,type="n",xlab=expression(paste(italic(F)," value")),ylab="Density",xlim=c(0,20),ylim=c(0,1),xaxt="n",yaxt="n",axes=FALSE) 
		axis(side=1,pos=0)
		axis(side=2,pos=0) 
		lines(xx,fxx,lwd=2) 
		ff=c(.1,.25,.4) 
		R2=ff^2/(1+ff^2) 
		for (j in 1:3){ 
			f=ff[j] 
			fxx=df(xx,inx$dfNum,inx$dfDen,ncp=inx$outNcp)
			lines(xx,fxx,lwd=2,col=rgb(10+j*40,10+j*40,10+j*40,255,maxColorValue=255))   
		}
	}

})