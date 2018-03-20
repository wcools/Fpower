
# install required packages not yet installed
pckgs <- c('shiny','reshape2','ggplot2','plyr','arm')
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
		sldBeta <- as.numeric(as.character(isolate(input$sldBeta)))
		sldNtotal <- as.numeric(as.character(isolate(input$sldNtotal)))
		sldEff <- as.numeric(as.character(isolate(input$sldEff)))
		# what is happening here ???
		if(length(input$sldBeta) == 1){
			sldNtotal <- as.numeric(as.character(input$sldNtotal))
			sldEff <- as.numeric(as.character(input$sldEff))		
		}
		if(length(input$sldNtotal) == 1){
			sldBeta <- as.numeric(as.character(input$sldBeta))
			sldEff <- as.numeric(as.character(input$sldEff))		
		}
		if(length(input$sldEff) == 1){
			sldBeta <- as.numeric(as.character(input$sldBeta))
			sldNtotal <- as.numeric(as.character(input$sldNtotal))
		}
		dbAlpha <- as.numeric(as.character(input$dbAlpha))
		dbPred <- as.numeric(as.character(input$dbPred))

		if(input$dbFree=="b"){
			sldBeta <- 1-pwr.anova.test(k = 3, n = sldNtotal, f = sldEff, sig.level = dbAlpha, power = )$power
		}
		# apriori
		if(input$dbFree=="es"){
			sldEff <- pwr.anova.test(k = 3, n = sldNtotal, f = , sig.level = dbAlpha, power = 1-sldBeta)$f
		}
		# sensitivity
		if(input$dbFree=="ss"){
			sldNtotal <- pwr.anova.test(k = 3, n = , f = sldEff, sig.level = dbAlpha, power = 1-sldBeta)$n		
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
		out <- paste0(out,"number of predictors ",inx$dbPred,"<br>")
		out <- paste0(out,"<u>critical F value</u> with df ",inx$dfNum," and ",inx$dfDen,": ",round(inx$outCritF,4),"<br>")
		out <- paste0(out,"<u>non-centrality parameter</u> (ncp): ",round(inx$outNcp,2),"<br>")
		out <- paste0(out,"ratio between / within variance ",round(inx$sldEff^2,4),"<br>")
		out
	})
	# total sample size
	output$sld.ntotal <- renderUI({
		inx <- processChange()
		sldNtotal <- isolate(inx$sldNtotal) #as.numeric(as.character(isolate(input$sldNtotal)))
		if(length(inx$sldNtotal)==0) sldNtotal <- 3
		sliderInput("sldNtotal", "sample size:", min = 3, max = 256, value = sldNtotal)
	})
	# effect size f
	output$sld.eff <- renderUI({
		inx <- processChange()
		sldEff <- isolate(inx$sldEff) #as.numeric(as.character(isolate(input$sldEff)))
		if(length(inx$sldEff)==0) sldEff <- 0.01
		sliderInput("sldEff", "effect size:", min = 0.01, max = 2, value = sldEff, step=.001)
	})
	# type II error, beta
	output$sld.beta <- renderUI({
		inx <- processChange()
		sldBeta <- inx$sldBeta #as.numeric(as.character(inx$sldBeta))
		if(length(inx$sldBeta)==0) sldBeta <- 0
		sliderInput("sldBeta", paste0("type II error (power~",round(1-sldBeta,2),")"), min = 0, max = 1, value = sldBeta,step=.001)
	})
	
	# ----- #
	
	# permanent plot, two rows of F distributions Ho and Ha 
	output$plotHoHa2 <- renderPlot({
		plotOutput(getHoHa2(), height = 100, width = 450)
	})
	
	# remaining plots (possibly remove some of them, extend others)
	

	# power curve
	output$checkPlotPowercurve <- renderUI({
		checkboxInput("showPowerCurve","Power Curve", FALSE)
	})
	output$plotPowerCurve <- renderPlot({
		if(input$showPowerCurve) plotOutput(getPowerCurve(), height = 100, width = 450)
	})
	
	# null distribution with rejection area
	output$checkPlotHoF <- renderUI({
		checkboxInput("showPlotHoF","F Ho + rejection area", FALSE)
	})
	output$plotHoF <- renderPlot({
		if(input$showPlotHoF) plotOutput(getHoF(), height = 100, width = 450)
	})	
	
	# null and alternative distribution
	output$checkPlotHoHaF <- renderUI({
		checkboxInput("showPlotHoHaF","F Ho + Ha", FALSE)
	})
	output$plotHoHaF <- renderPlot({
		if(input$showPlotHoHaF) plotOutput(getHoHaF(), height = 100, width = 450)
	})

	# ----------------------------------- #
	
	# function to get power, given type I error, number of observations, and predictors and eta-squared as effect size
	fpi <- function(eta2,n,alpha,a){
		f.crit=qf(1-alpha,a-1,n-a)
		lambda=eta2/(1-eta2)*n
		fpi = 1-pf(f.crit,a-1,n-a,lambda)
		return(fpi)
	}

	# F-null and alternative distributions on top of each other with rejection areas
	getHoHa2 <- function(){
		inx <- processChange()
		# F-null and alternative distributions - rows
		xmax=15 
		xx=seq(0.01,xmax,.001) 
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
		polygon(x=c(xs,cc),y=c(ys,0)+mnx,col="red") 
		text(cc,.6+mnx,"true state of nature",cex=1.5,pos=4) 
		text(cc,.6,"decision",cex=1.5,pos=4)
		text(cc-2.9,.3+mnx,substitute(italic(F)[list(dfn,dfd,ncp)],list(dfn=inx$dfNum,dfd=inx$dfDen,ncp=inx$outNcp)),cex=1.3,pos=4) 
		text(cc-2.9,.8,substitute(italic(F)[list(dfn,dfd,ncp)],list(dfn=inx$dfNum,dfd=inx$dfDen,ncp=0)),cex=1.3,pos=4) 	
	}
	
	# list(sldBeta=sldBeta,sldEff=sldEff,sldNtotal=sldNtotal,dbAlpha=dbAlpha,outCritF=outCritF,outNcp=outNcp,dfNum=dfNum,dfDen=dfDen,dbPred=dbPred)


	getHoF <- function(){
		inx <- processChange()
		# F-null distribution with area of rejection
		xx=seq(0,6,.001)
		fxx=df(xx,inx$dfNum,inx$dfDen)
		plot(xx,fxx,type="n",xlab=expression(paste(italic(F)," value")),ylab="Density",xlim=c(0,5.5),ylim=c(0,1),xaxt="n",yaxt="n",axes=FALSE)
		axis(side=1,pos=0,at=c(0,1,2,4,5))
		axis(side=2,pos=0)
		lines(xx,fxx,lwd=2)
		lines(c(inx$outCritF,inx$outCritF),c(-.05,.0),col="red",lwd=2)
		mtext(substitute(italic(F)[list(dfn,dfd)]^{cfd}==es,list(dfn=inx$dfNum,dfd=inx$dfDen,cfd=1-inx$dbAlpha,es=round(inx$outCritF,2))),side=1,adj=.6,padj=.5,col="red")
		text((inx$outCritF+1),.2,"reject H0",col="red") 
		text(1.3,.2,"do not reject H0")
		xxsub=xx[xx>inx$outCritF]
		fxxsub=fxx[xx>inx$outCritF]  
		polygon(x=c(inx$outCritF,xxsub,rev(xxsub),inx$outCritF),y=c(df(inx$outCritF,inx$dfNum,inx$dfDen),fxxsub,0*fxxsub,0),col="red")
		segments(inx$outCritF,0,inx$outCritF,.15)
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
	getPowerCurve <- function(){
		# inx <- adjustInput()
		inx <- processChange()
		cexlab=1.25 
		cexaxis=1.25 
		par(mgp=c(2.3,1,0)) 
		eta2seq=seq(0,.3,.001) 
		alpha=.05 
		pp=fpi(eta2=eta2seq,n=inx$sldNtotal,alpha=inx$dbAlpha,a=inx$dbPred) 
		plot(eta2seq,pp,type="l",xlab=expression(eta^2),ylab=expression(pi),ylim=c(0,1),yaxt="n",axes=FALSE, cex.lab=cexlab,cex.axis=cexaxis,lwd=3) 
		axis(1,pos=0,cex.lab=cexlab,cex.axis=cexaxis) 
		axis(2,pos=0,at=c(0,inx$dbAlpha,.2,.4,.6,.8,1),labels=c(0,inx$sldBeta,.2,.4,.6,.8,1),cex.lab=cexlab,cex.axis=cexaxis) 
		abline(h=0.05,col="gray80",lwd=2) 
	}

})