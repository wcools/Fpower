
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
	adjustInput <- reactive({
		dbFree <- input$dbFree
		sldNtotal <- as.numeric(as.character(input$sldNtotal))
		sldEff <- as.numeric(as.character(input$sldEff))
		# dbAlpha <- as.numeric(as.character(input$dbAlpha))
		# sldBeta <- as.numeric(as.character(input$sldBeta))
		# sldBvar <-  as.numeric(as.character(input$sldBvar))
		# sldWvar <-  as.numeric(as.character(input$sldWvar))
		sldNg1 <- as.numeric(as.character(input$sldNg1))
		sldNg2 <- as.numeric(as.character(input$sldNg2))
		sldNg3 <- as.numeric(as.character(input$sldNg3))
		sldAVg1 <- as.numeric(as.character(input$sldAVg1))
		sldAVg2 <- as.numeric(as.character(input$sldAVg2))
		sldAVg3 <- as.numeric(as.character(input$sldAVg3))
		sldSDg1 <- as.numeric(as.character(input$sldSDg1))
		sldSDg2 <- as.numeric(as.character(input$sldSDg2))
		sldSDg3 <- as.numeric(as.character(input$sldSDg3))

		# outEta2 <- sldBvar / (sldBvar+sldWvar)
		# sldEff <- sqrt(outEta2/(1-outEta2))
		nrPrds <- 3
		dfNum <- nrPrds - 1
		dfDen <- sldNtotal - dfNum - 1
		outCritF <- df(sldEff,dfNum,dfDen)
		outNcp <- sldEff^2 * sldNtotal
		# dbAlpha=dbAlpha,sldBeta=sldBeta,
		list(dbFree=dbFree,sldNtotal=sldNtotal,sldEff=sldEff,sldBvar=sldBvar,sldWvar=sldWvar,
		sldNg1=sldNg1,sldNg2=sldNg2,sldNg3=sldNg3,sldAVg1=sldAVg1,sldAVg2=sldAVg2,sldAVg3=sldAVg3,sldSDg1=sldSDg1,sldSDg2=sldSDg2,sldSDg3=sldSDg3,
		nrPrds=nrPrds,dfNum=dfNum,dfDen=dfDen,outNcp=outNcp,outEta2=outEta2)
	})

	
	# calculate effect sizes per time point
	processChange <- reactive({
		# post hoc
		sldBeta <- as.numeric(as.character(input$sldBeta))
		sldNtotal <- as.numeric(as.character(input$sldNtotal))
		sldEff <- as.numeric(as.character(input$sldEff))
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
		outCritF <- df(sldEff,dfNum,dfDen)
		outNcp <- sldEff^2 * sldNtotal
		list(sldBeta=sldBeta,sldEff=sldEff,sldNtotal=sldNtotal,dbAlpha=dbAlpha,outCritF=outCritF,outNcp=outNcp,dfNum=dfNum,dfDen=dfDen,dbPred=dbPred)
	})

	
	# output ncp critical F ----------------------------------------------------
	output$txt.out <- renderText({
		inx <- processChange()
		out <- ""
		out <- paste0(out,"total sample size (n): ",inx$sldNtotal,"<br>")
		out <- paste0(out,"type II error (beta): ",inx$sldBeta," or power ",1-inx$sldBeta,"<br>")
		out <- paste0(out,"effect size f: ",inx$sldEff,"<br>")
		out <- paste0(out,"non-centrality parameter (ncp): ",round(inx$outNcp,2),"<br>")
		out <- paste0(out,"type I error (alpha): ",inx$dbAlpha,"<br>")
		out <- paste0(out,"critical F value with df ",inx$dfNum," and ",inx$dfDen,": ",round(inx$outCritF,4),"<br>")
		out <- paste0(out,"ratio between / within variance ",round(inx$sldEff^2,3),"<br>")
		out <- paste0(out,"number of predictors ",inx$dbPred,"<br>")
		out
	})
	# free parameter
	# output$dd.free <- renderUI({
		# selectInput("dbFree", "free parameter:", c("select one"="NA","sample size"="ss","effect size"="es","type II error"="b"),selected="select one")
	# })
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
	# type I error, alpha
	# output$db.alpha <- renderUI({
		# dbAlpha <- as.numeric(as.character(isolate(input$dbAlpha)))
		# updateSelectInput("dbAlpha", "type I error:", c(".001" = ".001",".01" = ".01",".05" = ".05",".1" = ".1"),selected=inx$dbAlpha)
	# })
	# type II error, beta
	output$sld.beta <- renderUI({
		inx <- processChange()
		sldBeta <- inx$sldBeta #as.numeric(as.character(inx$sldBeta))
		if(length(inx$sldBeta)==0) sldBeta <- 0
		sliderInput("sldBeta", paste0("type II error (power~",round(1-sldBeta,2),")"), min = 0, max = 1, value = sldBeta,step=.001)
	})
	# effect size variance, between
	# output$sld.bvar <- renderUI({
		# inx <- processChange()
		# sldBvar <- inx$sldEff^2# as.numeric(as.character(isolate(input$sldBvar)))
		# sliderInput("sldBvar", "between/within variance:", min = -1, max = 32, value = sldBvar)
	# })
	# effect size variance, within
	# output$sld.wvar <- renderUI({
		# inx <- processChange()
		# sldWvar <- inx$sldEff^2 #as.numeric(as.character(isolate(input$sldWvar)))
		# sliderInput("sldWvar", "within variance:", min = 0, max = 32, value = 6)
	# })



	
	# ----------------------------------- #

	output$plot2 <- renderPlot({
		plotOutput(getPowerCurve(), height = 100, width = 450)
	}) 
	output$plotHoHa <- renderPlot({
		plotOutput(createHoHa(), height = 300, width = 450)
	}) 
	output$plotGetHoF <- renderPlot({
		plotOutput(getHoF(), height = 300, width = 450)
	}) 
	output$plotHoHaF <- renderPlot({
		plotOutput(getHoHaF(), height = 300, width = 450)
	}) 
	output$plotPowerCurve <- renderPlot({
		plotOutput(getPowerCurve(), height = 300, width = 450)
	}) 


	# ----------------------------------- #
	
	# function to get power, given type I error, number of observations, and predictors and eta-squared as effect size
	fpi<-function(eta2,n,alpha,a){
	  f.crit=qf(1-alpha,a-1,n-a)
	  lambda=eta2/(1-eta2)*n
	  fpi = 1-pf(f.crit,a-1,n-a,lambda)
	  return(fpi)}

	getHoF <- function(){
		# inx <- adjustInput()
		inx <- processChange()
		n.sl=inx$sldNtotal
		cf.sl=qf(1-inx$dbAlpha,inx$dfNum,inx$dfDen)
		eta2.ex=inx$sldEff
		f.ex=sqrt(eta2.ex/(1-eta2.ex))
		lambda.ex=f.ex^2*n.sl

		xx=seq(0,6,.001)
		fxx=df(xx,inx$dfNum,inx$dfDen)
		plot(xx,fxx,type="n",xlab=expression(paste(italic(F)," value")),ylab="Density",xlim=c(0,5.5),ylim=c(0,1),xaxt="n",yaxt="n",axes=FALSE)
		axis(side=1,pos=0,at=c(0,1,2,4,5))
		axis(side=2,pos=0)
		lines(xx,fxx,lwd=2)
		lines(c(inx$outCritF,inx$outCritF),c(-.05,.0),col="red",lwd=2)
		mtext(expression(italic(F)[list(inx$dfNum,inx$dfNum)]^{0.95}==3.09),side=1,adj=.6,padj=.5,col="red")
		#text(4.5,.1,"Pr(reject H0|H0)=0.05")
		text((inx$outCritF+1),.2,"reject H0",col="red") 
		text(1.3,.2,"do not reject H0")
		xxsub=xx[xx>inx$outCritF]
		fxxsub=fxx[xx>inx$outCritF]  
		polygon(x=c(inx$outCritF,xxsub,rev(xxsub),cf.sl),y=c(df(inx$outCritF,inx$dfNum,inx$dfDen),fxxsub,0*fxxsub,0),col="red")
		# polygon(x=c(inx$crf,xxsub,rev(xxsub),inx$sst),y=c(df(inx$crf,inx$dfn,inx$dfd),fxxsub,0*fxxsub,0),col="red")
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

	# F-null and alternative distributions on top of each other with rejection areas
	createHoHa <- function(){
		inx <- processChange()
		xmax=15 
		xx=seq(0,xmax,.001)
		dfNum <- inx$dfNum
		dfDen <- inx$dfDen
		outNcp <- inx$outNcp
		fxx=df(xx,dfNum,dfDen) 
		mnx=max(fxx)+.2
		par(mar=c(2.5,0,0,0))
		plot(xx,fxx,type="n",xlab="",ylab="",xlim=c(0,xmax),ylim=c(0,2*mnx),xaxt="n",yaxt="n",axes=FALSE)
		lines(xx,fxx,lwd=2) 
		cc=qf(1-inx$dbAlpha,dfNum,dfDen) 
		text((cc+1.5),.2,"reject H0",col="red")  
		text(1.3,.2,"do not reject H0") 
		xxsub=xx[xx>cc] 
		fxxsub=fxx[xx>cc]   
		polygon(x=c(cc,xxsub,rev(xxsub),cc),y=c(df(cc,dfNum,dfDen),fxxsub,0*fxxsub,0),col="red") 
		segments(cc,0,cc,.15) 
		segments(0,0,0,1.1) 
		segments(0,0,xmax,0)
		fxx2=df(xx,dfNum,dfDen,ncp=outNcp) 
		lines(xx,fxx2+mnx,lwd=2) 
		segments(0,mnx,0,mnx+1.1) 
		segments(0,mnx,xmax,mnx) 
		segments(cc,0,cc,1.5*mnx) 
		xs=xx[xx>cc] 
		ys=fxx2[xx>cc] 
		polygon(x=c(xs,cc),y=c(ys,0)+mnx,col="red") 
		text(cc,.6+mnx,"true state of nature",cex=1.5,pos=4) 
		text(cc,.6,"decision",cex=1.5,pos=4)
		text(cc-2.9,.3+mnx,substitute(italic(F)[list(.num,.den,.ncp)],list(.num=dfNum,.den=dfDen,.ncp=outNcp)),cex=1.3,pos=4) 
		# text(cc-2.9,.8,expression(italic(F)[list(dfNum,dfDen)]),cex=1.3,pos=4) 
		text(cc-2.9,.8,substitute(italic(F)[list(.num,.den)],list(.num=dfNum,.den=dfDen)),cex=1.3,pos=4) 
	}

})