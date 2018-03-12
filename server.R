
# install required packages not yet installed
pckgs <- c('shiny','reshape2','ggplot2','plyr','arm')
todo <- pckgs[!is.element(pckgs, names(installed.packages()[,"Package"]))]
if(length(todo) > 0) install.packages(todo)  #,repos="http://cran.us.r-project.org")

# load in the packages
library(shiny)
library(ggplot2)  # ggplot2 -> plotting
library(dplyr)     # ddply -> data manipulation


# server file
shinyServer(function(input,output,session){

	# calculate effect sizes per time point
	adjustInput <- reactive({
		dbFree <- input$dbFree
		sldNtotal <- as.numeric(as.character(input$sldNtotal))
		sldEff <- as.numeric(as.character(input$sldEff))
		# dbAlpha <- as.numeric(as.character(input$dbAlpha))
		# sldBeta <- as.numeric(as.character(input$sldBeta))
		sldBvar <-  as.numeric(as.character(input$sldBvar))
		sldWvar <-  as.numeric(as.character(input$sldWvar))
		sldNg1 <- as.numeric(as.character(input$sldNg1))
		sldNg2 <- as.numeric(as.character(input$sldNg2))
		sldNg3 <- as.numeric(as.character(input$sldNg3))
		sldAVg1 <- as.numeric(as.character(input$sldAVg1))
		sldAVg2 <- as.numeric(as.character(input$sldAVg2))
		sldAVg3 <- as.numeric(as.character(input$sldAVg3))
		sldSDg1 <- as.numeric(as.character(input$sldSDg1))
		sldSDg2 <- as.numeric(as.character(input$sldSDg2))
		sldSDg3 <- as.numeric(as.character(input$sldSDg3))

		outEta2 <- sldBvar / (sldBvar+sldWvar)
		sldEff <- sqrt(outEta2/(1-outEta2))
		nrPrds <- 3
		dfNum <- nrPrds - 1
		dfDen <- sldNtotal - dfNum - 1
		outCritF <- df(sldEff,dfNum,dfDen)
		outNcp <- sldEff^2 * sldNtotal
		# dbAlpha=dbAlpha,sldBeta=sldBeta,
		list(dbFree=dbFree,sldNtotal=sldNtotal,sldEff=sldEff,sldBvar,sldWvar,
		sldNg1=sldNg1,sldNg2=sldNg2,sldNg3=sldNg3,sldAVg1=sldAVg1,sldAVg2=sldAVg2,sldAVg3=sldAVg3,sldSDg1=sldSDg1,sldSDg2=sldSDg2,sldSDg3=sldSDg3,
		nrPrds=nrPrds,dfNum=dfNum,dfDen=dfDen,outNcp=outNcp,outEta2=outEta2)
	})
	# output ncp critical F ----------------------------------------------------
	output$txt.out <- renderText({
		inx <- adjustInput()
		out <- ""
		if(inx$dbFree=="ss") out <- paste0(out,"sample size:",inx$sldNtotal,"<br>")
		out <- paste0(out,"noncentrality parameter ~ ",round(input$sldEff^2*input$sldNtotal,2),"<br>")
		out <- paste0(out,"partial eta squared",inx$outEta2,"<br>")
		out <- paste0(out,"check alpha",input$dbAlpha,"<br>")
		out <- paste0(out,"check beta",input$dbBeta,"<br>")
		# out <- paste0(out,"check alpha",inx$dbAlpha,"<br>")
		out
	})
	# free parameter
	output$dd.free <- renderUI({
		selectInput("dbFree", "free parameter:", c("select one"="NA","sample size"="ss","effect size"="es","type I error"="a","type II error"="b"),selected="select one")
	})
	# total sample size
	output$sld.ntotal <- renderUI({
		sldNtotal <- as.numeric(as.character(isolate(input$sldNtotal)))
		# inx <- adjustInput()
		if(length(sldNtotal)==0) sldNtotal <- 32
		# sliderInput("sldNtotal", "sample size:", min = -1, max = 256, value = sldNtotal)
		if(input$dbFree!="ss"){
			# xNtotal <- ifelse(input$sldNtotal<0,128,input$sldNtotal)
			sliderInput("sldNtotal", "sample size:", min = -1, max = 256, value = sldNtotal)
		}
	})
	# effect size f
	output$sld.eff <- renderUI({
		# inx <- adjustInput()
		sldEff <- as.numeric(as.character(isolate(input$sldEff)))
		if(length(sldEff)==0) sldEff <- .25
		sldWvar <- input$sldWvar
		sldBvar <- input$sldBvar
		outEta2 <- sldBvar / (sldBvar+sldWvar)
		sldEff <- sqrt(outEta2/(1-outEta2))

		# sliderInput("sldEff", "effect size:", min = 0, max = 16, value = .25,step=.001)
		if(input$dbFree!="es"){
			sliderInput("sldEff", "effect size:", min = 0, max = 2, value = sldEff,step=.001)
		}
	})
	# type I error, alpha
	output$db.alpha <- renderUI({
		# inx <- adjustInput()
		# input$dbAlpha
		dbAlpha <- as.numeric(as.character(isolate(input$dbAlpha)))
		# if(input$dbFree!="a"){
			# selectInput("dbAlpha", "type I error:", c(".001" = ".001",".01" = ".01",".05" = ".05",".1" = ".1"),selected=dbAlpha)
		# }
		updateSelectInput("dbAlpha", "type I error:", c(".001" = ".001",".01" = ".01",".05" = ".05",".1" = ".1"),selected=inx$dbAlpha)
	})
	# type II error, beta
	output$sld.beta <- renderUI({
		inx <- adjustInput()
		sldBeta <- as.numeric(as.character(input$sldBeta))
		if(length(sldBeta)==0) sldBeta <- .2
		if(input$dbFree=="b"){
			lambda=sqrt(inx$outEta2/(1-inx$outEta2))*inx$sldNtotal
			sldBeta = 1-pf(inx$outCritF,3-1,inx$sldNtotal-3,lambda)
		}
		
		# inx <- adjustInput()
		sliderInput("sldBeta", paste0("type II error (power~",round(1-sldBeta,2),")"), min = 0, max = 1, value = sldBeta,step=.001)
		# selectInput("sldBeta", "type II error:", c(".2" = ".2",".15" = ".15",".1" = ".1",".05" = ".05"))
		# if(inx$dbFree!="b"){
			# selectInput("sldBeta", "type II error:", c(".2" = ".2",".15" = ".15",".1" = ".1",".05" = ".05"))
		# }
	})
	# effect size variance, between
	output$sld.bvar <- renderUI({
		# inx <- adjustInput()
		sldBvar <- as.numeric(as.character(isolate(input$sldBvar)))
		if(length(sldBvar)==0) sldBvar <- 1
		sliderInput("sldBvar", "between variance:", min = -1, max = 32, value = sldBvar)
	})
	# effect size variance, within
	output$sld.wvar <- renderUI({
		# inx <- adjustInput()
		sldWvar <- as.numeric(as.character(isolate(input$sldWvar)))
		if(length(sldWvar)==0) sldWvar <- 6
		sliderInput("sldWvar", "within variance:", min = 0, max = 32, value = 6)
	})



	# ----------------------------------- #

	# sample size groups
	output$sld.ngs <- renderUI({
		inx <- adjustInput()
		sliderInput("sldNgs", "all groups:", min = 0, max = 100, value = 32)
	})
	# sample size group 1
	output$sld.ng1 <- renderUI({
		inx <- adjustInput()
		sliderInput("sldNg1", "group 1:", min = 0, max = 100, value = 32)
	})
	# sample size group 2
	output$sld.ng2 <- renderUI({
		inx <- adjustInput()
		sliderInput("sldNg2", "group 2:", min = 0, max = 100, value = 32)
	})
	# sample size group 3
	output$sld.ng3 <- renderUI({
		inx <- adjustInput()
		sliderInput("sldNg3", "group 3:", min = 0, max = 100, value = 32)
	})
	# sample size group 1
	output$sld.avg1 <- renderUI({
		inx <- adjustInput()
		sliderInput("sldAVg1", "group 1:", min = -100, max = 100, value = 0)
	})
	# sample size group 2
	output$sld.avg2 <- renderUI({
		inx <- adjustInput()
		sliderInput("sldAVg2", "group 2:", min = -100, max = 100, value = 2)
	})
	# sample size group 3
	output$sld.avg3 <- renderUI({
		inx <- adjustInput()
		sliderInput("sldAVg3", "group 3:", min = -100, max = 100, value = -2)
	})
	# sample size group 1
	output$sld.sdg1 <- renderUI({
		inx <- adjustInput()
		sliderInput("sldSDg1", "group 1:", min = 0, max = 100, value = 4)
	})
	# sample size group 2
	output$sld.sdg2 <- renderUI({
		inx <- adjustInput()
		sliderInput("sldSDg2", "group 2:", min = 0, max = 100, value = 4)
	})
	# sample size group 3
	output$sld.sdg3 <- renderUI({
		inx <- adjustInput()
		sliderInput("sldSDg3", "group 3:", min = 0, max = 100, value = 4)
	})
	
	# ----------------------------------- #

	output$plot2 <- renderPlot({
		plotOutput(getPowerCurve(), height = 100, width = 450)
	}) 
	output$plotHoHa <- renderPlot({
		plotOutput(createHoHa(), height = 300, width = 450)
	}) 


	# ----------------------------------- #
	
	# function to get power, given type I error, number of observations, and predictors and eta-squared as effect size
	fpi<-function(eta2,n,alpha,a){
	  f.crit=qf(1-alpha,a-1,n-a)
	  lambda=eta2/(1-eta2)*n
	  fpi = 1-pf(f.crit,a-1,n-a,lambda)
	  return(fpi)}

	getHoF <- function(){
		inx <- adjustInput()
		n.sl=inx$sst
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
		mtext(expression(italic(F)[list(2,102)]^{0.95}==3.09),side=1,adj=.6,padj=.5,col="red")
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
		inx <- adjustInput()
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
		inx <- adjustInput()
		cexlab=1.25 
		cexaxis=1.25 
		par(mgp=c(2.3,1,0)) 
		eta2seq=seq(0,.3,.001) 
		alpha=.05 
		pp=fpi(eta2=eta2seq,n=inx$sldNtotal,alpha=inx$dbAlpha,a=2) 
		plot(eta2seq,pp,type="l",xlab=expression(eta^2),ylab=expression(pi),ylim=c(0,1),yaxt="n",axes=FALSE, cex.lab=cexlab,cex.axis=cexaxis,lwd=3) 
		axis(1,pos=0,cex.lab=cexlab,cex.axis=cexaxis) 
		axis(2,pos=0,at=c(0,inx$dbAlpha,.2,.4,.6,.8,1),labels=c(0,inx$sldBeta,.2,.4,.6,.8,1),cex.lab=cexlab,cex.axis=cexaxis) 
		abline(h=0.05,col="gray80",lwd=2) 
	}

	# F-null and alternative distributions on top of each other with rejection areas
	createHoHa <- function(){
		inx <- adjustInput()
		dbAlpha <- as.numeric(as.character((input$dbAlpha)))
		xmax=15 
		xx=seq(0,xmax,.001) 
		fxx=df(xx,inx$dfNum,inx$dfDen) 
		mnx=max(fxx)+.2
		par(mar=c(2.5,0,0,0))
		plot(xx,fxx,type="n",xlab="",ylab="",xlim=c(0,xmax),ylim=c(0,2*mnx),xaxt="n",yaxt="n",axes=FALSE)
		lines(xx,fxx,lwd=2) 
		cc=qf(1-dbAlpha,inx$dfNum,inx$dfDen) 
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
		text(cc-2.9,.3+mnx,expression(italic(F)[list(2,102,6.7)]),cex=1.3,pos=4) 
		text(cc-2.9,.8,expression(italic(F)[list(2,102)]),cex=1.3,pos=4) 
	}

})