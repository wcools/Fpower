
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
		dbAlpha <- as.numeric(as.character(input$dbAlpha))
		dbBeta <- as.numeric(as.character(input$dbBeta))
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

		list(sldNtotal=sldNtotal,sldEff=sldEff,dbAlpha=dbAlpha,dbBeta=dbBeta,sldBvar,sldWvar,
		sldNg1=sldNg1,sldNg2=sldNg2,sldNg3=sldNg3,sldAVg1=sldAVg1,sldAVg2=sldAVg2,sldAVg3=sldAVg3,sldSDg1=sldSDg1,sldSDg2=sldSDg2,sldSDg3=sldSDg3)
	})

	# free parameter
	output$dd.free <- renderUI({
		selectInput("dbFree", "free parameter:", c("sample size","effect size","type I error","type II error"),selected="sample size")
	})
	# total sample size
	output$sld.ntotal <- renderUI({
		inx <- adjustInput()
		xNtotal <- ifelse(inx$sldNtotal<0,128,inx$sldNtotal)
		sliderInput("sldNtotal", "sample size:", min = -1, max = 256, value = xNtotal)
	})
	# effect size f
	output$sld.eff <- renderUI({
		inx <- adjustInput()
		sliderInput("sldEff", "effect size:", min = 0, max = 16, value = .25,step=.001)
	})
	# type I error, alpha
	output$db.alpha <- renderUI({
		selectInput("dbAlpha", "type I error:", c(".001" = ".001",".01" = ".01",".05" = ".05",".1" = ".1"))
	})
	# type II error, beta
	output$db.beta <- renderUI({
		selectInput("dbBeta", "type II error:", c(".2" = ".2",".15" = ".15",".1" = ".1",".05" = ".05"))
	})
	# effect size variance, between
	output$sld.bvar <- renderUI({
		inx <- adjustInput()
		sliderInput("sldBvar", "between variance:", min = 0, max = 32, value = 1)
	})
	# effect size variance, within
	output$sld.wvar <- renderUI({
		inx <- adjustInput()
		sliderInput("sldWvar", "within variance:", min = 0, max = 32, value = 6)
	})
	# output ncp critical F
	output$txt.out <- renderText({
		inx <- adjustInput()
		paste0("noncentrality parameter",inx$sldNtotal)
	})
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

	# function to get power, given type I error, number of observations, and predictors and eta-squared as effect size
	fpi<-function(eta2,n,alpha,a){
	  f.crit=qf(1-alpha,a-1,n-a)
	  lambda=eta2/(1-eta2)*n
	  fpi = 1-pf(f.crit,a-1,n-a,lambda)
	  return(fpi)}

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
		axis(2,pos=0,at=c(0,inx$dbAlpha,.2,.4,.6,.8,1),labels=c(0,inx$dbBeta,.2,.4,.6,.8,1),cex.lab=cexlab,cex.axis=cexaxis) 
		abline(h=0.05,col="gray80",lwd=2) 
	}

	# F-null and alternative distributions on top of each other with rejection areas
	createPlot <- function(){
		inx <- adjustInput()
		xmax=15 
		xx=seq(0,xmax,.001) 
		fxx=df(xx,as.numeric(as.character(inx$sldNtotal)) - 3 - 1, 3 - 1) 
		mnx=max(fxx)+.2
		par(mar=c(2.5,0,0,0))
		outpu2 <- plot(xx,fxx,type="n",xlab="",ylab="",xlim=c(0,xmax),ylim=c(0,2*mnx),xaxt="n",yaxt="n",axes=FALSE)
		lines(xx,fxx,lwd=2) 
		cc=qf(1-as.numeric(as.character(inx$dbAlpha)),as.numeric(as.character(inx$sldNtotal)) - 3 - 1, 3 - 1) 
		text((cc+1.5),.2,"reject H0",col="red")  
		text(1.3,.2,"do not reject H0") 
		xxsub=xx[xx>cc] 
		fxxsub=fxx[xx>cc]   
		polygon(x=c(cc,xxsub,rev(xxsub),cc),y=c(df(cc,as.numeric(as.character(inx$sldNtotal)) - 3 - 1, 3 - 1),fxxsub,0*fxxsub,0),col="red") 
		segments(cc,0,cc,.15) 
		segments(0,0,0,1.1) 
		segments(0,0,xmax,0)
		fxx2=df(xx,as.numeric(as.character(inx$sldNtotal)) - 3 - 1, 3 - 1,ncp=10) 
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


	output$plot2 <- renderPlot({
		plotOutput(getPowerCurve(), height = 100, width = 450)
	}) 
	output$plot1 <- renderPlot({
		plotOutput(createPlot(), height = 300, width = 450)
	}) 

})