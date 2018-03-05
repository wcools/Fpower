
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


# function to get power, given type I error, number of observations, and predictors and eta-squared as effect size
fpi<-function(eta2,n,alpha,a){
  f.crit=qf(1-alpha,a-1,n-a)
  lambda=eta2/(1-eta2)*n
  fpi = 1-pf(f.crit,a-1,n-a,lambda)
  return(fpi)}

getPlot1 <- function(){
		cexlab=1.25 
		cexaxis=1.25 
		par(mgp=c(2.3,1,0)) 
		eta2seq=seq(0,.3,.001) 
		alpha=.05 
		pp=fpi(eta2=eta2seq,n=as.numeric(as.character(input$sst)),alpha=as.numeric(as.character(input$e1)),a=2) 
		axis(1,pos=0,cex.lab=cexlab,cex.axis=cexaxis) 
		axis(2,pos=0,at=c(0,input$e1,.2,.4,.6,.8,1),labels=c(0,input$e1,.2,.4,.6,.8,1),cex.lab=cexlab,cex.axis=cexaxis) 
		abline(h=0.05,col="gray80",lwd=2) 
		plot(eta2seq,pp,type="l",xlab=expression(eta^2),ylab=expression(pi),ylim=c(0,1),yaxt="n",axes=FALSE, cex.lab=cexlab,cex.axis=cexaxis,lwd=3) 
}

# plot power for range of eta_squared values

	output$outpars <- renderPrint({
	out <- paste0("test\nagain")
	cat(out)
	})
	output$plot2 <- renderPlot({
		plotOutput(getPlot1(), height = 300, width = 450)
	}) 
	output$plot1 <- renderPlot({
		# F-null and alternative distributions on top of each other with rejection areas
		createPlot <- function(){
		xmax=15 
		xx=seq(0,xmax,.001) 
		fxx=df(xx,input$sst - 3 - 1, 3 - 1) 
		mnx=max(fxx)+.2
		par(mar=c(2.5,0,0,0))
		outpu2 <- plot(xx,fxx,type="n",xlab="",ylab="",xlim=c(0,xmax),ylim=c(0,2*mnx),xaxt="n",yaxt="n",axes=FALSE)
		lines(xx,fxx,lwd=2) 
		cc=qf(1-input$e1,input$sst - 3 - 1, 3 - 1) 
		text((cc+1.5),.2,"reject H0",col="red")  
		text(1.3,.2,"do not reject H0") 
		xxsub=xx[xx>cc] 
		fxxsub=fxx[xx>cc]   
		polygon(x=c(cc,xxsub,rev(xxsub),cc),y=c(df(cc,input$sst - 3 - 1, 3 - 1),fxxsub,0*fxxsub,0),col="red") 
		segments(cc,0,cc,.15) 
		segments(0,0,0,1.1) 
		segments(0,0,xmax,0)
		fxx2=df(xx,input$sst - 3 - 1, 3 - 1,ncp=10) 
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
		plotOutput(createPlot(), height = 300, width = 450)
	}) 

})