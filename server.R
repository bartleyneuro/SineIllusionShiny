library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(scrapeR)
library(shinyIncubator)

source("./functions.R")

#' Look at https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/THb1Ql5E20s for shiny server user-level data collection

shinyServer(function(input, output, clientData) {
  suppressMessages(library(ggplot2))
  
  userResults <- data.frame()
  
  output$illusion <- renderPlot({
    f <- function(x) 2*sin(x)
    fprime <- function(x) 2*cos(x)
    f2prime <- function(x) -2*sin(x)
    dframe <- createSine(n=40+2, len=1, f, fprime, f2prime, a=-pi, b=pi)[c(2:(41)),]
    
    trials <- expand.grid(w=c(0, .5, 1), type=c("x", "y"))
    
    minor.axis.correction <- correctx(seq(-pi, pi, pi/8), fprime, w=input$weight)
    dframeAdj <- cbind(adjx(dframe, fprime=fprime, w=input$weight), adj=paste("X corrected, weight =", input$weight))
#     dframe$type <- "Control"
#     dframeAdj$type <- "Adjusted"
#     dframe.all <- rbind.fill(dframe, dframeAdj)
#     dframe.all$type <- factor(dframe.all$type, levels=c("Control", "Adjusted"))
    dframe.all <- dframeAdj
    p1 <- qplot(x=xstart, xend=xend, y=ystart, yend=yend, geom="segment", data=dframe.all) + coord_equal(ratio=1) +
      theme_stimuli() + xlim(c(-pi, pi)) # + facet_wrap(~type)
    
    cd <- names(clientData)
    cd2 <- t(ldply(cd, function(i) clientData[[i]]))
    names(cd2) <- cd
    cd2 <- data.frame(id=input$id, final=input$submit, weight=input$weight,
                      ipid=input$ipid, md5=input$finger, time=Sys.time(), cd2)
    userResults <- rbind.fill(userResults, cd2)
    print(p1)
  })
  
  write.csv(userResults, "results.csv", row.names=FALSE)
  
  output$ycorrect <- renderPlot({
    f <- function(x) input$amp*sin(x)
    fprime <- function(x) input$amp*cos(x)
    f2prime <- function(x) -input$amp*sin(x)
    dframe <- createSine(n=input$obs+2, len=input$ell, f, fprime, f2prime, a=-pi, b=pi)[c(2:(input$obs+1)),]
    corr <- which(input$correct==c("none", "geom", "linear", "quad"))
    rat <- input$ell/(2*input$amp+input$ell)
    
    dframeAdj <- cbind(rbind(dframe, dframe), with(dframe, rbind(data.frame(seg.ystart=y-ell/2, seg.yend=y+ell/2, type="Segment"), data.frame(seg.ystart=ell/2, seg.yend=-ell/2, type="Adjustment"))), adj="Original Data")

    if(corr==4){
      dframeAdj1 <- adjQuad(dframe, f, fprime, f2prime)
      title = "Correction: Quadratic"
    } else if(corr==3){
      dframeAdj1 <- adjLinear(dframe, f, fprime, f2prime)
      title = "Correction: Linear"
    } else if(corr==2){
      dframeAdj1 <- adjGeom(dframe, f, fprime, f2prime)
      title = "Correction: Trigonometric"
    } else {
      dframeAdj1 <- adjNone(dframe, f, fprime, f2prime)
      title = "Correction: None"
    }
    limits <- range(c(subset(dframeAdj1, type=="Adjustment")$seg.ystart, 
                      subset(dframeAdj, type=="Adjustment")$seg.ystart, 
                      subset(dframeAdj1, type=="Adjustment")$seg.yend, 
                      subset(dframeAdj, type=="Adjustment")$seg.yend))
    if(corr==1) { 
      dplot <- dframeAdj
    } else{
      dplot <- rbind(dframeAdj, dframeAdj1)
    }
    p <- qplot(x=x, xend=x, y=seg.ystart, yend=seg.yend, geom="segment", data=dplot) +
      theme_bw() + coord_equal(ratio=1) + facet_grid(type~adj, scales="free_y", space="free_y") +
      scale_x_continuous(breaks=seq(-pi, pi, by=pi/2), 
                         labels=c(expression(-pi), expression(paste(-pi, "/2")), 0, 
                                  expression(paste(pi,"/2")), expression(pi))) +
      xlab("x") + ylab("y")
    print(p)
  })
})