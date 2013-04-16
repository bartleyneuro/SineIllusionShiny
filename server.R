library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(scrapeR)
library(shinyIncubator)

source("./functions.R")
source("./inputSpinner.R")
#' Look at https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/THb1Ql5E20s for shiny server user-level data collection

shinyServer(function(input, output, clientData) {
  suppressMessages(library(ggplot2))

  wopts <- seq(0.05, .8, .05)
  trialsreq <- expand.grid(w=c(0, sample(wopts, 2)), type=c("x", "y"))[sample(1:6, 6),]
  trialsextra <- expand.grid(w=wopts, type=c("x", "y"))[sample(1:(2*length(wopts)), 2*length(wopts)),]
  trials <- rbind(trialsreq, trialsextra)
  trials$w <- ceiling(trials$w/0.05)*.05
  reqtrials <- 6
  maxtrials <- nrow(trials)
  
  output$illusion <- renderPlot({
    
    f <- function(x) 2*sin(x)
    fprime <- function(x) 2*cos(x)
    f2prime <- function(x) -2*sin(x)
    if(length(input$weight)<1) w <- 0 else w <- as.numeric(input$weight)
    
    dframe <- createSine(n=40+2, len=1, f, fprime, f2prime, a=-pi, b=pi)[c(2:(41)),]
    dframe.all <- dframe
    q <- input$q %% maxtrials + 1
    if(trials$type[q]=="x"){
      dframeAdj <- cbind(adjx(dframe, fprime=fprime, w=w), adj=paste("X corrected, weight =", w))
      dframe.all <- dframeAdj
    } else {
      dframeAdj <- cbind(adjLinear(dframe, f, fprime, f2prime, w=w), adj=paste("Y corrected, weight =", w))
      dframe.all <- dframeAdj
    }
    
    cd <- names(clientData)
    cd2 <- t(ldply(cd, function(i) clientData[[i]]))
    names(cd2) <- cd
    inames <- names(input)
    ivals <- t(ldply(inames, function(i) input[[i]]))
    names(ivals) <- inames
    cd2 <- data.frame(cbind(cd2, ivals))
    names(cd2) <- c(cd, inames)
    cd2$time <- Sys.time()
    
    cd2 <- cd2[,order(names(cd2))]
    
    
    p1 <- qplot(x=xstart, xend=xend, y=ystart, yend=yend, geom="segment", data=dframe.all) + coord_equal(ratio=1) +
      theme_stimuli() + xlim(c(-pi, pi)) # + facet_wrap(~type)

    
    print(p1)
  })

  writeResults <- reactive({
      
#       if("results.csv"%in%list.files())  {
#         userResults <- read.csv("results.csv", stringsAsFactors=FALSE)
#         userResults <- rbind.fill(userResults, cd2)
#       } else {
#         userResults <- cd2
#       }
#           
#       write.csv(userResults, "results.csv", row.names=FALSE)
      write.csv(cd2, "results.csv", row.names=FALSE, append=TRUE)
    })
  
  output$testtext <- renderText({
    paste(sapply(names(input), function(i) paste(i, "=", input[[i]], sep=" ")), collapse="\n")
  })
  
  output$questionCounter <- renderText({
    if(input$q>=reqtrials) paste("Question ", input$q+1, " (", reqtrials," required).", sep="") else
      paste("Question ", input$q+1 , " of ", reqtrials," required.\n", reqtrials - 1 - input$q, " remaining.", sep="")
  })
  
  output$weightControl <- renderUI({
    q <- input$q %% maxtrials + 1
    inputSpinner("weight", value=trials$w[q]) 
  })

})