library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(shinyIncubator)
library(RMySQL)

source("./functions.R")
source("./inputSpinner.R")
#' Look at https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/THb1Ql5E20s 
#' for shiny server user-level data collection

shinyServer(function(input, output, clientData) {

  suppressMessages(library(ggplot2))

  wopts <- seq(0, 1, .05)
  
  # make list of all combinations of w, type, with corresponding index to reference list of dataframes.
  dframelist <- expand.grid(w=wopts, type=c("x", "y"), stringsAsFactors=FALSE)
  dframelist$idx <- 1:nrow(dframelist)
  
  reqtrials <- 6
  trialsreq <- expand.grid(w=c(1, sample(6:16, 1), 21), type=c("x", "y"))
  # sample the middle weight option, then 0, 1. Combine with each combination of x, y.
  trialsreq <- trialsreq[sample(1:reqtrials, reqtrials),] # shuffle
  
  trialsextra <- expand.grid(w=1:21, type=c("x", "y"))
  trialsextra <- trialsextra[sample(1:(2*length(wopts)), 2*length(wopts)),]
  # leave repeats in to get a measure of reliability
  trials <- rbind(trialsreq, trialsextra)
  trials$order <- 1:nrow(trials)
  trials$w <- wopts[trials$w]
  trials$w <- ceiling(trials$w/.05)*.05
  trials <- merge(trials, dframelist)
  trials <- trials[order(trials$order),-3]
  
  maxtrials <- nrow(trials)
  
  dframe <- do.call("rbind", lapply(dframelist$idx, function(i) getData.wtype(dframelist$w[i], dframelist$type[i])))
  dframe <- merge(dframe, dframelist, sort=FALSE)
  
  q <- reactive((input$q+input$skip) %% maxtrials + 1)
  
  
  output$weightControl <- renderUI({
    inputSpinner("weight", value=trials$w[q()])
  })
  
  
  output$illusion <- renderPlot({
    
    if(length(input$weight)<1) wt <- 0 else wt <- ceiling(as.numeric(input$weight)/.05)*.05

    dframe.all <- subset(dframe, type==trials$type[q()] & w==wt)

    p1 <- qplot(x=xstart, xend=xend, y=ystart, yend=yend, geom="segment", data=dframe.all) + coord_equal(ratio=1) +
      theme_stimuli() + xlim(c(-pi, pi))  
    print(p1)
  })

  output$testtext <- renderText(results())
  
  output$data <- renderPrint({
    invisible({

    cd <- reactiveValuesToList(clientData)
    cd1 <- reactiveValuesToList(input)
    both <- list(cd, cd1)
    n <- unique(unlist(lapply(both, names)))
    names(n) <- n
    cd2 <- lapply(n, function(ni) unlist(lapply(both, `[[`, ni)))
    iptemp <- strsplit(cd2$ipid, ".", fixed=TRUE)[[1]]
    iptemp[is.na(iptemp)] <- 0
    cd2$ipid <- paste(iptemp[1:3], collapse=".") 
    # censor last 3 digits of ip value for privacy - 1st 9 allow city-level resolution.
    cd2 <- as.data.frame(cd2, stringsAsFactors=FALSE)
    cd2$time <- as.POSIXct(Sys.time())
    if(!"weight"%in%names(cd2)) cd2$weight <- NA
    if(cd2$ipid !="NA.NA.NA" & !is.na(cd2$weight) & cd2$fingerprint!=""){
      # only write to db if there is certain identifying information.
      con <- dbConnect(MySQL(), user="skoons", password="sKOONsstat1t",dbname="skoons", host="mysql2.stat.iastate.edu")
      #     save(cd2, file="df.Rdata")
      cd2 <- cd2[,c("allowDataUriScheme", "fingerprint", "userid", "ipid", "output_illusion_height", "output_illusion_width", "pixelratio", "q", "skip", "time", "url_hostname", "url_pathname", "url_port", "url_protocol", "url_search", "weight")]
#       message(paste(names(cd2), cd2, sep=" = ", collapse=", "))
      
      dbWriteTable(con, name="SineIllusionShiny", value=cd2, append=TRUE, row.names=FALSE)
      dbDisconnect(con)
    }
    
    })
  })
  
  output$questionCounter1 <- renderText({
    paste("Questions Answered: ", input$q, " (", reqtrials, " required for payment)", sep="")
  })
  
  output$questionCounter2 <- renderText({
    act.trials <- input$skip + input$q
    
    paste("Questions Skipped: ", input$skip, " of ", act.trials, " attempted", sep="")
  })



})  
