library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
# library(shinyIncubator)
library(RMySQL)
library(digest)

source("./functions.R")
source("./inputSpinner.R")
#' Look at https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/THb1Ql5E20s 
#' for shiny server user-level data collection

shinyServer(function(input, output, session) {

  suppressMessages(library(ggplot2))

  diffs <- c(0, .5, .5, .5, .5, .25, .25, .25, .25, .25, .25, .25, .25, .1, .1, .05, .05, .05, .05, .05, .05, .025, .025, .025, .025, .025, .025, .025, .025, .025, .025, .025, .025, .05, .05, .05, .05, .1, .1, .1, .1, .1, .25, .25, .25, .25, .25, .25, .5, .5, .5, .5)
  wopts <- -4 +cumsum(diffs)
  
  # make list of all combinations of w, type, with corresponding index to reference list of dataframes.
  dframelist <- expand.grid(z=1:length(wopts), type=c("x", "y"), stringsAsFactors=FALSE)
  dframelist$idx <- 1:nrow(dframelist)
  dframelist$w <- wopts[dframelist$z]
  
  reqtrials <- 6
  trialsreq <- expand.grid(z=c(13, sample(16:26, 1), 31), type=c("x", "y"))
  # sample the middle weight option, then 0, 1. Combine with each combination of x, y.
  trialsreq <- trialsreq[sample(1:reqtrials, reqtrials),] # shuffle
  
  trialsextra <- expand.grid(z=13:31, type=c("x", "y"))
  trialsextra <- trialsextra[sample(1:(length(trialsextra$z)), length(trialsextra$z)),]
  # leave repeats in to get a measure of reliability
  trials <- rbind(trialsreq, trialsextra)
  trials$order <- 1:nrow(trials)
  trials$w <- wopts[trials$z]
  trials$w <- ceiling(trials$w/.05)*.05
  trials <- merge(trials, dframelist)
  trials <- trials[order(trials$order),-4]
  
  maxtrials <- nrow(trials)
  
  dframe <- do.call("rbind", lapply(dframelist$idx, function(i) getData.wtype(dframelist$w[i], dframelist$type[i])))
  dframe <- merge(dframe, dframelist, sort=FALSE)

  q <- reactive((input$q+input$skip) %% maxtrials + 1)
  wt <- reactive({
            if(length(input$weight)<1){
              return(wopts[trials$z[q()]])
            } else {
              return(wopts[as.numeric(input$weight)])
            }  
          })
  
  output$weightControl <- renderUI({
    (input$q+input$skip+input$reset)
    inputSpinner("weight", value=trials$z[q()])
  })

  output$illusion <- renderPlot({
    if(input$q<=30){
      dframe.all <- subset(dframe, type==trials$type[q()] & w==wt())
      p1 <- qplot(x=xstart, xend=xend, y=ystart, yend=yend, geom="segment", data=dframe.all) + 
        coord_equal(ratio=1) +
        theme_stimuli() + xlim(c(-pi, pi))  
    }else{
      p1 <- qplot(x=0, y=0, geom="text", label="Thank you for participating!", size=I(15)) + theme_stimuli()
    }      
    print(p1)
  })

  output$testtext <- renderText(paste("weight: ", wt(), 
                                      "     type: ", trials$type[q()],
                                      "     reset: ", input$reset,
                                      "     q: ", input$q,
                                      "     skip: ", input$skip
#                                       "     fingerprint: ", input$fingerprint, 
#                                       "     ip: ", input$ipid)
                                ))
  

  output$data <- renderPrint({
    invisible({

    cd <- reactiveValuesToList(session$clientData)
    cd1 <- reactiveValuesToList(input)
    both <- list(cd, cd1)
    n <- unique(unlist(lapply(both, names)))
    names(n) <- n
    cd2 <- lapply(n, function(ni) unlist(lapply(both, `[[`, ni)))
    iptemp <- strsplit(cd2$ipid, ".", fixed=TRUE)[[1]]
    iptemp[is.na(iptemp)] <- 0
    cd2$ipid <- paste(iptemp[1:3], collapse=".") 
    # censor last 3 digits of ip value for privacy - 1st 9 allow city-level resolution.
    cd2$iphash <- digest(input$ipid)
    # also hash full IP address 
    cd2 <- as.data.frame(cd2, stringsAsFactors=FALSE)
    cd2$time <- as.POSIXct(Sys.time())
    cd2$type <- trials$type[q()]
    if(!"weight"%in%names(cd2)) cd2$weight <- NA
    
    # only write to db if there is certain identifying information.
    if(cd2$ipid !="NA.NA.NA" & !is.na(cd2$weight) & cd2$fingerprint!=""){

      con <- dbConnect(MySQL(), group="stat")
      cd2 <- cd2[,c("allowDataUriScheme", "fingerprint", "userid", "ipid", "output_illusion_height", "output_illusion_width", "pixelratio", "q", "skip", "time", "url_hostname", "url_pathname", "url_port", "url_protocol", "url_search", "weight", "iphash", "type")]
#       save(cd2, file="df.Rdata")
      dbWriteTable(con, name="SineIllusionShiny", value=cd2, append=TRUE, row.names=FALSE)
      dbDisconnect(con)
    }
    
    })
  })
  
  output$questionCounter1 <- renderText({
    paste("Questions Answered: ", input$q, sep="")
  })
  
  output$questionCounter2 <- renderText({
    act.trials <- input$skip + input$q
    
    paste("Questions Skipped: ", input$skip, " of ", act.trials, " attempted", sep="")
  })

  output$getsPaid <- renderText({
    if((input$q-input$skip)>6 & nchar(input$userid)>3) 
      "You are now eligible for payment if your Worker ID is correct and you accepted the HIT on Amazon" 
    else if((input$q-input$skip)>6) 
      "Please input your Worker ID to receive payment through Amazon Turk." 
    else " "
  })


})  
