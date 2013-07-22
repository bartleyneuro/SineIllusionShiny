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


#' If date is greater than 7.21.2013 at 12pm, then omit 1st 2 trials. 
shinyServer(function(input, output, clientData) {
  suppressMessages(library(ggplot2))
  source("userSetup.R")

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
    if(input$q==0){ # Initial trial of y-axis
      dframe.all <- subset(dframe, type=="y" & w==wt())
      p1 <- qplot(x=xstart, xend=xend, y=ystart, yend=yend, geom="segment", data=dframe.all) + 
        geom_line(aes(x=x, y=y), data=dframe.all, colour=I("grey60")) +
        coord_equal(ratio=1) +
        theme_stimuli() + xlim(c(-pi, pi))  
    } else if(input$q==1){ # Initial trial of x-axis
        dframe.all <- subset(dframe, type=="x" & w==wt())
        p1 <- qplot(x=xstart, xend=xend, y=ystart, yend=yend, geom="segment", data=dframe.all) + 
          geom_line(aes(x=x, y=y), data=dframe.all, colour=I("grey75")) +
          coord_equal(ratio=1) +
          theme_stimuli() + xlim(c(-pi, pi))  
    } else if(input$q<=32){
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
    if(input$q<2) paste("Sample Question ", input$q+1, sep="")
    else paste("Questions Answered: ", max(input$q-2,0), sep="")
  })
  
  output$questionCounter2 <- renderText({
    act.trials <- input$skip + input$q
    
    paste("Questions Skipped: ", input$skip, " of ", act.trials, " attempted", sep="")
  })

  output$getsPaid <- renderText({
    if((input$q-input$skip)>reqtrials & nchar(input$userid)>3) 
      "You are now eligible for payment if your Worker ID is correct and you accepted the HIT on Amazon. You may continue adjusting these figures if you wish - it will help us estimate the effect more precisely. " 
    else if((input$q-input$skip)>reqtrials) 
      "Please input your Worker ID if you wish to receive payment through Amazon Turk. You may continue adjusting these figures if you wish - it will help us estimate the effect more precisely. " 
    else " "
  })
  
  output$helpText <- renderText({
    if(input$q == 0) "Adjust the image until the line segments appear to be equal. Use the grey line to help you this time."
    else if(input$q == 1) "Adjust the image until the line segments appear to be equal and approximately follow the grey line. The grey line is here to help you this time, but will not be present in future trials."
    else if((input$q-input$skip)>reqtrials & nchar(input$userid)>3) 
      "You are now eligible for payment if your Worker ID is correct and you accepted the HIT on Amazon.You may continue adjusting these figures if you wish - it will help us estimate the effect more precisely. " 
    else if((input$q-input$skip)>reqtrials) 
      "Please input your Worker ID to receive payment through Amazon Turk.  You may continue adjusting these figures if you wish - it will help us estimate the effect more precisely. " 
    else " "
  })


})  
