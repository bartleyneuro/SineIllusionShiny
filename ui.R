library(shiny)
library(shinyIncubator)

inputUserid <- function(inputId, value='') {
#   print(paste(inputId, "=", value))
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "userid", value=as.character(value), type="text", style="display:none;")
  )
}

inputIp <- function(inputId, value=''){
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "ipaddr", value=as.character(value), type="text", style="display:none;")
  )
}

incrementButton <- function(inputId, prelabel="", postlabel="", value = 1) {
  tagList(
    singleton(tags$head(tags$script(src = "js/increment.js"))),
    tags$table(id="incrementbutton",
               tags$tr(tags$td(prelabel), 
                       tags$td(tags$button(id = inputId,
                                           class = "increment btn",
                                           type = "button",
                                           as.character(value))),
                       tags$td(postlabel)))
  )
}
source("./inputSpinner.R")
# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Graphical Cognition"),
  
  sidebarPanel(
    textInput("userid", "ID"), 
    br(), 
    br(),
    uiOutput("weightControl"),
    br(), 
    actionButton("q", "Next Question"),br(),br(),
    textOutput("questionCounter1"),br(),
    textOutput("questionCounter2"),br(),
    inputIp("ipid"),
    inputUserid("fingerprint"),
    actionButton("skip", "Skip Question")
  ),
  mainPanel(
#     textOutput("testtext"),
    plotOutput("illusion", width="auto"),
    textOutput("data")
  )
))
