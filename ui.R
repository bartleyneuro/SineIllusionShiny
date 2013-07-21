library(shiny)
# library(shinyIncubator)

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
# 
# incrementButton <- function(inputId, prelabel="", postlabel="", value = 1) {
#   tagList(
#     singleton(tags$head(tags$script(src = "js/increment.js"))),
#     tags$table(id="incrementbutton",
#                tags$tr(tags$td(prelabel), 
#                        tags$td(tags$button(id = inputId,
#                                            class = "increment btn",
#                                            type = "button",
#                                            as.character(value))),
#                        tags$td(postlabel)))
#   )
# }
source("./inputSpinner.R")
# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  # Application title  
  headerPanel("Graphical Cognition"),
  
  sidebarPanel(
    conditionalPanel(condition = "input.q<30",
      uiOutput("weightControl"),
      actionButton("q", "Finished"), 
      span(" ", style="display:inline-block;margin-left:39%"),
      actionButton("skip", "Skip Question")
    ),
    br(),br(),
    textOutput("questionCounter1"),
    textOutput("questionCounter2"),
    tags$small(textOutput("getsPaid")),br(),
    inputIp("ipid"),
    inputUserid("fingerprint"),
    textInput("userid", "Amazon Worker ID"), 
    helpText("Only necessary for payment through Amazon Turk."),
    br(),
    tagList(p(tags$small("This experiment is IRB exempt ", a(href = "https://dl.dropboxusercontent.com/u/5756914/IRBForm.pdf", "(More Information)"))))
  ),
  mainPanel(
#     textOutput("testtext"),
    plotOutput("illusion", width="auto"),
    textOutput("helpText"),
    textOutput("data")
  )
))
