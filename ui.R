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

incrementButton <- function(inputId, label='A', value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/increment.js"))),
    tags$button(id = inputId,
                class = "increment btn",
                type = "button",
#                 text = label, 
                as.character(value))
  )
}

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Graphical Cognition"),
  
  sidebarPanel(
    sliderInput("weight", "", min=0, max=1, value=runif(1), step=.05), br(),br(),
    textInput("id", "ID"),br(),br(),
    incrementButton("submit", "Next Question"), br(),
    inputUserid("finger"),
    inputIp("ipid")
  ),
  mainPanel(
    plotOutput("illusion", width="auto")
  )
))
