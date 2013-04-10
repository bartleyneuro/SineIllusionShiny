library(shiny)
library(shinyIncubator)

getID <- function(inputId) {
  print(inputId)
  tagList(
#     singleton(tags$head(tags$script(src = "js/jqfp.js", type='text/javascript'))),
#     singleton(tags$head(tags$script(src = "js/jquery.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setuid()"),
    tags$input(id = inputId, class = "userid", value="init"),
    tags$style(type='text/css', ".userid { display:none; }")
  )
}
# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Graphical Cognition"),
  
  sidebarPanel(
    sliderInput("weight", "", min=0, max=1, value=0, step=.075),
    textInput("id", "ID"), br(),br(),
    actionButton("submit", label="Next Question"),br(),br(),
    getID("finger")
  ),
  mainPanel(
#     includeHTML("www/js/wimp.js"),
    plotOutput("illusion", width="auto"),
    textOutput("idtext")
  )
))
