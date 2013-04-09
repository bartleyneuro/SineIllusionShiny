library(shiny)
library(shinyIncubator)

fingerprintInput <- function(inputId) {
  tagList(
    singleton(tags$head(tags$script(src = "js/jqfp.js"))),
    singleton(tags$head(tags$script(src = "js/jquery.js"))),
    singleton(tags$head(tags$script(src = "js/md5.js"))),
    tags$label("User ID"),
    tags$input(id = inputId, class = "shiny-bound-input", type = "text")
  )
}

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Graphical Cognition"),
  
  sidebarPanel(
    sliderInput("weight", "", min=0, max=1, value=0, step=.075),
    textInput("id", "ID"), 
    actionButton("submit", label="Next Question"),
    fingerprintInput("user")
  ),
  mainPanel(
    plotOutput("illusion", width="auto")
  )
))
