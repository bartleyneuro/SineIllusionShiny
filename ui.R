library(shiny)
library(shinyIncubator)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Graphical Cognition"),
  
  sidebarPanel(
    sliderInput("weight", "", min=0, max=1, value=0, step=.075),
    textInput("id", "ID"), 
    actionButton("submit", label="Next Question")
  ),
  mainPanel(
    plotOutput("illusion", width="auto")
  )
))
