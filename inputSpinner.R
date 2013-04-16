
inputSpinner <- function(inputId, value = 0) {
  if(length(value)<1) value = 0
  tagList(
    singleton(tags$head(tags$script(src = "js/spinner.js"))),
    tags$p("Please adjust the value below until the lines are of even size."),
    tags$table(id="spinner",
               tags$tr(tags$td(tags$input(class="lowerbutton",
                                          type="button", 
                                          value = " - ", 
                                          onclick="textlower();",
                                          style="font-size:15px;margin:0px 1px 5px 1px;width=45px")),
                       tags$td(tags$input(class="spinnernumber",
                                          type="text",
                                          id=inputId,
                                          value=as.character(value),
                                          style="max-width:45px", readonly = "TRUE")),
                       tags$td(tags$input(class="raisebutton",
                                          type="button", 
                                          value = " + ", 
                                          onclick="textraise();",
                                          style="font-size:15px;margin:0px 1px 5px 1px;width=45px"))
               )
    )
  )
}
