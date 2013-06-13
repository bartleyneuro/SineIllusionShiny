
inputSpinner <- function(inputId, value = 0) {
  if(length(value)<1) value = 0
  tagList(
    singleton(tags$head(tags$script(src = "js/spinner.js"))),
    tags$p("Please adjust the value below until you feel the lines are (approximately) the same size."),
    tags$p(id="spinner", tags$input(class="spinnernumber",
                                    type="text",
                                    id=inputId,
                                    value=as.character(value),
                                    style="display:none"), 
                         tags$input(class="spinnerorig",
                                    type="text",
                                    id=inputId,
                                    value=as.character(value),
                                    style="display:none"), 
           tags$div(tags$table(id="spinnerout", tags$tr(
                            tags$td(tags$input(class="btn btn-small",
                                               type="button", 
                                               value = " - ", 
                                               onclick="textlower();")),
                            tags$td(tags$input(class="spinnerlabel",
                                               type="text",
                                               id=inputId,
                                               value="Adjustment",
                                               style="max-width:75px;margin:auto", readonly = "TRUE")),
                            tags$td(tags$input(class="btn btn-small",
                                               type="button", 
                                               value = " + ", 
                                               onclick="textraise();")),
                            tags$td(" ", style="padding-left:2em;"),
                            tags$td(tags$input(class="btn btn-small",
                                               type="button",
                                               value = "Reset",
                                               onclick="textreset();"))
                          )),style="width:60%;  margin:0 auto;")
      ),
    tags$p("When you are satisfied, press the \"Finished\" button for another question.")
  )
}

