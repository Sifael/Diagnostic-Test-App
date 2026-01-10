#' cutoff_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cutoff_inputs_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("cutoff_ui"))
}

#' cutoff_inputs Server Functions
#'
#' @noRd
mod_cutoff_inputs_server <- function(id, label, data, numeric_cols) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$cutoff_ui <- renderUI({
      req(data(), numeric_cols())

      bs4Card(
        title = paste0(label, " Variable Cutoff Selection"),
        width = 12,
        fluidRow(
          col_4( selectInput(
                              inputId = ns("var"),
                              label = "Select Variable",
                              choices = c("— Select a variable —" = "", numeric_cols()), 
                              selected = NULL
          )),
          col_4(selectInput(
                              inputId = ns("operator"),
                              label = "Operator (Positive if...)",
                              choices = c("— Select an operator —" = "", get_operators()),
                              selected = NULL  
          )),
          col_4(sliderInput(
                              inputId = ns("cutoff"),
                              label = "Cutoff Value",
                              min = 0, max = 1, value = .5
          ))
        )
      )
    })
    outputOptions(output, "cutoff_ui", suspendWhenHidden = FALSE)

    # Updates the min, max and value with range
    observeEvent(input$var, {
      req(data())

      x <- data()[[input$var]]
      rng <- round(range(x, na.rm = TRUE), 2)
      updateSliderInput(
        session = session,
        inputId = "cutoff",
        min = min(rng),
        max = max(rng),
        value = mean(rng)
      )
    })


    # returns the selections
    cutoff <- reactive({
      req(input$var, input$operator, input$cutoff)

      list(
        var = input$var,
        operator = input$operator,
        value = input$cutoff
      )
    })

    return(list(cutoff = cutoff))
  })
}

## To be copied in the UI
# mod_cutoff_inputs_ui("cutoff_inputs_1")

## To be copied in the server
# mod_cutoff_inputs_server("cutoff_inputs_1")
