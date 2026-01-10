#' ci_method_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ci_method_inputs_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ci_methods_ui"))
}

#' ci_method_inputs Server Functions
#'
#' @noRd
mod_ci_method_inputs_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$ci_methods_ui <- renderUI({
      req(data())

      ci_methods <- get_available_ci_methods()
      bs4Card(
        title = "Confidence Interval Statistical Analysis",
        width = 12,
        fluidRow(
          col_6( selectInput(
                              inputId = ns("ci_method"),
                              label = "Select Confidence Interval Method:",
                              choices = ci_methods,
                              selected = ci_methods[1]
          )),
          col_6( sliderInput(
                              inputId = ns("conf_level"),
                              label = "Select Confidence Level:",
                              min = .5, max = .99, value = .95
          ))
        )
      )
    })
    outputOptions(output, "ci_methods_ui", suspendWhenHidden = FALSE)

    method <- reactive({
      req(input$ci_method)
      return(input$ci_method)
    })

    conf_level <- reactive({
      req(input$conf_level)
      return(input$conf_level)
    })

    return(list(method = method, conf_level = conf_level))
  })
}

## To be copied in the UI
# mod_ci_method_inputs_ui("ci_method_inputs_1")

## To be copied in the server
# mod_ci_method_inputs_server("ci_method_inputs_1")
