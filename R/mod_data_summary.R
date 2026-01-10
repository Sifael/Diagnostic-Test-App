#' data_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_summary_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("summary_ui"))
}

#' data_summary Server Functions
#'
#' @noRd
mod_data_summary_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # instance of plotsclass
    plot_maker <- PlotsClass$new()

    output$summary_ui <- renderUI({
      req(data())

      bs4Card(
        title = "Data Summary Statistics",
        width = 12,
        headerBorder = FALSE,
        tabsetPanel(
          tabPanel(
            "Data Preview",
            icon = icon("table"),
            hr(),
            DTOutput(ns("data_preview"))
          ),
          tabPanel(
            "Data Summary",
            icon = icon("chart-bar"),
            hr(),
            DTOutput(ns("data_summary"))
          ),
          tabPanel(
            "Distribution Visualization",
            icon = icon("fire"),
            plotOutput(ns("numeric_distribution"))
          )
        )
      )
    })


    # Preview of the Dataset
    output$data_preview <- renderDT({
      req(data())
      datatable(data(),
        options = list(dom = "t"),
        rownames = FALSE
      )
    })

    # Summary Statistics of the Dataset
    output$data_summary <- renderDT({
      req(data())
      datatable(
        DataClass$summarize_dataset(data()),
        options = list(dom = "t"),
        rownames = FALSE
      )
    })

    # Distribution Plot for Numeric Variables
    output$numeric_distribution <- renderPlot({
      req(data())
      plot_maker$plot_numeric_boxplots(data())
    })
  })
}

## To be copied in the UI
# mod_data_summary_ui("data_summary_1")

## To be copied in the server
# mod_data_summary_server("data_summary_1")
