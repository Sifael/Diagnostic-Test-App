#' data_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_upload_ui <- function(id) {
  ns <- NS(id)
    bs4Card(
        width = 12,
        title = "Data Upload and Summary Statistics",
        solidHeader = TRUE,
        fileInput( inputId = ns("file_upload"),
                   label = "Enter a CSV or Excel File",
                   accept = c(".csv", ".xlsx", ".xls"),
                   placeholder = "No file selected"
        ),
        hr(),
        actionButton(
          inputId = ns("load_demo_data"),
          label = "Load Demo Data"
        )
    )
}
    
#' data_upload Server Functions
#'
#' @noRd 
mod_data_upload_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    data_obj <- DataClass$new()
    data_version <- reactiveVal(0)

    observeEvent( input$load_demo_data,{
      data_obj$generate_demo_data()
      data_version(data_version() + 1) # need this because data_obj is static
    } )

    observeEvent( input$file_upload, {
      req(input$file_upload)
      data_version(data_version() + 1)

      tryCatch(
        {
          data_obj$read_validated_file(input$file_upload)
        },
        error = function(e){
          showNotification(
            e$message,
            type = "error",
            duration = 5
          )
        }
      )
    })


    # returns data object
    data <- reactive({
      data_version()
      if (nrow(data_obj$df) == 0) {
        return(NULL)
      }
      data_obj$df
    })

    # returns numeric_cols
    numeric_cols <- reactive({
      data_version()
      if (nrow(data_obj$df) == 0) {  
        return(NULL)
      }
      data_obj$get_numeric_columns()
    })


    return(list(
      data = data,
      numeric_cols = numeric_cols
    ))
  })

}
    
## To be copied in the UI
# mod_data_upload_ui("data_upload_1")
    
## To be copied in the server
# mod_data_upload_server("data_upload_1")
