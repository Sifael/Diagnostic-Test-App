#' cutoff_data_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cutoff_data_summary_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("cutoff_data"))
}
    
#' cutoff_data_summary Server Functions
#'
#' @noRd 
mod_cutoff_data_summary_server <- function(id, data, ref_obj, test_obj){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    # output
    output$cutoff_data <- renderUI({
      req(data())
      bs4Card(
            title = "Reference & Test Variables with Cutoff Validation",
            width = 12,
            DTOutput(ns("cutoff_summary"))
        )
      })

    # cut off summary object
    cutoff_summary_df <- reactive({
        req(data(), ref_obj(), test_obj())

        # Process Reference Variable
        ref_data <- data()[[ref_obj()$var]]
        ref_logical <- apply_threshold(ref_data, ref_obj()$value, ref_obj()$operator)

        # Process Test Variable
        test_data <- data()[[test_obj()$var]]
        test_logical <- apply_threshold(test_data, test_obj()$value, test_obj()$operator)
        
        df <- data.frame(
            ref_col = ref_data,
            test_col = test_data,
            ref_logical = ref_logical,
            test_logical = test_logical,
            check.names = FALSE
        )
        colnames(df) <- c( ref_obj()$var, test_obj()$var,
                           paste0(ref_obj()$var, "_logical"),
                           paste0(test_obj()$var, "_logical"))
        return(df)
    })

    output$cutoff_summary <- renderDT({
            req(cutoff_summary_df())
            datatable(
                cutoff_summary_df(),
                options = list(dom = "t", paging = TRUE, searching = FALSE),
                rownames = FALSE
            )
        })

    })
}
    
## To be copied in the UI
# mod_cutoff_data_summary_ui("cutoff_data_summary_1")
    
## To be copied in the server
# mod_cutoff_data_summary_server("cutoff_data_summary_1")
