#' ci_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ci_results_ui <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "ci_results",
    fluidRow(
      bs4Card(
        title = "Summary Metrics",
        width = 6,
        DTOutput(ns("clean_summary")),
        mod_download_handler_ui(ns("dl_summary_results"), "Download Summary")
      ),
      bs4Card(
        title = "Contingency Table",
        width = 6,
        DTOutput(ns("contingency_matrix")),
        mod_download_handler_ui(ns("dl_contingency_matrix"), "Download Matrix")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Confidence Interval Metrics",
        width = 12,
        DTOutput(ns("ci_intervals")),
        mod_download_handler_ui(ns("dl_ci_intervals"), "Download CI Intervals")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Contingency Counts",
        width = 6,
        DTOutput(ns("contingency_count")),
        mod_download_handler_ui(ns("dl_contingency_counts"), "Download Counts")
      )
    )
  )
}
    
#' ci_results Server Functions
#'
#' @noRd 
mod_ci_results_server <- function(id, data, ref_obj, test_obj, method, conf_level){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    contingency_obj <- reactive({
      req(data(), ref_obj(), test_obj())
      
      # Apply thresholds to get boolean vectors
      ref_vec <- apply_threshold(data()[[ref_obj()$var]], ref_obj()$value, ref_obj()$operator)
      test_vec <- apply_threshold(data()[[test_obj()$var]], test_obj()$value, test_obj()$operator)

      BinaryContingency$new(ref_vec, test_vec)
    })

    # 2. Reactive Dataframes for Outputs
    cont_matrix <- reactive({ contingency_obj()$get_contingency_matrix() })
    cont_counts <- reactive({ contingency_obj()$get_contingency_counts_df() })
    summary_results <- reactive({ contingency_obj()$get_clean_summary(ref_obj(), test_obj()) })
    
    ci_results <- reactive({
      req(contingency_obj(), method(), conf_level())
      
      # Get base metrics
      metrics_df <- contingency_obj()$get_classification_metrics_df()
      
      
      ci_calc <- CIMethods$new(conf_level = conf_level())
      ci_calc$calculate_ci(metrics_df, method() )
    })

    
    
    # 3. Render Outputs
    output$contingency_matrix <- renderDT({ 
      datatable(  cont_matrix(), 
                  options = list(dom = "t"), 
                  rownames = FALSE) 
    })
    
    output$contingency_count <- renderDT({ 
      datatable( cont_counts(), 
                 options = list(dom = "t"), 
                 rownames = FALSE) 
    })
    
    # render confidence interval result
    output$ci_intervals <- renderDT({ 
      datatable( DataClass$round_numeric_columns(ci_results()), # use of round_numeric_columns
                 options = list(dom = "t"), 
                 rownames = FALSE)  
    })

    # render clean summary
    output$clean_summary <- renderDT({
      datatable( summary_results(),
                 options = list(dom = "t"), 
                 rownames = FALSE) 
    })

    
    # Download Initializers 
    mod_download_handler_server("dl_summary_results", "clean_summary", summary_results, type = "csv")
    mod_download_handler_server("dl_contingency_matrix", "contingency_matrix", cont_matrix, type = "csv")
    mod_download_handler_server("dl_contingency_counts", "contingency_counts", cont_counts, type = "csv")
    mod_download_handler_server("dl_ci_intervals", "confidence_interval", ci_results, type = "csv")
    

    # Returning Objects
    ci_interval_df <- reactive({
      req(ci_results())
      ci_results()
    })
    

    return(list(ci_interval_df = ci_interval_df))
  })
}
    
## To be copied in the UI
# mod_ci_results_ui("ci_results_1")
    
## To be copied in the server
# mod_ci_results_server("ci_results_1")
