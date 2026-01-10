#' ci_plots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ci_plots_ui <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "plots",
    fluidRow(
      bs4Card(
        title = "Confidence Interval Range",
        width = 6,
        plotOutput(ns("ci_range_plot")),
        mod_download_handler_ui(ns("dl_ci_range_plot"), "Download Plot")
      ),
      bs4Card(
        title = "Box Plot - Reference vs. Test",
        width = 6,
        plotOutput(ns("distribution_plot")),
        mod_download_handler_ui(ns("dl_distribution_plot"), "Download Plot")
      ),
      bs4Card(
        title = "Test Variable Distribution",
        width = 12,
        plotOutput(ns("test_var_dist_plot"))
      )
    )
  )
}
    
#' ci_plots Server Functions
#'
#' @noRd 
mod_ci_plots_server <- function(id, data, ref_obj, test_obj, ci_interval_df, ci_level){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Instantiate Plot Manager
    plot_manager <- PlotsClass$new()

    output$distribution_plot <- renderPlot({
      req(data(), ref_obj(), test_obj())
      plot_manager$plot_comparison_boxplots(data(), ref_obj()$var, test_obj()$var)
    })

    output$ci_range_plot <- renderPlot({
      req(ci_interval_df(), ci_level())
      plot_manager$plot_ci_ranges(ci_interval_df(), ci_level())
    })

    output$test_var_dist_plot <- renderPlot({
      req(data(), test_obj())
      plot_manager$plot_distribution(data(), test_obj()$var)
    })
    
    
    # Download Handlers
    dist_plot <- reactive({
      req(data(), ref_obj(), test_obj())
      plot_manager$plot_comparison_boxplots(data(), ref_obj()$var, test_obj()$var)
    })
    mod_download_handler_server("dl_distribution_plot", "distribution_plot", dist_plot, type = "png")


    ci_range_plot <- reactive({
      req(ci_interval_df(), ci_level())
      plot_manager$plot_ci_ranges(ci_interval_df(), ci_level())
    })
    mod_download_handler_server("dl_ci_range_plot", "ci_range_plot", ci_range_plot, type = "png")


  })
}
    
## To be copied in the UI
# mod_ci_plots_ui("ci_plots_1")
    
## To be copied in the server
# mod_ci_plots_server("ci_plots_1")
