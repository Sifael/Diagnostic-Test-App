#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @import DT
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
          header = dashboardHeader(title = "Sample Case App"), 
          sidebar = dashboardSidebar(
              sidebarMenu(
                    menuItem(text = "Data Upload", tabName = "data_upload", icon = icon("upload")),
                    menuItem(text = "Variable Selection", tabName = "ci_method_selection", icon = icon("sliders-h")),
                    menuItem(text = "Standard Metrics", tabName = "ci_results", icon = icon("table")),
                    menuItem(text = "Analysis Plots", tabName = "ci_plots", icon = icon("chart-line")),
                    menuItem(text = "App Information", tabName = "app_info", icon = icon("info"))
                  )
                ),
          body = dashboardBody(
              tabItems(
                tabItem(
                  tabName = "data_upload",
                  mod_data_upload_ui("data_upload"),
                  mod_data_summary_ui("data_summary")
                ),
                tabItem(
                  tabName = "ci_method_selection",
                  mod_cutoff_inputs_ui("ref_cutoff"),
                  mod_cutoff_inputs_ui("test_cutoff"),
                  mod_cutoff_data_summary_ui("cutoff_summary")
                ),
                tabItem(
                  tabName = "ci_results",
                  mod_ci_method_inputs_ui("ci_method_input"),
                  mod_ci_results_ui("ci_results")
                ),
                tabItem(
                  tabName = "ci_plots",
                  mod_ci_plots_ui("ci_plots")
                ),
                tabItem(
                  tabName = "app_info",
                  mod_design_patterns_ui("app_info")
                )
              )
      )
                
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "interviewcaseapp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
