#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  # Your application server logic
  data_upload <- mod_data_upload_server("data_upload") 

  # visualize some basic summary stats
  mod_data_summary_server("data_summary", data_upload$data)

  # get the cutoffs selected by
  ref_cutoff <- mod_cutoff_inputs_server("ref_cutoff", "Reference Variable", data_upload$data, data_upload$numeric_cols)
  test_cutoff <- mod_cutoff_inputs_server("test_cutoff", "Test Variable", data_upload$data, data_upload$numeric_cols)

  # render UI component for tests
  method_selection <- mod_ci_method_inputs_server("ci_method_input", data_upload$data)

  # cut off summary
  mod_cutoff_data_summary_server("cutoff_summary", data_upload$data, ref_cutoff$cutoff, test_cutoff$cutoff)

  # Returns the CI interval dataframe
  # 
  ci_interval_df <- mod_ci_results_server( "ci_results", 
                                            data_upload$data,
                                            ref_cutoff$cutoff,
                                            test_cutoff$cutoff,
                                            method_selection$method,
                                            method_selection$conf_level
                                          )
  
  # Plot Visualization
  mod_ci_plots_server( "ci_plots",
                        data_upload$data,
                        ref_cutoff$cutoff,
                        test_cutoff$cutoff,
                        ci_interval_df$ci_interval_df,
                        method_selection$conf_level
                      )
  
  # app info
  mod_design_patterns_server("app_info")
  

}
