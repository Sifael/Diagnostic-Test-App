#' download_handler UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param label Label for the download button
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_handler_ui <- function(id, label = "Download") {
    ns <- NS(id)
    downloadButton(ns("download"), label)
}

#' download_handler Server Functions
#'
#' @noRd
mod_download_handler_server <- function(id, filename, data, type = "csv") {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output$download <- downloadHandler(
            filename = function() {
                paste0(filename, "-", Sys.Date(), ".", type)
            },
            content = function(file) {
                req(data())
                if (type == "csv") {
                    write.csv(data(), file, row.names = FALSE)
                } else if (type == "png") {
                    png(file)
                    print(data())
                    dev.off()
                }
            }
        )
    })
}

## To be copied in the UI
# mod_download_handler_ui("download_handler_1")

## To be copied in the server
# mod_download_handler_server("download_handler_1")
