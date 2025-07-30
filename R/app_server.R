#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import bslib
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_01_intro_server("01_intro")
  data_module <- mod_02_data_selection_server("02_data_selection")
  plot_module <- mod_03_plotting_server("03_plotting", data_module)
  mod_04_code_export_server("04_export", data_module, plot_module)
}
