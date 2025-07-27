#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import bslib
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_01_intro_server("01_intro")
  mod_02_data_selection_server("02_data_selection")
}
