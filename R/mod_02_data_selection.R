#' 02_data_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_02_data_selection_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 02_data_selection Server Functions
#'
#' @noRd 
mod_02_data_selection_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_02_data_selection_ui("02_data_selection")
    
## To be copied in the server
# mod_02_data_selection_server("02_data_selection")
