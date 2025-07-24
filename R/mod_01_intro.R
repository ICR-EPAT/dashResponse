#' 01_intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList includeMarkdown
#' @importFrom bslib card card_header card_body layout_columns
mod_01_intro_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Main intro content
    includeMarkdown(app_sys("app", "www", "intro_content.Rmd")),
    
    # Feature overview cards
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header("What You Can Create"),
        card_body(
          includeMarkdown(app_sys("app", "www", "features.Rmd"))
        )
      ),
      
      card(
        card_header("How It Works"),
        card_body(
          includeMarkdown(app_sys("app", "www", "process.Rmd"))
        )
      )
    ),
    
    # Data requirements
    card(
      card_header("Data Requirements"),
      card_body(
        includeMarkdown(app_sys("app", "www", "data_requirements.Rmd"))
      )
    ),
    
    # Get started button
    div(
      class = "text-center mt-4",
      actionButton(
        ns("get_started"),
        "Get Started with Data Upload",
        class = "btn-primary btn-lg"
      )
    )
  )
}

#' 01_intro Server Functions
#'
#' @noRd
mod_01_intro_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    reactive({
      input$get_started
    })
  })
}
## To be copied in the UI
# mod_01_intro_ui("01_intro")
    
## To be copied in the server
# mod_01_intro_server("01_intro")
