#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic with bslib theming
    page_navbar(
      title = "dashResponse",
      fillable = TRUE,  # Allow content to fill available space
      theme = bs_theme(
        version = 5,
        bootswatch = "flatly",  # Professional, clean theme
        primary = "#2c3e50",    # Dark blue-gray
        secondary = "#95a5a6",  # Light gray
        success = "#27ae60",    # Green for positive outcomes
        info = "#3498db",       # Blue for informational content
        warning = "#f39c12",    # Orange for warnings
        danger = "#e74c3c",     # Red for errors/negative outcomes
        base_font = font_google("Inter"),  # Modern, readable font
        heading_font = font_google("Inter", wght = "600"),
        code_font = font_google("JetBrains Mono")
      ),
      
      # Navigation tabs
      nav_panel(
        title = "Introduction",
        icon = icon("home"),
        div(class = "container-fluid py-4",
            mod_01_intro_ui("01_intro")
        )
      ),
      
      nav_panel(
        title = "Data Selection", 
        icon = icon("upload"),
        div(class = "container-fluid py-4",
            # mod_02_data_ui("02_data_1") # Add when ready
            div(class = "text-center mt-5",
                h3("Data Selection Module"),
                p("Coming soon...")
            )
        )
      ),
      
      nav_panel(
        title = "Plotting",
        icon = icon("chart-line"),
        div(class = "container-fluid py-4",
            # mod_03_plotting_ui("03_plotting_1") # Add when ready
            div(class = "text-center mt-5",
                h3("Plotting Module"), 
                p("Coming soon...")
            )
        )
      ),
      
      nav_panel(
        title = "Code Export",
        icon = icon("code"),
        div(class = "container-fluid py-4",
            # mod_04_export_ui("04_export_1") # Add when ready
            div(class = "text-center mt-5",
                h3("Code Export Module"),
                p("Coming soon...")
            )
        )
      ),
      
      # Optional: Add a dropdown menu with additional options
      nav_menu(
        title = "Help",
        icon = icon("question-circle"),
        nav_panel("User Guide", 
                 div(class = "container mt-4",
                     h3("User Guide"),
                     p("Detailed instructions will go here...")
                 )
        ),
        nav_panel("About",
                 div(class = "container mt-4",
                     h3("About dashResponse"),
                     p("Version information and credits...")
                 )
        ),
        "----", # Separator
        nav_panel("Report Issue",
                 div(class = "container mt-4",
                     h3("Report an Issue"),
                     p("Link to GitHub issues or feedback form...")
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
      app_title = "dashResponse"
    ),
    # Add Bootstrap Icons for better icon support
    tags$link(
      rel = "stylesheet", 
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.0/font/bootstrap-icons.css"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}