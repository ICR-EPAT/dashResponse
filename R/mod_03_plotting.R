#' 03_plotting UI Function
#'
#' @description A shiny Module for interactive data visualization.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' 
#' @importFrom shiny NS tagList selectInput sliderInput actionButton observeEvent moduleServer renderUI req dateRangeInput checkboxInput textInput
#' @importFrom bslib page_sidebar sidebar card card_header card_body navset_card_tab nav_panel value_box popover
#' @importFrom plotly plotlyOutput renderPlotly
mod_03_plotting_ui <- function(id) {
  ns <- NS(id)
  
  page_sidebar(
    # Sidebar for common controls
    sidebar = sidebar(
      title = "Plot Controls",
      width = 300,
      
      # Data status
      uiOutput(ns("data_status")),
      
      # Common plot options
      div(
        h6("Common Options", class = "text-primary fw-bold mb-3"),
        
        selectInput(
          ns("color_scheme"),
          "Color Scheme:",
          choices = list(
            "Default" = "default",
            "Viridis" = "viridis", 
            "Set1" = "set1",
            "Custom" = "custom"
          ),
          selected = "default"
        ),
        
        selectInput(
          ns("theme"),
          "Plot Theme:",
          choices = list(
            "Light" = "light",
            "Dark" = "dark",
            "Minimal" = "minimal",
            "Classic" = "classic"
          ),
          selected = "light"
        ),
        
        sliderInput(
          ns("font_size"),
          "Font Size:",
          min = 8,
          max = 20,
          value = 12,
          step = 1
        )
      ),
      
      # Data filters
      div(
        class = "mt-4",
        h6("Data Filters", class = "text-info fw-bold mb-3"),
        
        uiOutput(ns("patient_filter")),
        uiOutput(ns("treatment_filter")),
        uiOutput(ns("date_filter"))
      ),
      
      # Export options
      div(
        class = "mt-4",
        h6("Export Plot", class = "text-success fw-bold mb-3"),
        
        selectInput(
          ns("export_format"),
          "Format:",
          choices = list("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
          selected = "png"
        ),
        
        div(
          class = "d-grid gap-2",
          actionButton(
            ns("export_plot"),
            "Export Current Plot",
            class = "btn-success btn-sm"
          )
        )
      )
    ),
    
    # Main plotting area
    div(
      class = "h-100",
      
      navset_card_tab(
        id = ns("plot_tabs"),
        
        # Response Over Time Plot
        nav_panel(
          title = "Response Timeline",
          icon = icon("chart-line"),
          
          div(
            class = "p-3",
            
            # Plot header with config popover
            div(
              class = "d-flex justify-content-between align-items-center mb-3",
              h5("Response Over Time", class = "mb-0"),
              popover(
                trigger = actionButton(
                  ns("timeline_config"),
                  icon("cog"),
                  class = "btn-outline-secondary btn-sm"
                ),
                title = "Timeline Plot Options",
                div(
                  textInput(ns("timeline_title"), "Plot Title:", "Response Over Time"),
                  textInput(ns("timeline_subtitle"), "Subtitle:", ""),
                  textInput(ns("timeline_x_label"), "X-axis Label:", "Assessment Date"),
                  textInput(ns("timeline_y_label"), "Y-axis Label:", "Patient"),
                  hr(),
                  checkboxInput(ns("timeline_show_legend"), "Show Legend", TRUE),
                  checkboxInput(ns("timeline_show_grid"), "Show Grid", TRUE),
                  selectInput(
                    ns("timeline_point_size"),
                    "Point Size:",
                    choices = list("Small" = 2, "Medium" = 3, "Large" = 4),
                    selected = 3
                  ),
                  checkboxInput(ns("timeline_connect_points"), "Connect Points", FALSE)
                ),
                placement = "left"
              )
            ),
            
            # Plot output
            plotly::plotlyOutput(ns("timeline_plot"), height = "70vh")
          )
        ),
        
        # Waterfall Plot
        nav_panel(
          title = "Waterfall Plot", 
          icon = icon("chart-bar"),
          
          div(
            class = "p-3",
            
            div(
              class = "d-flex justify-content-between align-items-center mb-3",
              h5("Best Response Waterfall", class = "mb-0"),
              popover(
                trigger = actionButton(
                  ns("waterfall_config"),
                  icon("cog"),
                  class = "btn-outline-secondary btn-sm"
                ),
                title = "Waterfall Plot Options",
                div(
                  textInput(ns("waterfall_title"), "Plot Title:", "Best Response Waterfall"),
                  textInput(ns("waterfall_subtitle"), "Subtitle:", ""),
                  textInput(ns("waterfall_x_label"), "X-axis Label:", "Patient"),
                  textInput(ns("waterfall_y_label"), "Y-axis Label:", "% Change from Baseline"),
                  hr(),
                  checkboxInput(ns("waterfall_sort"), "Sort by Response", TRUE),
                  checkboxInput(ns("waterfall_show_response_line"), "Show Response Thresholds", TRUE),
                  selectInput(
                    ns("waterfall_response_metric"),
                    "Response Metric:",
                    choices = list(
                      "Best % Change" = "best_percent_change",
                      "Best Absolute Change" = "best_absolute_change"
                    ),
                    selected = "best_percent_change"
                  ),
                  checkboxInput(ns("waterfall_color_by_response"), "Color by Response", TRUE)
                ),
                placement = "left"
              )
            ),
            
            plotly::plotlyOutput(ns("waterfall_plot"), height = "70vh")
          )
        ),
        
        # Swimmer Plot
        nav_panel(
          title = "Swimmer Plot",
          icon = icon("swimming-pool"),
          
          div(
            class = "p-3",
            
            div(
              class = "d-flex justify-content-between align-items-center mb-3", 
              h5("Treatment Duration & Response", class = "mb-0"),
              popover(
                trigger = actionButton(
                  ns("swimmer_config"),
                  icon("cog"),
                  class = "btn-outline-secondary btn-sm"
                ),
                title = "Swimmer Plot Options",
                div(
                  textInput(ns("swimmer_title"), "Plot Title:", "Treatment Duration & Response"),
                  textInput(ns("swimmer_subtitle"), "Subtitle:", ""),
                  textInput(ns("swimmer_x_label"), "X-axis Label:", "Time (Days)"),
                  textInput(ns("swimmer_y_label"), "Y-axis Label:", "Patient"),
                  hr(),
                  checkboxInput(ns("swimmer_show_events"), "Show Key Events", TRUE),
                  checkboxInput(ns("swimmer_sort_duration"), "Sort by Duration", TRUE),
                  sliderInput(
                    ns("swimmer_bar_height"),
                    "Bar Height:",
                    min = 0.5,
                    max = 2,
                    value = 0.8,
                    step = 0.1
                  ),
                  checkboxInput(ns("swimmer_show_response_markers"), "Show Response Markers", TRUE)
                ),
                placement = "left"
              )
            ),
            
            plotly::plotlyOutput(ns("swimmer_plot"), height = "70vh")
          )
        ),
        
        # Summary Statistics
        nav_panel(
          title = "Summary",
          icon = icon("chart-pie"),
          
          div(
            class = "p-3",
            
            div(
              class = "row",
              
              div(
                class = "col-md-6",
                card(
                  card_header("Response Distribution"),
                  card_body(
                    plotly::plotlyOutput(ns("response_pie"), height = "300px")
                  )
                )
              ),
              
              div(
                class = "col-md-6", 
                card(
                  card_header("Treatment Summary"),
                  card_body(
                    uiOutput(ns("treatment_summary"))
                  )
                )
              )
            ),
            
            div(
              class = "row mt-3",
              div(
                class = "col-12",
                card(
                  card_header("Response Over Time Heatmap"),
                  card_body(
                    plotly::plotlyOutput(ns("response_heatmap"), height = "400px")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' 03_plotting Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param data_module Reactive data from mod_02_data_selection
#'
#' @noRd
mod_03_plotting_server <- function(id, data_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for plot configurations and filtered data
    values <- reactiveValues(
      filtered_data = NULL,
      plot_configs = list(
        timeline = list(),
        waterfall = list(),
        swimmer = list(),
        summary = list()
      )
    )
    
    # Data status indicator
    output$data_status <- renderUI({
      if (!data_module$is_ready()) {
        div(
          class = "alert alert-warning py-2",
          icon("exclamation-triangle"), 
          strong(" Data Required"), br(),
          span("Please complete data selection first", class = "small")
        )
      } else if (is.null(data_module$data())) {
        div(
          class = "alert alert-danger py-2",
          icon("times-circle"),
          strong(" No Data Available")
        )
      } else {
        summary <- data_module$summary()
        div(
          class = "alert alert-success py-2",
          icon("check-circle"),
          strong(" Data Ready"), br(),
          span(paste(summary$n_patients, "patients,", summary$n_records, "records"), class = "small")
        )
      }
    })
    
    # Dynamic filter controls based on available data
    output$patient_filter <- renderUI({
      req(data_module$data())
      
      patient_choices <- sort(unique(data_module$data()$patient_id))
      
      selectInput(
        ns("selected_patients"),
        "Select Patients:",
        choices = patient_choices,
        selected = patient_choices,
        multiple = TRUE,
        size = min(8, length(patient_choices))
      )
    })
    
    output$treatment_filter <- renderUI({
      req(data_module$data())
      
      if ("treatment_type" %in% names(data_module$data())) {
        treatment_choices <- sort(unique(data_module$data()$treatment_type[!is.na(data_module$data()$treatment_type)]))
        
        if (length(treatment_choices) > 0) {
          selectInput(
            ns("selected_treatments"),
            "Filter Treatments:",
            choices = treatment_choices,
            selected = treatment_choices,
            multiple = TRUE
          )
        }
      }
    })
    
    output$date_filter <- renderUI({
      req(data_module$data())
      
      if ("assessment_date" %in% names(data_module$data())) {
        date_range <- range(data_module$data()$assessment_date, na.rm = TRUE)
        
        dateRangeInput(
          ns("date_range"),
          "Date Range:",
          start = date_range[1],
          end = date_range[2],
          min = date_range[1],
          max = date_range[2]
        )
      }
    })
    
    # Reactive filtered data
    observe({
      req(data_module$data())
      
      filtered <- data_module$data()
      
      # Apply patient filter
      if (!is.null(input$selected_patients)) {
        filtered <- filtered[filtered$patient_id %in% input$selected_patients, ]
      }
      
      # Apply treatment filter
      if (!is.null(input$selected_treatments) && "treatment_type" %in% names(filtered)) {
        filtered <- filtered[filtered$treatment_type %in% input$selected_treatments, ]
      }
      
      # Apply date filter
      if (!is.null(input$date_range) && "assessment_date" %in% names(filtered)) {
        filtered <- filtered[
          filtered$assessment_date >= input$date_range[1] & 
          filtered$assessment_date <= input$date_range[2], 
        ]
      }
      
      values$filtered_data <- filtered
    })
    
    # Update plot configurations when inputs change
    observe({
      values$plot_configs$timeline <- list(
        title = input$timeline_title %||% "Response Over Time",
        subtitle = input$timeline_subtitle %||% "",
        x_label = input$timeline_x_label %||% "Assessment Date",
        y_label = input$timeline_y_label %||% "Patient",
        show_legend = input$timeline_show_legend %||% TRUE,
        show_grid = input$timeline_show_grid %||% TRUE,
        point_size = as.numeric(input$timeline_point_size %||% 3),
        connect_points = input$timeline_connect_points %||% FALSE,
        color_scheme = input$color_scheme %||% "default",
        theme = input$theme %||% "light",
        font_size = input$font_size %||% 12
      )
    })
    
    observe({
      values$plot_configs$waterfall <- list(
        title = input$waterfall_title %||% "Best Response Waterfall",
        subtitle = input$waterfall_subtitle %||% "",
        x_label = input$waterfall_x_label %||% "Patient",
        y_label = input$waterfall_y_label %||% "% Change from Baseline",
        sort = input$waterfall_sort %||% TRUE,
        show_response_line = input$waterfall_show_response_line %||% TRUE,
        response_metric = input$waterfall_response_metric %||% "best_percent_change",
        color_by_response = input$waterfall_color_by_response %||% TRUE,
        color_scheme = input$color_scheme %||% "default",
        theme = input$theme %||% "light",
        font_size = input$font_size %||% 12
      )
    })
    
    observe({
      values$plot_configs$swimmer <- list(
        title = input$swimmer_title %||% "Treatment Duration & Response",
        subtitle = input$swimmer_subtitle %||% "",
        x_label = input$swimmer_x_label %||% "Time (Days)",
        y_label = input$swimmer_y_label %||% "Patient",
        show_events = input$swimmer_show_events %||% TRUE,
        sort_duration = input$swimmer_sort_duration %||% TRUE,
        bar_height = input$swimmer_bar_height %||% 0.8,
        show_response_markers = input$swimmer_show_response_markers %||% TRUE,
        color_scheme = input$color_scheme %||% "default",
        theme = input$theme %||% "light",
        font_size = input$font_size %||% 12
      )
    })
    
    # Timeline plot
    output$timeline_plot <- plotly::renderPlotly({
      req(values$filtered_data, nrow(values$filtered_data) > 0)
      
      config <- values$plot_configs$timeline
      
      # This will eventually call a business logic function
      # plot_response_timeline(values$filtered_data, config)
      
      # Placeholder implementation
      p <- plotly::plot_ly(
        data = values$filtered_data,
        x = ~assessment_date,
        y = ~patient_id,
        color = ~response,
        type = "scatter",
        mode = if (config$connect_points) "lines+markers" else "markers",
        marker = list(size = config$point_size * 3)
      ) |>
        plotly::layout(
          title = list(text = config$title, font = list(size = config$font_size + 4)),
          xaxis = list(title = config$x_label, showgrid = config$show_grid),
          yaxis = list(title = config$y_label, showgrid = config$show_grid),
          showlegend = config$show_legend,
          font = list(size = config$font_size)
        )
      
      # Apply theme
      if (config$theme == "dark") {
        p <- p |> plotly::layout(
          paper_bgcolor = "black",
          plot_bgcolor = "black",
          font = list(color = "white")
        )
      }
      
      p
    })
    
    # Waterfall plot
    output$waterfall_plot <- plotly::renderPlotly({
      req(values$filtered_data, nrow(values$filtered_data) > 0)
      
      config <- values$plot_configs$waterfall
      
      # Placeholder implementation - calculate best response per patient
      if ("percent_change" %in% names(values$filtered_data)) {
        waterfall_data <- values$filtered_data |>
          dplyr::group_by(patient_id) |>
          dplyr::summarise(
            best_change = min(percent_change, na.rm = TRUE),
            best_response = dplyr::first(response[which.min(percent_change)]),
            .groups = "drop"
          ) |>
          dplyr::filter(!is.infinite(best_change))
        
        if (config$sort) {
          waterfall_data <- waterfall_data |> dplyr::arrange(best_change)
        }
        
        colors <- if (config$color_by_response) ~best_response else "steelblue"
        
        p <- plotly::plot_ly(
          data = waterfall_data,
          x = ~factor(patient_id, levels = patient_id),
          y = ~best_change,
          color = colors,
          type = "bar",
          text = ~paste("Patient:", patient_id, "<br>Change:", round(best_change, 1), "%<br>Response:", best_response),
          textposition = "none",
          hovertemplate = "%{text}<extra></extra>"
        ) |>
          plotly::layout(
            title = list(text = config$title, font = list(size = config$font_size + 4)),
            xaxis = list(title = config$x_label),
            yaxis = list(title = config$y_label),
            font = list(size = config$font_size)
          )
        
        # Add response threshold lines
        if (config$show_response_line) {
          p <- p |>
            plotly::add_lines(y = -30, line = list(color = "green", dash = "dash"), 
                             annotation = list(text = "PR threshold (-30%)", x = 0.02, xref = "paper")) |>
            plotly::add_lines(y = 20, line = list(color = "red", dash = "dash"),
                             annotation = list(text = "PD threshold (+20%)", x = 0.02, xref = "paper"))
        }
        
        p
      } else {
        # Fallback if no response value data
        plotly::plot_ly() |>
          plotly::add_annotations(
            text = "No response value data available for waterfall plot",
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE
          )
      }
    })
    
    # Swimmer plot
    output$swimmer_plot <- plotly::renderPlotly({
      req(values$filtered_data, nrow(values$filtered_data) > 0)
      
      config <- values$plot_configs$swimmer
      
      # Calculate treatment duration per patient
      swimmer_data <- values$filtered_data |>
        dplyr::group_by(patient_id) |>
        dplyr::summarise(
          start_date = min(treatment_start, na.rm = TRUE),
          end_date = max(treatment_end, na.rm = TRUE),
          duration = as.numeric(max(treatment_end, na.rm = TRUE) - min(treatment_start, na.rm = TRUE)),
          .groups = "drop"
        ) |>
        dplyr::filter(!is.na(start_date), !is.na(end_date))
      
      if (nrow(swimmer_data) == 0) {
        return(plotly::plot_ly() |>
          plotly::add_annotations(
            text = "No treatment duration data available",
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE
          ))
      }
      
      if (config$sort_duration) {
        swimmer_data <- swimmer_data |> dplyr::arrange(desc(duration))
      }
      
      swimmer_data$y_pos <- seq_len(nrow(swimmer_data))
      
      p <- plotly::plot_ly(swimmer_data) |>
        plotly::add_segments(
          x = ~start_date, xend = ~end_date,
          y = ~y_pos, yend = ~y_pos,
          line = list(width = config$bar_height * 20),
          color = I("steelblue"),
          text = ~paste("Patient:", patient_id, "<br>Duration:", duration, "days"),
          hovertemplate = "%{text}<extra></extra>"
        ) |>
        plotly::layout(
          title = list(text = config$title, font = list(size = config$font_size + 4)),
          xaxis = list(title = config$x_label),
          yaxis = list(
            title = config$y_label,
            tickvals = swimmer_data$y_pos,
            ticktext = swimmer_data$patient_id
          ),
          font = list(size = config$font_size)
        )
      
      p
    })
    
    # Response pie chart
    output$response_pie <- plotly::renderPlotly({
      req(values$filtered_data, nrow(values$filtered_data) > 0)
      
      response_counts <- table(values$filtered_data$response)
      
      plotly::plot_ly(
        labels = names(response_counts),
        values = as.numeric(response_counts),
        type = "pie",
        textinfo = "label+percent",
        textposition = "inside"
      ) |>
        plotly::layout(
          font = list(size = input$font_size %||% 12)
        )
    })
    
    # Treatment summary
    output$treatment_summary <- renderUI({
      req(values$filtered_data)
      
      summary <- data_module$summary()
      n_patients <- length(unique(values$filtered_data$patient_id))
      n_assessments <- nrow(values$filtered_data)
      
      div(
        value_box(
          title = "Filtered Patients",
          value = n_patients,
          showcase = icon("users"),
          theme = "primary",
          height = "80px"
        ),
        br(),
        value_box(
          title = "Filtered Assessments", 
          value = n_assessments,
          showcase = icon("clipboard-list"),
          theme = "info",
          height = "80px"
        )
      )
    })
    
    # Response heatmap (placeholder)
    output$response_heatmap <- plotly::renderPlotly({
      req(values$filtered_data, nrow(values$filtered_data) > 0)
      
      # Placeholder heatmap - could show response over time
      plotly::plot_ly(
        z = ~matrix(rnorm(100), nrow = 10),
        type = "heatmap",
        colorscale = "Viridis"
      ) |>
        plotly::layout(
          title = "Response Timeline Heatmap (Placeholder)",
          font = list(size = input$font_size %||% 12)
        )
    })
    
    # Export functionality
    observeEvent(input$export_plot, {
      showNotification("Plot export functionality to be implemented", type = "message")
    })
    
    # Return plot configurations for code export module
    return(
      list(
        configs = reactive(values$plot_configs),
        filtered_data = reactive(values$filtered_data),
        current_tab = reactive(input$plot_tabs)
      )
    )
  })
}
