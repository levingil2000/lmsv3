# Enhanced Facilities Module with AI Report Generation

# Source the AI report generation functions
source("modules/enrollment/enrollment_dashboard/ai_helpers.R")
source("modules/enrollment/enrollment_dashboard/report_document_helpers.R")
source("modules/facilities/facilities_report_helpers.R") 

facilities_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1("Facilities Management", style = "color: #2c3e50; margin-bottom: 30px;"),
    
    tabsetPanel(
      # Facilities Registry Tab
      tabPanel("Facilities Registry",
               br(),
               fluidRow(
                 column(12,
                        actionButton(ns("add_facility"), "Add New Facility", 
                                     class = "btn-primary", style = "margin-bottom: 15px;")
                 )
               ),
               
               # Filters Row
               fluidRow(
                 column(3,
                        selectInput(ns("filter_status"), "Filter by Status:",
                                    choices = c("All" = "", "Available", "In Use", "Under Maintenance", "Damaged"),
                                    selected = "")
                 ),
                 column(3,
                        selectInput(ns("filter_type"), "Filter by Type:",
                                    choices = NULL, selected = NULL)
                 ),
                 column(6,
                        searchInput(ns("search_facilities"), "Search Facilities:",
                                    placeholder = "Search by name or description...")
                 )
               ),
               
               br(),
               
               # Summary Cards
               fluidRow(
                 column(3,
                        div(class = "value-box",
                            h3(textOutput(ns("total_facilities"))),
                            p("Total Facilities")
                        )
                 ),
                 column(3,
                        div(class = "value-box",
                            h3(textOutput(ns("available_facilities"))),
                            p("Available")
                        )
                 ),
                 column(3,
                        div(class = "value-box",
                            h3(textOutput(ns("in_use_facilities"))),
                            p("In Use")
                        )
                 ),
                 column(3,
                        div(class = "value-box",
                            h3(textOutput(ns("total_value"))),
                            p("Total Value")
                        )
                 )
               ),
               
               br(),
               
               # Facilities Table
               fluidRow(
                 column(12,
                        withSpinner(DT::dataTableOutput(ns("facilities_table")))
                 )
               )
      ),
      
      # Facilities Dashboard Tab
      tabPanel("Facilities Dashboard",
               br(),
               
               # Enhanced Summary Cards
               fluidRow(
                 column(2,
                        div(class = "value-box",
                            h3(textOutput(ns("dash_total_facilities"))),
                            p("Total Facilities")
                        )
                 ),
                 column(2,
                        div(class = "value-box",
                            h3(textOutput(ns("dash_available_facilities"))),
                            p("Available")
                        )
                 ),
                 column(2,
                        div(class = "value-box",
                            h3(textOutput(ns("dash_in_use_facilities"))),
                            p("In Use")
                        )
                 ),
                 column(2,
                        div(class = "value-box",
                            h3(textOutput(ns("dash_maintenance_facilities"))),
                            p("Maintenance")
                        )
                 ),
                 column(2,
                        div(class = "value-box",
                            h3(textOutput(ns("dash_damaged_facilities"))),
                            p("Damaged")
                        )
                 ),
                 column(2,
                        div(class = "value-box",
                            h3(textOutput(ns("dash_total_value"))),
                            p("Total Value")
                        )
                 )
               ),
               
               br(),
               
               # Charts Row
               fluidRow(
                 column(6,
                        wellPanel(
                          h4("Facility Status Distribution", style = "color: #2c3e50;"),
                          withSpinner(plotlyOutput(ns("status_distribution_chart")))
                        )
                 ),
                 column(6,
                        wellPanel(
                          h4("Facility Types Overview", style = "color: #2c3e50;"),
                          withSpinner(plotlyOutput(ns("facility_types_chart")))
                        )
                 )
               ),
               
               br(),
               
               fluidRow(
                 column(6,
                        wellPanel(
                          h4("High-Value Facilities", style = "color: #2c3e50;"),
                          withSpinner(plotlyOutput(ns("high_value_chart")))
                        )
                 ),
                 column(6,
                        wellPanel(
                          h4("Asset Value by Status", style = "color: #2c3e50;"),
                          withSpinner(plotlyOutput(ns("value_by_status_chart")))
                        )
                 )
               )
      ),
      
      # AI Report Analysis Tab
      tabPanel("AI Report Analysis",
               br(),
               fluidRow(
                 column(12,
                        wellPanel(
                          h4("AI-Generated Facilities Analysis", style = "color: #2c3e50;"),
                          p("Generate comprehensive AI-powered reports analyzing facility utilization, asset management, maintenance patterns, and strategic recommendations."),
                          br(),
                          actionButton(ns("generate_full_report"), "Generate Complete AI Analysis Report", 
                                       class = "btn-success btn-lg", 
                                       icon = icon("robot"),
                                       style = "width: 100%; margin-bottom: 20px;"),
                          
                          conditionalPanel(
                            condition = paste0("output['", ns("show_analysis"), "']"),
                            wellPanel(
                              h5("AI Analysis Preview:", style = "color: #34495e;"),
                              div(id = ns("analysis_content"),
                                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; max-height: 400px; overflow-y: auto;",
                                  htmlOutput(ns("ai_analysis_preview"))
                              )
                            )
                          ),
                          
                          conditionalPanel(
                            condition = paste0("output['", ns("show_download"), "']"),
                            br(),
                            downloadButton(ns("download_report"), "Download Full Report (.docx)", 
                                           class = "btn-primary btn-lg",
                                           style = "width: 100%;")
                          )
                        )
                 )
               )
      )
    ),
    
    # Add/Edit Facility Modal
    bsModal("facility_modal", "Facility Information", ns("add_facility"), size = "medium",
            fluidRow(
              column(6,
                     textInput(ns("FR_ID"), "Facility ID:", ""),
                     textInput(ns("facility_name"), "Facility Name:", ""),
                     textAreaInput(ns("description"), "Description:", "", rows = 3),
                     selectInput(ns("status"), "Status:",
                                 choices = c("Available", "In Use", "Under Maintenance", "Damaged"),
                                 selected = "Available")
              ),
              column(6,
                     textInput(ns("facility_type"), "Type:", ""),
                     numericInput(ns("amount"), "Amount/Value:", value = 0, min = 0)
              )
            ),
            
            footer = tagList(
              actionButton(ns("save_facility"), "Save", class = "btn-primary"),
              actionButton(ns("delete_facility"), "Delete", class = "btn-danger", style = "display: none;"),
              actionButton(ns("cancel_facility"), "Cancel", class = "btn-default")
            )
    )
  )
}
facilities_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      facilities_data = NULL,
      filtered_data = NULL,
      editing_id = NULL,
      refresh = 0,
      report_result = NULL,
      ai_analysis = NULL
    )
    
    # Load facilities data
    observe({
      values$refresh  # Dependency for refresh
      values$facilities_data <- get_table_data(con, "facilities_table")
    })
    
    # Update filter choices
    observe({
      req(values$facilities_data)
      
      # Types
      types <- unique(values$facilities_data$type[!is.na(values$facilities_data$type)])
      updateSelectInput(session, "filter_type",
                        choices = c("All" = "", types))
    })
    
    # Filter facilities data
    observe({
      req(values$facilities_data)
      
      filtered <- values$facilities_data
      
      # Apply status filter
      if (!is.null(input$filter_status) && input$filter_status != "") {
        filtered <- filtered[filtered$status == input$filter_status, ]
      }
      
      # Apply type filter
      if (!is.null(input$filter_type) && input$filter_type != "") {
        filtered <- filtered[filtered$type == input$filter_type, ]
      }
      
      # Apply search filter
      if (!is.null(input$search_facilities) && input$search_facilities != "") {
        search_term <- tolower(input$search_facilities)
        filtered <- filtered[
          grepl(search_term, tolower(filtered$name)) | 
            grepl(search_term, tolower(filtered$description)),
        ]
      }
      
      values$filtered_data <- filtered
    })
    
    # Registry Tab Summary statistics
    output$total_facilities <- renderText({
      req(values$filtered_data)
      nrow(values$filtered_data)
    })
    
    output$available_facilities <- renderText({
      req(values$filtered_data)
      sum(values$filtered_data$status == "Available", na.rm = TRUE)
    })
    
    output$in_use_facilities <- renderText({
      req(values$filtered_data)
      sum(values$filtered_data$status == "In Use", na.rm = TRUE)
    })
    
    output$total_value <- renderText({
      req(values$filtered_data)
      total <- sum(values$filtered_data$amount, na.rm = TRUE)
      paste("₱", format(total, big.mark = ",", nsmall = 2))
    })
    
    # Dashboard Tab Summary statistics (using all data, not filtered)
    output$dash_total_facilities <- renderText({
      req(values$facilities_data)
      nrow(values$facilities_data)
    })
    
    output$dash_available_facilities <- renderText({
      req(values$facilities_data)
      sum(values$facilities_data$status == "Available", na.rm = TRUE)
    })
    
    output$dash_in_use_facilities <- renderText({
      req(values$facilities_data)
      sum(values$facilities_data$status == "In Use", na.rm = TRUE)
    })
    
    output$dash_maintenance_facilities <- renderText({
      req(values$facilities_data)
      sum(values$facilities_data$status == "Under Maintenance", na.rm = TRUE)
    })
    
    output$dash_damaged_facilities <- renderText({
      req(values$facilities_data)
      sum(values$facilities_data$status == "Damaged", na.rm = TRUE)
    })
    
    output$dash_total_value <- renderText({
      req(values$facilities_data)
      total <- sum(values$facilities_data$amount, na.rm = TRUE)
      paste("₱", format(total, big.mark = ","))
    })
    
    # Facilities table
    output$facilities_table <- DT::renderDataTable({
      req(values$filtered_data)
      
      display_data <- values$filtered_data %>%
        select(FR_ID, name, description, status, type, amount) %>%
        mutate(amount = ifelse(is.na(amount), 0, amount))
      
      DT::datatable(display_data,
                    options = list(
                      pageLength = 10, 
                      scrollX = TRUE,
                      columnDefs = list(
                        list(targets = c(5), render = DT::JS(
                          "function(data, type, row, meta) {",
                          "return '₱' + parseFloat(data).toLocaleString();",
                          "}"
                        ))
                      )
                    ),
                    rownames = FALSE,
                    selection = "single") %>%
        DT::formatStyle(columns = "status",
                        backgroundColor = DT::styleEqual(
                          c("Available", "In Use", "Under Maintenance", "Damaged"),
                          c("#d4edda", "#fff3cd", "#cce5ff", "#f8d7da")
                        ))
    })
    
    # Dashboard Charts
    # Status Distribution Chart
    output$status_distribution_chart <- renderPlotly({
      req(values$facilities_data)
      
      status_counts <- values$facilities_data %>%
        count(status) %>%
        arrange(desc(n))
      
      if (nrow(status_counts) > 0) {
        colors <- c("Available" = "#2ecc71", "In Use" = "#f39c12", 
                    "Under Maintenance" = "#3498db", "Damaged" = "#e74c3c")
        
        p <- plot_ly(status_counts, 
                     labels = ~status, 
                     values = ~n,
                     type = 'pie',
                     marker = list(colors = colors[status_counts$status]),
                     hovertemplate = '<b>%{label}</b><br>Count: %{value}<br>Percentage: %{percent}<extra></extra>') %>%
          layout(title = "")
        p
      } else {
        plotly_empty() %>% layout(title = "No data available")
      }
    })
    
    # Facility Types Chart
    output$facility_types_chart <- renderPlotly({
      req(values$facilities_data)
      
      type_counts <- values$facilities_data %>%
        filter(!is.na(type) & type != "") %>%
        count(type, sort = TRUE) %>%
        head(10)
      
      if (nrow(type_counts) > 0) {
        p <- plot_ly(type_counts, 
                     x = ~reorder(type, n), 
                     y = ~n,
                     type = 'bar',
                     marker = list(color = '#9b59b6'),
                     hovertemplate = '<b>%{x}</b><br>Count: %{y}<extra></extra>') %>%
          layout(title = "",
                 xaxis = list(title = "Facility Type"),
                 yaxis = list(title = "Count"))
        p
      } else {
        plotly_empty() %>% layout(title = "No data available")
      }
    })
    
    # High-Value Facilities Chart
    output$high_value_chart <- renderPlotly({
      req(values$facilities_data)
      
      high_value <- values$facilities_data %>%
        filter(!is.na(amount) & amount > 0) %>%
        arrange(desc(amount)) %>%
        head(10)
      
      if (nrow(high_value) > 0) {
        p <- plot_ly(high_value, 
                     x = ~reorder(name, amount), 
                     y = ~amount,
                     type = 'bar',
                     marker = list(color = '#e67e22'),
                     hovertemplate = '<b>%{x}</b><br>Value: ₱%{y}<extra></extra>') %>%
          layout(title = "",
                 xaxis = list(title = "Facility"),
                 yaxis = list(title = "Value (₱)"))
        p
      } else {
        plotly_empty() %>% layout(title = "No data available")
      }
    })
    
    # Asset Value by Status Chart
    output$value_by_status_chart <- renderPlotly({
      req(values$facilities_data)
      
      value_by_status <- values$facilities_data %>%
        group_by(status) %>%
        summarise(total_value = sum(amount, na.rm = TRUE), .groups = 'drop') %>%
        filter(total_value > 0) %>%
        arrange(desc(total_value))
      
      if (nrow(value_by_status) > 0) {
        colors <- c("Available" = "#2ecc71", "In Use" = "#f39c12", 
                    "Under Maintenance" = "#3498db", "Damaged" = "#e74c3c")
        
        p <- plot_ly(value_by_status, 
                     x = ~status, 
                     y = ~total_value,
                     type = 'bar',
                     marker = list(color = ~colors[status]),
                     hovertemplate = '<b>%{x}</b><br>Total Value: ₱%{y}<extra></extra>') %>%
          layout(title = "",
                 xaxis = list(title = "Status"),
                 yaxis = list(title = "Total Value (₱)"))
        p
      } else {
        plotly_empty() %>% layout(title = "No data available")
      }
    })
    
    # Handle row selection for editing
    observeEvent(input$facilities_table_rows_selected, {
      if (length(input$facilities_table_rows_selected) > 0) {
        selected_row <- input$facilities_table_rows_selected
        facility_data <- values$filtered_data[selected_row, ]
        
        values$editing_id <- facility_data$id
        
        # Populate modal fields
        updateTextInput(session, "FR_ID", value = facility_data$FR_ID)
        updateTextInput(session, "facility_name", value = facility_data$name)
        updateTextAreaInput(session, "description", value = facility_data$description)
        updateSelectInput(session, "status", selected = facility_data$status)
        updateTextInput(session, "facility_type", value = facility_data$type)
        updateNumericInput(session, "amount", value = ifelse(is.na(facility_data$amount), 0, facility_data$amount))
        
        # Show delete button
        shinyjs::show("delete_facility")
        
        toggleModal(session, "facility_modal", toggle = "open")
      }
    })
    
    # Handle add new facility
    observeEvent(input$add_facility, {
      values$editing_id <- NULL
      
      # Generate new facility ID
      facility_id <- paste0("FAC_", format(Sys.Date(), "%Y%m%d"), "_", sprintf("%04d", sample(1:9999, 1)))
      
      # Clear modal fields
      updateTextInput(session, "FR_ID", value = facility_id)
      updateTextInput(session, "facility_name", value = "")
      updateTextAreaInput(session, "description", value = "")
      updateSelectInput(session, "status", selected = "Available")
      updateTextInput(session, "facility_type", value = "")
      updateNumericInput(session, "amount", value = 0)
      
      # Hide delete button
      shinyjs::hide("delete_facility")
    })
    
    # Handle save facility
    observeEvent(input$save_facility, {
      req(input$FR_ID, input$facility_name)
      
      facility_data <- data.frame(
        FR_ID = input$FR_ID,
        name = input$facility_name,
        description = input$description,
        status = input$status,
        type = input$facility_type,
        amount = input$amount,
        stringsAsFactors = FALSE
      )
      
      if (is.null(values$editing_id)) {
        # Insert new facility
        result <- insert_data(con, "facilities_table", facility_data)
      } else {
        # Update existing facility
        result <- update_data(con, "facilities_table", facility_data, "id", values$editing_id)
      }
      
      if (result$success) {
        showNotification(result$message, type = "message")
        values$refresh <- values$refresh + 1
        toggleModal(session, "facility_modal", toggle = "close")
      } else {
        showNotification(result$message, type = "error")
      }
    })
    
    # Handle delete facility
    observeEvent(input$delete_facility, {
      req(values$editing_id)
      
      showModal(modalDialog(
        title = "Confirm Deletion",
        "Are you sure you want to delete this facility? This action cannot be undone.",
        footer = tagList(
          actionButton(ns("confirm_delete"), "Yes, Delete", class = "btn-danger"),
          modalButton("Cancel")
        )
      ))
    })
    
    # Handle confirm delete
    observeEvent(input$confirm_delete, {
      req(values$editing_id)
      
      result <- delete_data(con, "facilities_table", "id", values$editing_id)
      
      if (result$success) {
        showNotification(result$message, type = "message")
        values$refresh <- values$refresh + 1
        removeModal()
        toggleModal(session, "facility_modal", toggle = "close")
      } else {
        showNotification(result$message, type = "error")
      }
    })
    
    # Handle cancel
    observeEvent(input$cancel_facility, {
      toggleModal(session, "facility_modal", toggle = "close")
    })
    
    # =============================================================================
    # AI REPORT GENERATION FUNCTIONALITY
    # =============================================================================
    
    # Handle full report generation (AI Report Analysis tab)
    observeEvent(input$generate_full_report, {
      req(values$facilities_data)
      
      if (nrow(values$facilities_data) == 0) {
        showNotification("No facility data available for analysis.", type = "warning")
        return()
      }
      
      withProgress(message = 'Generating AI Analysis...', value = 0, {
        incProgress(0.2, detail = "Preparing facility data...")
        
        tryCatch({
          incProgress(0.3, detail = "Running AI analysis...")
          
          result <- handle_facilities_report_generation(values$facilities_data, "Project ARAL Management System")
          
          incProgress(0.8, detail = "Finalizing report...")
          
          if (result$success) {
            values$report_result <- result
            values$ai_analysis <- result$ai_analysis
            
            incProgress(1.0, detail = "Complete!")
            
            showNotification("AI analysis completed successfully!", type = "success")
          } else {
            showNotification(paste("Analysis failed:", result$message), type = "error")
          }
          
        }, error = function(e) {
          showNotification(paste("Error generating analysis:", e$message), type = "error")
        })
      })
    })
    
    # Show analysis content conditionally
    output$show_analysis <- reactive({
      !is.null(values$ai_analysis)
    })
    outputOptions(output, "show_analysis", suspendWhenHidden = FALSE)
    
    # Show download button conditionally
    output$show_download <- reactive({
      !is.null(values$report_result) && values$report_result$success
    })
    outputOptions(output, "show_download", suspendWhenHidden = FALSE)
    
    # Render AI analysis preview
    output$ai_analysis_preview <- renderText({
      req(values$ai_analysis)
      
      preview_html <- paste0(
        "<h5 style='color: #2c3e50; margin-bottom: 15px;'>Executive Summary</h5>",
        "<p style='text-align: justify; margin-bottom: 20px;'>", values$ai_analysis$executive_summary, "</p>",
        
        "<h5 style='color: #2c3e50; margin-bottom: 15px;'>Utilization Analysis</h5>",
        "<p style='text-align: justify; margin-bottom: 20px;'>", 
        substr(values$ai_analysis$utilization_analysis, 1, 300), "...</p>",
        
        "<h5 style='color: #2c3e50; margin-bottom: 15px;'>Asset Management Insights</h5>",
        "<p style='text-align: justify; margin-bottom: 20px;'>", 
        substr(values$ai_analysis$asset_management, 1, 300), "...</p>",
        
        "<h5 style='color: #2c3e50; margin-bottom: 15px;'>Strategic Recommendations</h5>",
        "<p style='text-align: justify;'>", 
        substr(values$ai_analysis$recommendations, 1, 400), "...</p>",
        
        "<div style='margin-top: 20px; padding: 10px; background-color: #e8f6f3; border-left: 4px solid #2ecc71;'>",
        "<strong>🏢 Full detailed analysis including maintenance insights and facility directory is available in the downloadable Word document.</strong>",
        "</div>"
      )
      
      preview_html
    })
    
    # Download handler for the generated report
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("Facilities_Analysis_Report_", format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        req(values$report_result, values$report_result$success)
        
        # Copy the temporary file to the download location
        file.copy(values$report_result$file_path, file)
      },
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    )
    
    # Clean up temporary files when session ends
    session$onSessionEnded(function() {
      if (!is.null(values$report_result) && values$report_result$success) {
        if (file.exists(values$report_result$file_path)) {
          file.remove(values$report_result$file_path)
        }
      }
    })
  })
}