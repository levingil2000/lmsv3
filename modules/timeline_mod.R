# timeline_ui.R



timeline_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1("Event Timeline Management", style = "color: #2c3e50; margin-bottom: 30px;"),
    
    tabsetPanel(
      # Events Management Tab
      tabPanel("Events Management",
               br(),
               fluidRow(
                 column(12,
                        actionButton(ns("add_event"), "Add New Event", 
                                     class = "btn-primary", style = "margin-bottom: 15px;")
                 )
               ),
               
               fluidRow(
                 column(12,
                        withSpinner(DT::dataTableOutput(ns("events_table")))
                 )
               )
      ),
      
      # Events Analytics Tab
      tabPanel("Events Analytics",
               br(),
               
               # Filters
               fluidRow(
                 column(4,
                        selectInput(ns("filter_status"), "Event Status:",
                                    choices = NULL, selected = NULL)
                 ),
                 column(4,
                        dateRangeInput(ns("filter_date_range"), "Date Range:",
                                       start = Sys.Date() - 30, 
                                       end = Sys.Date() + 30)
                 ),
                 column(4,
                        br(),
                        downloadButton(ns("download_csv"), "Download CSV", 
                                       class = "btn-success", style = "margin-top: 5px;")
                 )
               ),
               
               # First row of charts
               fluidRow(
                 column(6,
                        wellPanel(
                          h4("Events by Status Distribution", style = "color: #2c3e50;"),
                          withSpinner(plotlyOutput(ns("status_distribution_chart")))
                        )
                 ),
                 column(6,
                        wellPanel(
                          h4("Events Timeline Overview", style = "color: #2c3e50;"),
                          withSpinner(plotlyOutput(ns("timeline_overview_chart")))
                        )
                 )
               ),
               
               # Events Analysis Section
               fluidRow(
                 column(6,
                        wellPanel(
                          h4("Event Status Summary", style = "color: #2c3e50;"),
                          br(),
                          withSpinner(DT::dataTableOutput(ns("status_summary_table")))
                        )
                 ),
                 column(6,
                        wellPanel(
                          h4("Monthly Event Trends", style = "color: #2c3e50;"),
                          br(),
                          withSpinner(plotlyOutput(ns("monthly_trends_chart")))
                        )
                 )
               ),
               
               br(),
               
               fluidRow(
                 column(12,
                        wellPanel(
                          h4("Detailed Events Summary", style = "color: #2c3e50;"),
                          withSpinner(DT::dataTableOutput(ns("events_summary_table")))
                        )
                 )
               )
      ),
      
      # Timeline Visualization Tab
      tabPanel("Timeline Visualization",
               br(),
               fluidRow(
                 column(12,
                        wellPanel(
                          h4("Interactive Events Timeline", style = "color: #2c3e50; text-align: center;"),
                          p("Visual representation of all events in chronological order", 
                            style = "text-align: center; color: #7f8c8d;"),
                          br(),
                          withSpinner(htmlOutput(ns("timeline_visual")))
                        )
                 )
               )
      )
    ),
    
    # Add/Edit Event Modal
    bsModal("event_modal", "Event Information", ns("add_event"), size = "large",
            fluidRow(
              column(6,
                     textInput(ns("event_id"), "Event ID:", "", placeholder = "e.g., EVT001"),
                     dateInput(ns("event_date"), "Event Date:", value = Sys.Date()),
                     textAreaInput(ns("event_description"), "Description:", "", rows = 3)
              ),
              column(6,
                     textInput(ns("event_persons_involved"), "Persons Involved:", ""),
                     selectInput(ns("event_status"), "Status:",
                                 choices = c("Pending", "In Progress", "Completed", "Cancelled"),
                                 selected = "Pending"),
                     textAreaInput(ns("event_remarks"), "Remarks:", "", rows = 3)
              )
            ),
            
            footer = tagList(
              actionButton(ns("save_event"), "Save", class = "btn-primary"),
              actionButton(ns("cancel_event"), "Cancel", class = "btn-default")
            )
    )
  )
}

timeline_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      events_data = NULL,
      editing_id = NULL,
      refresh = 0
    )
    
    # Initialize database table
    observe({
      tryCatch({
        dbExecute(con, "
          CREATE TABLE IF NOT EXISTS events (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            event_id TEXT UNIQUE,
            event_date DATE,
            event_description TEXT,
            event_persons_involved TEXT,
            event_status TEXT,
            event_remarks TEXT,
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP
          )
        ")
      }, error = function(e) {
        showNotification(paste("Database error:", e$message), type = "error")
      })
    })
    
    # Load events data
    observe({
      values$refresh  # Dependency for refresh
      tryCatch({
        values$events_data <- dbGetQuery(con, "SELECT * FROM events ORDER BY event_date DESC")
      }, error = function(e) {
        showNotification(paste("Error loading events:", e$message), type = "error")
        values$events_data <- data.frame()
      })
    })
    
    # Update filter choices
    observe({
      if (!is.null(values$events_data) && nrow(values$events_data) > 0) {
        statuses <- sort(unique(values$events_data$event_status))
        updateSelectInput(session, "filter_status", 
                          choices = c("All" = "", statuses))
      }
    })
    
    # Events table
    output$events_table <- DT::renderDataTable({
      req(values$events_data)
      
      display_data <- values$events_data %>%
        select(event_id, event_date, event_description, event_persons_involved, 
               event_status, event_remarks) %>%
        rename(
          "Event ID" = event_id,
          "Date" = event_date,
          "Description" = event_description,
          "Persons Involved" = event_persons_involved,
          "Status" = event_status,
          "Remarks" = event_remarks
        )
      
      DT::datatable(display_data,
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE,
                    selection = "single") %>%
        DT::formatStyle(columns = "Status",
                        backgroundColor = DT::styleEqual(
                          c("Pending", "In Progress", "Completed", "Cancelled"), 
                          c("#fff3cd", "#cce5ff", "#d4edda", "#f8d7da")
                        ))
    })
    
    # Handle row selection for editing
    observeEvent(input$events_table_rows_selected, {
      if (length(input$events_table_rows_selected) > 0) {
        selected_row <- input$events_table_rows_selected
        event_data <- values$events_data[selected_row, ]
        
        values$editing_id <- event_data$id
        
        # Populate modal fields
        updateTextInput(session, "event_id", value = event_data$event_id)
        updateDateInput(session, "event_date", value = as.Date(event_data$event_date))
        updateTextAreaInput(session, "event_description", value = event_data$event_description)
        updateTextInput(session, "event_persons_involved", value = event_data$event_persons_involved)
        updateSelectInput(session, "event_status", selected = event_data$event_status)
        updateTextAreaInput(session, "event_remarks", value = event_data$event_remarks)
        
        toggleModal(session, "event_modal", toggle = "open")
      }
    })
    
    # Handle add new event
    observeEvent(input$add_event, {
      values$editing_id <- NULL
      
      # Clear modal fields
      updateTextInput(session, "event_id", value = "")
      updateDateInput(session, "event_date", value = Sys.Date())
      updateTextAreaInput(session, "event_description", value = "")
      updateTextInput(session, "event_persons_involved", value = "")
      updateSelectInput(session, "event_status", selected = "Pending")
      updateTextAreaInput(session, "event_remarks", value = "")
    })
    
    # Handle save event
    observeEvent(input$save_event, {
      req(input$event_id, input$event_date, input$event_description, input$event_status)
      
      tryCatch({
        if (is.null(values$editing_id)) {
          # Insert new event
          dbExecute(con, "
            INSERT INTO events (event_id, event_date, event_description, 
                              event_persons_involved, event_status, event_remarks)
            VALUES (?, ?, ?, ?, ?, ?)
          ", params = list(
            input$event_id,
            as.character(input$event_date),
            input$event_description,
            input$event_persons_involved,
            input$event_status,
            input$event_remarks
          ))
          
          showNotification("Event added successfully!", type = "message")
        } else {
          # Update existing event
          dbExecute(con, "
            UPDATE events 
            SET event_id = ?, event_date = ?, event_description = ?, 
                event_persons_involved = ?, event_status = ?, event_remarks = ?
            WHERE id = ?
          ", params = list(
            input$event_id,
            as.character(input$event_date),
            input$event_description,
            input$event_persons_involved,
            input$event_status,
            input$event_remarks,
            as.integer(values$editing_id)
          ))
          
          showNotification("Event updated successfully!", type = "message")
        }
        
        values$refresh <- values$refresh + 1
        toggleModal(session, "event_modal", toggle = "close")
        
      }, error = function(e) {
        showNotification(paste("Error saving event:", e$message), type = "error")
      })
    })
    
    # Handle cancel
    observeEvent(input$cancel_event, {
      toggleModal(session, "event_modal", toggle = "close")
    })
    
    # Helper function to build filter conditions for analytics
    build_analytics_filter <- function() {
      conditions <- c()
      
      if (!is.null(input$filter_status) && input$filter_status != "") {
        conditions <- c(conditions, paste0("event_status = '", input$filter_status, "'"))
      }
      
      if (!is.null(input$filter_date_range)) {
        start_date <- input$filter_date_range[1]
        end_date <- input$filter_date_range[2]
        conditions <- c(conditions, 
                        paste0("event_date >= '", start_date, "' AND event_date <= '", end_date, "'"))
      }
      
      if (length(conditions) > 0) {
        return(paste0("WHERE ", paste(conditions, collapse = " AND ")))
      } else {
        return("")
      }
    }
    
    # Status Distribution Chart
    output$status_distribution_chart <- renderPlotly({
      req(values$events_data)
      
      filter_conditions <- build_analytics_filter()
      
      query <- paste0("
        SELECT event_status, COUNT(*) as count
        FROM events
        ", filter_conditions, "
        GROUP BY event_status
        ORDER BY count DESC
      ")
      
      status_data <- dbGetQuery(con, query)
      
      if (nrow(status_data) > 0) {
        colors <- c("Pending" = "#f39c12", "In Progress" = "#3498db", 
                    "Completed" = "#27ae60", "Cancelled" = "#e74c3c")
        
        p <- plot_ly(status_data, 
                     labels = ~event_status, 
                     values = ~count,
                     type = 'pie',
                     marker = list(colors = colors[status_data$event_status]),
                     hovertemplate = '<b>%{label}</b><br>Count: %{value}<br>Percentage: %{percent}<extra></extra>') %>%
          layout(title = "",
                 showlegend = TRUE)
        p
      } else {
        plotly_empty() %>%
          layout(title = "No data available")
      }
    })
    
    # Timeline Overview Chart
    output$timeline_overview_chart <- renderPlotly({
      req(values$events_data)
      
      filter_conditions <- build_analytics_filter()
      
      query <- paste0("
        SELECT event_date, event_status, COUNT(*) as count
        FROM events
        ", filter_conditions, "
        GROUP BY event_date, event_status
        ORDER BY event_date
      ")
      
      timeline_data <- dbGetQuery(con, query)
      
      if (nrow(timeline_data) > 0) {
        colors <- c("Pending" = "#f39c12", "In Progress" = "#3498db", 
                    "Completed" = "#27ae60", "Cancelled" = "#e74c3c")
        
        p <- plot_ly(timeline_data, x = ~as.Date(event_date), y = ~count, 
                     color = ~event_status, colors = colors,
                     type = 'scatter', mode = 'lines+markers',
                     hovertemplate = '<b>%{fullData.name}</b><br>Date: %{x}<br>Count: %{y}<extra></extra>') %>%
          layout(title = "",
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "Number of Events"),
                 hovermode = 'closest')
        p
      } else {
        plotly_empty() %>%
          layout(title = "No data available")
      }
    })
    
    # Status Summary Table
    output$status_summary_table <- DT::renderDataTable({
      req(values$events_data)
      
      filter_conditions <- build_analytics_filter()
      
      query <- paste0("
        SELECT event_status as 'Status',
               COUNT(*) as 'Total Events',
               ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM events ", 
                      if(filter_conditions != "") filter_conditions else "", "), 2) as 'Percentage'
        FROM events
        ", filter_conditions, "
        GROUP BY event_status
        ORDER BY `Total Events` DESC
      ")
      
      status_summary <- dbGetQuery(con, query)
      
      DT::datatable(status_summary,
                    options = list(pageLength = 10, dom = 't'),
                    rownames = FALSE) %>%
        DT::formatStyle(columns = "Status",
                        backgroundColor = DT::styleEqual(
                          c("Pending", "In Progress", "Completed", "Cancelled"), 
                          c("#fff3cd", "#cce5ff", "#d4edda", "#f8d7da")
                        ))
    })
    
    # Monthly Trends Chart
    output$monthly_trends_chart <- renderPlotly({
      req(values$events_data)
      
      filter_conditions <- build_analytics_filter()
      
      query <- paste0("
        SELECT strftime('%Y-%m', event_date) as month,
               COUNT(*) as event_count
        FROM events
        ", filter_conditions, "
        GROUP BY strftime('%Y-%m', event_date)
        ORDER BY month
      ")
      
      monthly_data <- dbGetQuery(con, query)
      
      if (nrow(monthly_data) > 0) {
        p <- plot_ly(monthly_data, 
                     x = ~month, 
                     y = ~event_count,
                     type = 'bar',
                     marker = list(color = '#9b59b6'),
                     hovertemplate = '<b>%{x}</b><br>Events: %{y}<extra></extra>') %>%
          layout(title = "",
                 xaxis = list(title = "Month"),
                 yaxis = list(title = "Number of Events"))
        p
      } else {
        plotly_empty() %>%
          layout(title = "No data available")
      }
    })
    
    # Events Summary Table
    output$events_summary_table <- DT::renderDataTable({
      req(values$events_data)
      
      filter_conditions <- build_analytics_filter()
      
      query <- paste0("
        SELECT event_id as 'Event ID',
               event_date as 'Date',
               event_description as 'Description',
               event_persons_involved as 'Persons Involved',
               event_status as 'Status',
               CASE 
                 WHEN event_remarks IS NULL OR event_remarks = '' THEN 'No remarks'
                 ELSE event_remarks
               END as 'Remarks'
        FROM events
        ", filter_conditions, "
        ORDER BY event_date DESC
      ")
      
      summary_data <- dbGetQuery(con, query)
      
      DT::datatable(summary_data,
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE) %>%
        DT::formatStyle(columns = "Status",
                        backgroundColor = DT::styleEqual(
                          c("Pending", "In Progress", "Completed", "Cancelled"), 
                          c("#fff3cd", "#cce5ff", "#d4edda", "#f8d7da")
                        ))
    })
    
    # Download CSV
    output$download_csv <- downloadHandler(
      filename = function() {
        paste("events_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(values$events_data)
        
        filter_conditions <- build_analytics_filter()
        
        query <- paste0("
          SELECT event_id, event_date, event_description, event_persons_involved, 
                 event_status, event_remarks
          FROM events
          ", filter_conditions, "
          ORDER BY event_date DESC
        ")
        
        export_data <- dbGetQuery(con, query)
        
        if(nrow(export_data) > 0) {
          names(export_data) <- c("Event ID", "Date", "Description", 
                                  "Persons Involved", "Status", "Remarks")
          write.csv(export_data, file, row.names = FALSE)
        }
      }
    )
    
    # Timeline visualization as enhanced HTML
    output$timeline_visual <- renderText({
      req(values$events_data)
      
      if(nrow(values$events_data) == 0) {
        return("<div style='text-align: center; color: #7f8c8d; padding: 50px;'>
                <h3>No events to display</h3>
                <p>Add some events to see them in the timeline visualization.</p>
                </div>")
      }
      
      # Sort by date chronologically (oldest first)
      timeline_data <- values$events_data %>%
        arrange(event_date)
      
      # Create enhanced HTML timeline
      timeline_html <- "<div style='position: relative; padding: 20px;'>"
      
      for(i in 1:nrow(timeline_data)) {
        event <- timeline_data[i, ]
        
        # Determine status color
        status_color <- switch(event$event_status,
                               "Pending" = "#f39c12",
                               "In Progress" = "#3498db",
                               "Completed" = "#27ae60",
                               "Cancelled" = "#e74c3c",
                               "#95a5a6")  # Default color
        
        timeline_html <- paste0(timeline_html,
                                "<div style='margin-bottom: 30px; border-left: 4px solid ", status_color, "; padding-left: 20px; background-color: #f8f9fa; border-radius: 0 8px 8px 0; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>",
                                "<div style='background-color: white; padding: 15px; border-radius: 5px;'>",
                                "<h5 style='color: #2c3e50; margin-bottom: 10px; display: flex; align-items: center;'>",
                                "<span style='background-color: ", status_color, "; color: white; padding: 3px 8px; border-radius: 15px; font-size: 12px; margin-right: 10px;'>", event$event_status, "</span>",
                                event$event_id, "</h5>",
                                "<p style='color: #34495e; margin-bottom: 8px;'><strong>📅 Date:</strong> ", format(as.Date(event$event_date), "%B %d, %Y"), "</p>",
                                "<p style='color: #34495e; margin-bottom: 8px;'><strong>📝 Description:</strong> ", event$event_description, "</p>"
        )
        
        # Add persons involved if not empty
        if(!is.na(event$event_persons_involved) && event$event_persons_involved != "") {
          timeline_html <- paste0(timeline_html,
                                  "<p style='color: #34495e; margin-bottom: 8px;'><strong>👥 Persons Involved:</strong> ", event$event_persons_involved, "</p>"
          )
        }
        
        # Add remarks if not empty
        if(!is.na(event$event_remarks) && event$event_remarks != "") {
          timeline_html <- paste0(timeline_html,
                                  "<p style='color: #34495e; margin-bottom: 0;'><strong>💭 Remarks:</strong> ", event$event_remarks, "</p>"
          )
        }
        
        timeline_html <- paste0(timeline_html, "</div></div>")
      }
      
      timeline_html <- paste0(timeline_html, "</div>")
      
      return(timeline_html)
    })
  })
}

# Usage example:
# In your main app UI: timeline_ui("timeline")
# In your main app server: timeline_server("timeline", con)