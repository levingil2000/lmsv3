# Teachers Module

teachers_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1("Teachers Management", style = "color: #2c3e50; margin-bottom: 30px;"),
    
    tabsetPanel(
      # Teacher Registry Tab
      tabPanel("Teacher Registry",
               br(),
               fluidRow(
                 column(12,
                        actionButton(ns("add_teacher"), "Add New Teacher", 
                                     class = "btn-primary", style = "margin-bottom: 15px;")
                 )
               ),
               
               fluidRow(
                 column(12,
                        withSpinner(DT::dataTableOutput(ns("teachers_table")))
                 )
               )
      ),
      
      # Teacher Analytics Tab
      tabPanel("Teacher Analytics",
               br(),
               
               # Filters
               fluidRow(
                 column(4,
                        selectInput(ns("filter_grade"), "Grade Level:",
                                    choices = NULL, selected = NULL)
                 ),
                 column(4,
                        selectInput(ns("filter_subject"), "Subject:",
                                    choices = NULL, selected = NULL)
                 ),
                 column(4,
                        selectInput(ns("filter_competency"), "Learning Competency:",
                                    choices = NULL, selected = NULL)
                 )
               ),
               
               # Summary Cards
               fluidRow(
                 column(6,
                        wellPanel(
                          h4("Total Tutor Sessions by Teacher", style = "color: #2c3e50;"),
                          withSpinner(plotlyOutput(ns("sessions_by_teacher_chart")))
                        )
                 ),
                 column(6,
                        wellPanel(
                          h4("Teaching Hours Distribution", style = "color: #2c3e50;"),
                          withSpinner(plotlyOutput(ns("hours_distribution_chart")))
                        )
                 )
               ),
               
               br(),
               
               fluidRow(
                 column(12,
                        wellPanel(
                          h4("Teacher Performance Summary", style = "color: #2c3e50;"),
                          withSpinner(DT::dataTableOutput(ns("teacher_performance_table")))
                        )
                 )
               )
      )
    ),
    
    # Add/Edit Teacher Modal
    bsModal("teacher_modal", "Teacher Information", ns("add_teacher"), size = "medium",
            fluidRow(
              column(6,
                     textInput(ns("teacher_name"), "Teacher Name:", ""),
                     textInput(ns("teacherID"), "Teacher ID:", ""),
                     textInput(ns("subject_handled"), "Subject Handled:", "")
              ),
              column(6,
                     numericInput(ns("year_level"), "Year Level:", value = 7, min = 1, max = 12),
                     selectInput(ns("teacher_status"), "Status:",
                                 choices = c("Active", "Inactive", "On Leave"),
                                 selected = "Active")
              )
            ),
            
            footer = tagList(
              actionButton(ns("save_teacher"), "Save", class = "btn-primary"),
              actionButton(ns("cancel_teacher"), "Cancel", class = "btn-default")
            )
    )
  )
}

teachers_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      teachers_data = NULL,
      editing_id = NULL,
      refresh = 0
    )
    
    # Load teachers data
    observe({
      values$refresh  # Dependency for refresh
      values$teachers_data <- get_table_data(con, "teacher_registry")
    })
    
    # Update filter choices
    observe({
      # Grade levels from teacher registry
      if (!is.null(values$teachers_data) && nrow(values$teachers_data) > 0) {
        grades <- sort(unique(values$teachers_data$year_level))
        updateSelectInput(session, "filter_grade", 
                          choices = c("All" = "", grades))
      }
      
      # Subjects from teacher sessions
      subjects <- dbGetQuery(con, "SELECT DISTINCT subject FROM teacher_sessions WHERE subject IS NOT NULL")$subject
      updateSelectInput(session, "filter_subject",
                        choices = c("All" = "", subjects))
      
      # Learning competencies
      competencies <- dbGetQuery(con, "SELECT DISTINCT learning_competency FROM teacher_sessions WHERE learning_competency IS NOT NULL")$learning_competency
      updateSelectInput(session, "filter_competency",
                        choices = c("All" = "", competencies))
    })
    
    # Teachers table
    output$teachers_table <- DT::renderDataTable({
      req(values$teachers_data)
      
      display_data <- values$teachers_data %>%
        select(name, teacherID, subject_handled, year_level, teacher_status)
      
      DT::datatable(display_data,
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE,
                    selection = "single") %>%
        DT::formatStyle(columns = "teacher_status",
                        backgroundColor = DT::styleEqual(c("Active", "Inactive", "On Leave"), 
                                                         c("#d4edda", "#f8d7da", "#fff3cd")))
    })
    
    # Handle row selection for editing
    observeEvent(input$teachers_table_rows_selected, {
      if (length(input$teachers_table_rows_selected) > 0) {
        selected_row <- input$teachers_table_rows_selected
        teacher_data <- values$teachers_data[selected_row, ]
        
        values$editing_id <- teacher_data$id
        
        # Populate modal fields
        updateTextInput(session, "teacher_name", value = teacher_data$name)
        updateTextInput(session, "teacherID", value = teacher_data$teacherID)
        updateTextInput(session, "subject_handled", value = teacher_data$subject_handled)
        updateNumericInput(session, "year_level", value = teacher_data$year_level)
        updateSelectInput(session, "teacher_status", selected = teacher_data$teacher_status)
        
        toggleModal(session, "teacher_modal", toggle = "open")
      }
    })
    
    # Handle add new teacher
    observeEvent(input$add_teacher, {
      values$editing_id <- NULL
      
      # Clear modal fields
      updateTextInput(session, "teacher_name", value = "")
      updateTextInput(session, "teacherID", value = "")
      updateTextInput(session, "subject_handled", value = "")
      updateNumericInput(session, "year_level", value = 7)
      updateSelectInput(session, "teacher_status", selected = "Active")
    })
    
    # Handle save teacher
    observeEvent(input$save_teacher, {
      req(input$teacher_name, input$teacherID)
      
      teacher_data <- data.frame(
        name = input$teacher_name,
        teacherID = input$teacherID,
        subject_handled = input$subject_handled,
        year_level = input$year_level,
        teacher_status = input$teacher_status,
        stringsAsFactors = FALSE
      )
      
      if (is.null(values$editing_id)) {
        # Insert new teacher
        result <- insert_data(con, "teacher_registry", teacher_data)
      } else {
        # Update existing teacher
        result <- update_data(con, "teacher_registry", teacher_data, "id", values$editing_id)
      }
      
      if (result$success) {
        showNotification(result$message, type = "message")
        values$refresh <- values$refresh + 1
        toggleModal(session, "teacher_modal", toggle = "close")
      } else {
        showNotification(result$message, type = "error")
      }
    })
    
    # Handle cancel
    observeEvent(input$cancel_teacher, {
      toggleModal(session, "teacher_modal", toggle = "close")
    })
    
    # Helper function to build filter conditions for analytics
    build_analytics_filter <- function() {
      conditions <- c()
      
      if (!is.null(input$filter_grade) && input$filter_grade != "") {
        conditions <- c(conditions, paste0("ts.grade_level = ", input$filter_grade))
      }
      
      if (!is.null(input$filter_subject) && input$filter_subject != "") {
        conditions <- c(conditions, paste0("ts.subject = '", input$filter_subject, "'"))
      }
      
      if (!is.null(input$filter_competency) && input$filter_competency != "") {
        conditions <- c(conditions, paste0("ts.learning_competency = '", input$filter_competency, "'"))
      }
      
      if (length(conditions) > 0) {
        return(paste0("WHERE ", paste(conditions, collapse = " AND ")))
      } else {
        return("")
      }
    }
    
    # Sessions by Teacher Chart
    output$sessions_by_teacher_chart <- renderPlotly({
      filter_conditions <- build_analytics_filter()
      
      query <- paste0("
        SELECT tr.name, COUNT(ts.id) as session_count
        FROM teacher_registry tr
        LEFT JOIN teacher_sessions ts ON tr.teacherID = ts.teacher_id
        ", gsub("WHERE", "WHERE tr.teacher_status = 'Active' AND", filter_conditions, fixed = TRUE),
                      if (filter_conditions == "") "WHERE tr.teacher_status = 'Active'" else "",
                      "
        GROUP BY tr.name
        ORDER BY session_count DESC
        LIMIT 10
      ")
      
      teacher_data <- dbGetQuery(con, query)
      
      if (nrow(teacher_data) > 0) {
        p <- plot_ly(teacher_data, 
                     x = ~reorder(name, session_count), 
                     y = ~session_count,
                     type = 'bar',
                     marker = list(color = '#9b59b6'),
                     hovertemplate = '<b>%{x}</b><br>Sessions: %{y}<extra></extra>') %>%
          layout(title = "",
                 xaxis = list(title = "Teacher"),
                 yaxis = list(title = "Number of Sessions"))
        p
      } else {
        plotly_empty() %>%
          layout(title = "No data available")
      }
    })
    
    # Teaching Hours Distribution Chart
    output$hours_distribution_chart <- renderPlotly({
      filter_conditions <- build_analytics_filter()
      
      query <- paste0("
        SELECT tr.name, SUM(ts.duration) as total_hours
        FROM teacher_registry tr
        LEFT JOIN teacher_sessions ts ON tr.teacherID = ts.teacher_id
        ", gsub("WHERE", "WHERE tr.teacher_status = 'Active' AND", filter_conditions, fixed = TRUE),
                      if (filter_conditions == "") "WHERE tr.teacher_status = 'Active'" else "",
                      "
        GROUP BY tr.name
        HAVING total_hours > 0
        ORDER BY total_hours DESC
      ")
      
      hours_data <- dbGetQuery(con, query)
      
      if (nrow(hours_data) > 0) {
        p <- plot_ly(hours_data, 
                     labels = ~name, 
                     values = ~total_hours,
                     type = 'pie',
                     hovertemplate = '<b>%{label}</b><br>Hours: %{value}<extra></extra>') %>%
          layout(title = "")
        p
      } else {
        plotly_empty() %>%
          layout(title = "No data available")
      }
    })
    
    # Teacher Performance Table
    output$teacher_performance_table <- DT::renderDataTable({
      filter_conditions <- build_analytics_filter()
      
      query <- paste0("
        SELECT tr.name as 'Teacher Name',
               tr.subject_handled as 'Subject',
               COUNT(ts.id) as 'Total Sessions',
               ROUND(SUM(ts.duration), 2) as 'Total Hours',
               ROUND(AVG(ts.duration), 2) as 'Avg Session Duration'
        FROM teacher_registry tr
        LEFT JOIN teacher_sessions ts ON tr.teacherID = ts.teacher_id
        ", gsub("WHERE", "WHERE tr.teacher_status = 'Active' AND", filter_conditions, fixed = TRUE),
                      if (filter_conditions == "") "WHERE tr.teacher_status = 'Active'" else "",
                      "
        GROUP BY tr.name, tr.subject_handled
        ORDER BY `Total Hours` DESC
      ")
      
      performance_data <- dbGetQuery(con, query)
      
      DT::datatable(performance_data,
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    })
  })
}