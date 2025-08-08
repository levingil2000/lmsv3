# Sessions Module

sessions_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1("Sessions Management", style = "color: #2c3e50; margin-bottom: 30px;"),
    
    fluidRow(
      column(12,
             actionButton(ns("add_session"), "Add New Session", 
                          class = "btn-primary", style = "margin-bottom: 15px;")
      )
    ),
    
    # Filters Row
    fluidRow(
      column(3,
             selectInput(ns("filter_teacher"), "Filter by Teacher:",
                         choices = NULL, selected = NULL)
      ),
      column(3,
             selectInput(ns("filter_subject"), "Filter by Subject:",
                         choices = NULL, selected = NULL)
      ),
      column(3,
             selectInput(ns("filter_competency"), "Filter by Competency:",
                         choices = NULL, selected = NULL)
      ),
      column(3,
             dateRangeInput(ns("filter_date"), "Filter by Date:",
                            start = Sys.Date() - 30,
                            end = Sys.Date())
      )
    ),
    
    br(),
    
    # Sessions Table
    fluidRow(
      column(12,
             withSpinner(DT::dataTableOutput(ns("sessions_table")))
      )
    ),
    
    # Add/Edit Session Modal
    bsModal("session_modal", "Session Information", ns("add_session"), size = "large",
            fluidRow(
              column(6,
                     textInput(ns("session_id"), "Session ID:", ""),
                     selectInput(ns("teacher_id"), "Teacher:",
                                 choices = NULL, selected = NULL),
                     dateInput(ns("session_date"), "Date:", value = Sys.Date()),
                     numericInput(ns("duration"), "Duration (hours):", value = 1, min = 0.5, step = 0.5)
              ),
              column(6,
                     textInput(ns("learning_competency"), "Learning Competency:", ""),
                     textInput(ns("subject"), "Subject:", ""),
                     numericInput(ns("grade_level"), "Grade Level:", value = 7, min = 1, max = 12),
                     textAreaInput(ns("list_of_students"), "List of Students (comma-separated):", "", rows = 4)
              )
            ),
            
            footer = tagList(
              actionButton(ns("save_session"), "Save", class = "btn-primary"),
              actionButton(ns("delete_session"), "Delete", class = "btn-danger", style = "display: none;"),
              actionButton(ns("cancel_session"), "Cancel", class = "btn-default")
            )
    )
  )
}

sessions_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      sessions_data = NULL,
      filtered_data = NULL,
      editing_id = NULL,
      refresh = 0
    )
    
    # Load sessions data
    observe({
      values$refresh  # Dependency for refresh
      values$sessions_data <- get_table_data(con, "teacher_sessions")
    })
    
    # Update filter choices
    observe({
      req(values$sessions_data)
      
      # Teachers
      teachers <- dbGetQuery(con, "SELECT DISTINCT name, teacherID FROM teacher_registry WHERE teacher_status = 'Active'")
      teacher_choices <- setNames(teachers$teacherID, teachers$name)
      updateSelectInput(session, "filter_teacher",
                        choices = c("All" = "", teacher_choices))
      
      # For modal teacher selection
      updateSelectInput(session, "teacher_id",
                        choices = c("Select Teacher" = "", teacher_choices))
      
      # Subjects
      subjects <- unique(values$sessions_data$subject[!is.na(values$sessions_data$subject)])
      updateSelectInput(session, "filter_subject",
                        choices = c("All" = "", subjects))
      
      # Learning competencies
      competencies <- unique(values$sessions_data$learning_competency[!is.na(values$sessions_data$learning_competency)])
      updateSelectInput(session, "filter_competency",
                        choices = c("All" = "", competencies))
    })
    
    # Filter sessions data
    observe({
      req(values$sessions_data)
      
      filtered <- values$sessions_data
      
      # Apply filters
      if (!is.null(input$filter_teacher) && input$filter_teacher != "") {
        filtered <- filtered[filtered$teacher_id == input$filter_teacher, ]
      }
      
      if (!is.null(input$filter_subject) && input$filter_subject != "") {
        filtered <- filtered[filtered$subject == input$filter_subject, ]
      }
      
      if (!is.null(input$filter_competency) && input$filter_competency != "") {
        filtered <- filtered[filtered$learning_competency == input$filter_competency, ]
      }
      
      if (!is.null(input$filter_date)) {
        filtered <- filtered[filtered$date >= input$filter_date[1] & filtered$date <= input$filter_date[2], ]
      }
      
      values$filtered_data <- filtered
    })
    
    # Sessions table
    output$sessions_table <- DT::renderDataTable({
      req(values$filtered_data)
      
      # Join with teacher names for display
      display_data <- values$filtered_data
      
      if (nrow(display_data) > 0) {
        # Get teacher names
        teacher_names <- dbGetQuery(con, "SELECT teacherID, name FROM teacher_registry")
        display_data <- merge(display_data, teacher_names, by.x = "teacher_id", by.y = "teacherID", all.x = TRUE)
        
        display_data <- display_data %>%
          select(session_id, name, date, duration, learning_competency, subject, grade_level, list_of_students) %>%
          rename(teacher_name = name)
      }
      
      DT::datatable(display_data,
                    options = list(
                      pageLength = 10, 
                      scrollX = TRUE,
                      columnDefs = list(
                        list(targets = c(7), width = "200px")  # list_of_students column
                      )
                    ),
                    rownames = FALSE,
                    selection = "single")
    })
    
    # Handle row selection for editing
    observeEvent(input$sessions_table_rows_selected, {
      if (length(input$sessions_table_rows_selected) > 0) {
        selected_row <- input$sessions_table_rows_selected
        session_data <- values$filtered_data[selected_row, ]
        
        values$editing_id <- session_data$id
        
        # Populate modal fields
        updateTextInput(session, "session_id", value = session_data$session_id)
        updateSelectInput(session, "teacher_id", selected = session_data$teacher_id)
        updateDateInput(session, "session_date", value = as.Date(session_data$date))
        updateNumericInput(session, "duration", value = session_data$duration)
        updateTextInput(session, "learning_competency", value = session_data$learning_competency)
        updateTextInput(session, "subject", value = session_data$subject)
        updateNumericInput(session, "grade_level", value = session_data$grade_level)
        updateTextAreaInput(session, "list_of_students", value = session_data$list_of_students)
        
        # Show delete button
        shinyjs::show("delete_session")
        
        toggleModal(session, "session_modal", toggle = "open")
      }
    })
    
    # Handle add new session
    observeEvent(input$add_session, {
      values$editing_id <- NULL
      
      # Generate new session ID
      session_id <- paste0("SES_", format(Sys.Date(), "%Y%m%d"), "_", sprintf("%04d", sample(1:9999, 1)))
      
      # Clear modal fields
      updateTextInput(session, "session_id", value = session_id)
      updateSelectInput(session, "teacher_id", selected = "")
      updateDateInput(session, "session_date", value = Sys.Date())
      updateNumericInput(session, "duration", value = 1)
      updateTextInput(session, "learning_competency", value = "")
      updateTextInput(session, "subject", value = "")
      updateNumericInput(session, "grade_level", value = 7)
      updateTextAreaInput(session, "list_of_students", value = "")
      
      # Hide delete button
      shinyjs::hide("delete_session")
    })
    
    # Handle save session
    observeEvent(input$save_session, {
      req(input$session_id, input$teacher_id, input$session_date, input$duration)
      
      session_data <- data.frame(
        session_id = input$session_id,
        teacher_id = input$teacher_id,
        date = input$session_date,
        duration = input$duration,
        learning_competency = input$learning_competency,
        subject = input$subject,
        grade_level = input$grade_level,
        list_of_students = input$list_of_students,
        stringsAsFactors = FALSE
      )
      
      if (is.null(values$editing_id)) {
        # Insert new session
        result <- insert_data(con, "teacher_sessions", session_data)
      } else {
        # Update existing session
        result <- update_data(con, "teacher_sessions", session_data, "id", values$editing_id)
      }
      
      if (result$success) {
        showNotification(result$message, type = "message")
        values$refresh <- values$refresh + 1
        toggleModal(session, "session_modal", toggle = "close")
      } else {
        showNotification(result$message, type = "error")
      }
    })
    
    # Handle delete session
    observeEvent(input$delete_session, {
      req(values$editing_id)
      
      showModal(modalDialog(
        title = "Confirm Deletion",
        "Are you sure you want to delete this session? This action cannot be undone.",
        footer = tagList(
          actionButton(ns("confirm_delete"), "Yes, Delete", class = "btn-danger"),
          modalButton("Cancel")
        )
      ))
    })
    
    # Handle confirm delete
    observeEvent(input$confirm_delete, {
      req(values$editing_id)
      
      result <- delete_data(con, "teacher_sessions", "id", values$editing_id)
      
      if (result$success) {
        showNotification(result$message, type = "message")
        values$refresh <- values$refresh + 1
        removeModal()
        toggleModal(session, "session_modal", toggle = "close")
      } else {
        showNotification(result$message, type = "error")
      }
    })
    
    # Handle cancel
    observeEvent(input$cancel_session, {
      toggleModal(session, "session_modal", toggle = "close")
    })
  })
}