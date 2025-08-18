

sessions_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1("Sessions Management", style = "color: #2c3e50; margin-bottom: 30px;"),
    
    fluidRow(
      column(6,
             actionButton(ns("add_session"), "Add New Session", 
                          class = "btn-primary", style = "margin-bottom: 15px;")
      ),
      column(6,
             actionButton(ns("refresh_data"), "Refresh Data", 
                          class = "btn-info", style = "margin-bottom: 15px; float: right;")
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
                     numericInput(ns("duration"), "Duration (hours):", value = 1, min = 0.5, step = 0.5),
                     numericInput(ns("grade_level"), "Grade Level:", value = 7, min = 1, max = 12),
                     selectInput(ns("quarter"), "Quarter:", choices = 1:4, selected = 1)
              ),
              column(6,
                     textInput(ns("learning_competency"), "Learning Competency:", ""),
                     textInput(ns("subject"), "Subject:", ""),
                     textAreaInput(ns("remarks_observation"), "Remarks and Observations:",
                                   "Most of the students are having a hard time multiplying", rows = 3),
                     textAreaInput(ns("assessment_reflection"), "Assessment and Reflection:",
                                   "14 out of 15 students scored well on the exercise, this specific method seems effective", rows = 3)
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
    
    values <- reactiveValues(
      sessions_data = NULL,
      teachers_data = NULL,  # Add separate reactive value for teachers
      filtered_data = NULL,
      editing_id = NULL,
      refresh = 0
    )
    
    # Load both sessions and teachers data when refresh changes
    observe({
      values$refresh
      values$sessions_data <- get_table_data(con, "teacher_sessions")
      # Refresh teachers data as well
      values$teachers_data <- dbGetQuery(con, "SELECT DISTINCT name, teacherID, subject_handled FROM teacher_registry WHERE teacher_status = 'Active'")
    })
    
    # Refresh button
    observeEvent(input$refresh_data, {
      values$refresh <- values$refresh + 1
    })
    
    # Update dropdowns when teachers data changes
    observe({
      req(values$teachers_data)
      
      teacher_choices <- setNames(values$teachers_data$teacherID, values$teachers_data$name)
      
      updateSelectInput(session, "filter_teacher", choices = c("All" = "", teacher_choices))
      updateSelectInput(session, "teacher_id", choices = c("Select Teacher" = "", teacher_choices))
    })
    
    # Update subject and competency filters when sessions data changes
    observe({
      req(values$sessions_data)
      
      subjects <- unique(values$sessions_data$subject[!is.na(values$sessions_data$subject)])
      updateSelectInput(session, "filter_subject", choices = c("All" = "", subjects))
      
      competencies <- unique(values$sessions_data$learning_competency[!is.na(values$sessions_data$learning_competency)])
      updateSelectInput(session, "filter_competency", choices = c("All" = "", competencies))
    })
    
    # Auto-fill subject when teacher is selected
    observeEvent(input$teacher_id, {
      req(values$teachers_data, input$teacher_id)
      if (input$teacher_id != "") {
        teacher_row <- values$teachers_data[values$teachers_data$teacherID == input$teacher_id, ]
        if (nrow(teacher_row) > 0) {
          updateTextInput(session, "subject", value = teacher_row$subject_handled[1])
        }
      }
    })
    
    observe({
      req(values$sessions_data)
      filtered <- values$sessions_data
      
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
    
    output$sessions_table <- DT::renderDataTable({
      req(values$filtered_data, values$teachers_data)
      
      display_data <- values$filtered_data
      if (nrow(display_data) > 0) {
        # Use the cached teachers data instead of querying again
        teacher_names <- values$teachers_data[, c("teacherID", "name")]
        display_data <- merge(display_data, teacher_names, by.x = "teacher_id", by.y = "teacherID", all.x = TRUE)
        
        display_data <- display_data %>%
          select(session_id, name, date, duration, learning_competency, subject, grade_level, quarter, remarks_observation, assessment_reflection) %>%
          rename(teacher_name = name)
      }
      
      DT::datatable(display_data,
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE,
                    selection = "single")
    })
    
    observeEvent(input$sessions_table_rows_selected, {
      if (length(input$sessions_table_rows_selected) > 0) {
        selected_row <- input$sessions_table_rows_selected
        session_data <- values$filtered_data[selected_row, ]
        values$editing_id <- session_data$id
        
        updateTextInput(session, "session_id", value = session_data$session_id)
        updateSelectInput(session, "teacher_id", selected = session_data$teacher_id)
        updateDateInput(session, "session_date", value = as.Date(session_data$date))
        updateNumericInput(session, "duration", value = session_data$duration)
        updateTextInput(session, "learning_competency", value = session_data$learning_competency)
        updateTextInput(session, "subject", value = session_data$subject)
        updateNumericInput(session, "grade_level", value = session_data$grade_level)
        updateSelectInput(session, "quarter", selected = session_data$quarter)
        updateTextAreaInput(session, "remarks_observation", value = session_data$remarks_observation)
        updateTextAreaInput(session, "assessment_reflection", value = session_data$assessment_reflection)
        
        shinyjs::show("delete_session")
        toggleModal(session, "session_modal", toggle = "open")
      }
    })
    
    observeEvent(input$add_session, {
      values$editing_id <- NULL
      session_id <- paste0("SES_", format(Sys.Date(), "%Y%m%d"), "_", sprintf("%04d", sample(1:9999, 1)))
      
      updateTextInput(session, "session_id", value = session_id)
      updateSelectInput(session, "teacher_id", selected = "")
      updateDateInput(session, "session_date", value = Sys.Date())
      updateNumericInput(session, "duration", value = 1)
      updateTextInput(session, "learning_competency", value = "")
      updateTextInput(session, "subject", value = "")
      updateNumericInput(session, "grade_level", value = 7)
      updateSelectInput(session, "quarter", selected = 1)
      updateTextAreaInput(session, "remarks_observation", value = "Most of the students are having a hard time multiplying")
      updateTextAreaInput(session, "assessment_reflection", value = "14 out of 15 students scored well on the exercise, this specific method seems effective")
      
      shinyjs::hide("delete_session")
    })
    
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
        quarter = input$quarter,
        remarks_observation = input$remarks_observation,
        assessment_reflection = input$assessment_reflection,
        stringsAsFactors = FALSE
      )
      
      if (is.null(values$editing_id)) {
        result <- insert_data(con, "teacher_sessions", session_data)
      } else {
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
    
    observeEvent(input$delete_session, {
      req(values$editing_id)
      showModal(modalDialog(
        title = "Confirm Deletion",
        "Are you sure you want to delete this session?",
        footer = tagList(
          actionButton(ns("confirm_delete"), "Yes, Delete", class = "btn-danger"),
          modalButton("Cancel")
        )
      ))
    })
    
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
    
    observeEvent(input$cancel_session, {
      toggleModal(session, "session_modal", toggle = "close")
    })
  })
}