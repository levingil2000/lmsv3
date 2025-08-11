#manage class tab
manage_classes_tab_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Manage Classes",
    value = "manage",
    icon = icon("edit"),
    br(),
    fluidRow(
      box(
        title = "Manage Enrollments, Attendance & Status", 
        status = "warning", 
        solidHeader = TRUE, 
        width = 12,
        h4("Update Attendance, Mark as Passed, and Remove Students"),
        
        fluidRow(
          column(2,
                 selectInput(ns("manage_quarter"), "Quarter:", 
                             choices = c("Q1", "Q2", "Q3", "Q4"), 
                             selected = "Q1")
          ),
          column(3,
                 selectInput(ns("manage_subject"), "Subject:", 
                             choices = c("English" = "english", 
                                         "Math" = "math", 
                                         "Science" = "science"), 
                             selected = "english")
          ),
          column(2,
                 selectInput(ns("manage_grade"), "Grade Level:", 
                             choices = c("All Grades" = "all"),
                             selected = "all")
          ),
          column(3,
                 numericInput(ns("set_attendance_value"), "Set Attendance To:", 
                              value = 0, min = 0, max = 100, step = 1)
          ),
          column(2,
                 actionButton(ns("refresh_manage"), "Refresh", class = "btn-info", 
                              style = "margin-top: 25px; width: 100%;")
          )
        ),
        
        fluidRow(
          column(4,
                 actionButton(ns("set_attendance_all"), "Set Attendance for All", 
                              class = "btn-success", style = "width: 100%; margin-top: 10px;")
          ),
          column(4,
                 actionButton(ns("set_attendance_selected"), "Set Attendance for Selected", 
                              class = "btn-warning", style = "width: 100%; margin-top: 10px;")
          ),
          column(4,
                 actionButton(ns("mark_passed_selected"), "Mark Selected as Passed", 
                              class = "btn-primary", style = "width: 100%; margin-top: 10px;")
          )
        ),
        
        hr(),
        p("• Click on attendance numbers to edit directly"),
        p("• Select students and use buttons to update their attendance or mark as passed"),
        p("• Use 'Mark Selected as Passed' to change status from Active to Passed"),
        p("• Use Remove buttons to unenroll students"),
        h4(textOutput(ns("manage_title"))),
        DT::dataTableOutput(ns("attendance_management_table"))
      )
    )
  )
}

# =============================================================================
# SERVER MODULE
# =============================================================================

manage_classes_tab_server <- function(id, con, refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update manage grade choices when quarter/subject changes
    observe({
      req(input$manage_quarter, input$manage_subject)
      
      enrolled_grades <- get_enrolled_grades(con, input$manage_quarter, input$manage_subject)
      
      if (length(enrolled_grades) > 0) {
        grade_choices <- c("All Grades" = "all")
        for (grade in enrolled_grades) {
          grade_choices[paste("Grade", grade)] <- as.character(grade)
        }
        updateSelectInput(session, "manage_grade", choices = grade_choices)
      } else {
        updateSelectInput(session, "manage_grade", choices = c("All Grades" = "all"))
      }
    })
    
    # Management table with grade level filtering
    manage_enrollment <- reactive({
      input$refresh_manage  # Dependency for refresh
      refresh_trigger()     # Additional dependency
      get_enrollment_data(con, input$manage_quarter, input$manage_subject, input$manage_grade)
    })
    
    output$manage_title <- renderText({
      enrollment_data <- manage_enrollment()
      grade_text <- if(input$manage_grade == "all") "All Grades" else paste("Grade", input$manage_grade)
      active_count <- sum(enrollment_data$status == "Active", na.rm = TRUE)
      passed_count <- sum(enrollment_data$status == "Passed", na.rm = TRUE)
      paste("Managing", toupper(input$manage_subject), "-", input$manage_quarter, 
            "- (", grade_text, "):", nrow(enrollment_data), "students",
            "(Active:", active_count, "| Passed:", passed_count, ")")
    })
    
    output$attendance_management_table <- DT::renderDataTable({
      enrollment_data <- manage_enrollment()
      
      if (nrow(enrollment_data) > 0) {
        # Add action buttons
        enrollment_data$Actions <- paste0(
          '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'', ns('remove_student'), '\', ', 
          enrollment_data$enrollment_id, ', {priority: \'event\'})">Remove</button>'
        )
        
        DT::datatable(
          enrollment_data %>% select(student_name, grade_level, section, enrollment_date, total_attendance, status, Actions),
          editable = list(target = 'cell', disable = list(columns = c(0:3, 5, 6))),
          escape = FALSE,
          selection = 'multiple',
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            searching = TRUE
          ),
          caption = "Select students to set attendance or mark as passed, or click attendance numbers to edit directly"
        ) %>%
          DT::formatStyle("status",
                          backgroundColor = DT::styleEqual(c("Active", "Passed"), c("#d4edda", "#cce5ff")))
      } else {
        empty_df <- data.frame(Message = "No students enrolled in this class/grade combination")
        DT::datatable(
          empty_df,
          options = list(searching = FALSE, paging = FALSE, info = FALSE)
        )
      }
    })
    
    # Handle cell edits in management table
    observeEvent(input$attendance_management_table_cell_edit, {
      info <- input$attendance_management_table_cell_edit
      enrollment_data <- manage_enrollment()
      
      if (info$col == 4) {  # total_attendance column (0-indexed)
        enrollment_id <- enrollment_data[info$row, "enrollment_id"]
        new_attendance <- as.numeric(info$value)
        
        if (!is.na(new_attendance) && new_attendance >= 0) {
          update_attendance(con, enrollment_id, new_attendance)
          showNotification("Attendance updated successfully", type = "message")
          refresh_trigger(Sys.time())
        } else {
          showNotification("Invalid attendance value", type = "error")
        }
      }
    })
    
    # Handle student removal
    observeEvent(input$remove_student, {
      enrollment_id <- input$remove_student
      remove_enrollment(con, enrollment_id)
      showNotification("Student removed from class", type = "message")
      refresh_trigger(Sys.time())
    })
    
    # Handle setting attendance for all students
    observeEvent(input$set_attendance_all, {
      enrollment_data <- manage_enrollment()
      new_attendance <- input$set_attendance_value
      
      if (nrow(enrollment_data) > 0) {
        if (is.na(new_attendance) || new_attendance < 0) {
          showNotification("Please enter a valid attendance value (0 or greater)", type = "error")
          return()
        }
        
        enrollment_ids <- enrollment_data$enrollment_id
        success_count <- set_attendance_for_students(con, enrollment_ids, new_attendance)
        
        showNotification(
          paste("Set attendance to", new_attendance, "for", success_count, "students"),
          type = "message"
        )
        
        # Refresh the table
        refresh_trigger(Sys.time())
      } else {
        showNotification("No students to update", type = "warning")
      }
    })
    
    # Handle setting attendance for selected students
    observeEvent(input$set_attendance_selected, {
      selected_rows <- input$attendance_management_table_rows_selected
      new_attendance <- input$set_attendance_value
      
      if (length(selected_rows) > 0) {
        if (is.na(new_attendance) || new_attendance < 0) {
          showNotification("Please enter a valid attendance value (0 or greater)", type = "error")
          return()
        }
        
        enrollment_data <- manage_enrollment()
        selected_enrollment_ids <- enrollment_data[selected_rows, "enrollment_id"]
        
        success_count <- set_attendance_for_students(con, selected_enrollment_ids, new_attendance)
        
        showNotification(
          paste("Set attendance to", new_attendance, "for", success_count, "selected students"),
          type = "message"
        )
        
        # Refresh the table
        refresh_trigger(Sys.time())
      } else {
        showNotification("Please select students first", type = "warning")
      }
    })
    
    # Handle marking students as passed
    observeEvent(input$mark_passed_selected, {
      selected_rows <- input$attendance_management_table_rows_selected
      
      if (length(selected_rows) > 0) {
        enrollment_data <- manage_enrollment()
        selected_enrollment_ids <- enrollment_data[selected_rows, "enrollment_id"]
        
        # Only mark Active students as passed
        active_enrollments <- enrollment_data[selected_rows, ] %>%
          filter(status == "Active")
        
        if (nrow(active_enrollments) > 0) {
          active_enrollment_ids <- active_enrollments$enrollment_id
          success_count <- mark_students_passed(con, active_enrollment_ids)
          
          showNotification(
            paste("Marked", success_count, "students as passed"),
            type = "message"
          )
          
          # Refresh the table
          refresh_trigger(Sys.time())
        } else {
          showNotification("No active students selected to mark as passed", type = "warning")
        }
      } else {
        showNotification("Please select students first", type = "warning")
      }
    })
  })
}