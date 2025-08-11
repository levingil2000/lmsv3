# =============================================================================
# REMEDIAL CLASS MANAGEMENT MODULE (UPDATED)
# =============================================================================
# Author: Assistant
# Purpose: Shiny module for managing student enrollment in remedial classes
# Usage: Include in existing LMS as student_enrollment_ui() and student_enrollment_server()

library(shiny)
library(DT)
library(dplyr)
library(shinydashboard)
library(RSQLite)

# =============================================================================
# DATABASE HELPER FUNCTIONS
# =============================================================================

# Create enrollment table if it doesn't exist
create_enrollment_table <- function(con) {
  dbExecute(con, 
            "CREATE TABLE IF NOT EXISTS remedial_enrollments (
      enrollment_id INTEGER PRIMARY KEY AUTOINCREMENT,
      student_id INTEGER,
      quarter TEXT,
      subject TEXT,
      enrollment_date DATE DEFAULT CURRENT_DATE,
      total_attendance INTEGER DEFAULT 0,
      status TEXT DEFAULT 'Active',
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (student_id) REFERENCES full_student_registry(student_id),
      UNIQUE(student_id, quarter, subject)
    );")
}

# Get student registry data
get_student_registry <- function(con) {
  query <- "
    SELECT 
      student_id,
      student_name,
      grade_level,
      section,
      Total_score_per,
      RMA_before_math_proficiency,
      before_reading_proficiency,
      testq1math,
      testq2math,
      testq3math,
      testq4math,
      testq1sci,
      testq2sci,
      testq3sci,
      testq4sci,
      testq1eng,
      testq2eng,
      testq3eng,
      testq4eng
    FROM full_student_registry
    ORDER BY student_name
  "
  
  students <- dbGetQuery(con, query)
  
  # Calculate averages for each subject
  students <- students %>%
    rowwise() %>%
    mutate(
      math_avg = round(mean(c_across(starts_with("testq") & ends_with("math")), na.rm = TRUE), 1),
      sci_avg = round(mean(c_across(starts_with("testq") & ends_with("sci")), na.rm = TRUE), 1),
      eng_avg = round(mean(c_across(starts_with("testq") & ends_with("eng")), na.rm = TRUE), 1),
      overall_avg = round(mean(c(math_avg, sci_avg, eng_avg), na.rm = TRUE), 1)
    ) %>%
    ungroup()
  
  return(students)
}

# Get enrollment data with optional filtering
get_enrollment_data <- function(con, quarter = NULL, subject = NULL, grade_level = NULL) {
  base_query <- "
    SELECT 
      e.enrollment_id,
      e.student_id,
      e.quarter,
      e.subject,
      e.enrollment_date,
      e.total_attendance,
      e.status,
      s.student_name,
      s.grade_level,
      s.section
    FROM remedial_enrollments e
    LEFT JOIN full_student_registry s ON e.student_id = s.student_id
    WHERE e.status = 'Active'
  "
  
  params <- list()
  if (!is.null(quarter)) {
    base_query <- paste(base_query, "AND e.quarter = ?")
    params <- append(params, quarter)
  }
  if (!is.null(subject)) {
    base_query <- paste(base_query, "AND e.subject = ?")
    params <- append(params, subject)
  }
  if (!is.null(grade_level) && grade_level != "all") {
    base_query <- paste(base_query, "AND s.grade_level = ?")
    params <- append(params, as.numeric(grade_level))
  }
  
  base_query <- paste(base_query, "ORDER BY s.grade_level, s.student_name")
  
  if (length(params) > 0) {
    return(dbGetQuery(con, base_query, params))
  } else {
    return(dbGetQuery(con, base_query))
  }
}

# Get unique grade levels for a specific quarter/subject combination
get_enrolled_grades <- function(con, quarter, subject) {
  query <- "
    SELECT DISTINCT s.grade_level
    FROM remedial_enrollments e
    LEFT JOIN full_student_registry s ON e.student_id = s.student_id
    WHERE e.status = 'Active' AND e.quarter = ? AND e.subject = ?
    ORDER BY s.grade_level
  "
  result <- dbGetQuery(con, query, params = list(quarter, subject))
  return(result$grade_level)
}

# Enroll students
enroll_students <- function(con, student_ids, quarter, subject, initial_attendance = 0) {
  success_count <- 0
  duplicate_count <- 0
  
  # Ensure student_ids is a vector and handle it properly
  if (is.data.frame(student_ids) || is.matrix(student_ids)) {
    student_ids <- as.vector(student_ids)
  }
  
  # Debug: Print what we received
  cat("Function received student_ids:", class(student_ids), "- values:", student_ids, "\n")
  cat("Initial attendance:", initial_attendance, "\n")
  
  for (i in 1:length(student_ids)) {
    student_id <- student_ids[i]
    
    tryCatch({
      # First check if student is already enrolled
      existing <- dbGetQuery(con, 
                             "SELECT COUNT(*) as count FROM remedial_enrollments 
         WHERE student_id = ? AND quarter = ? AND subject = ? AND status = 'Active'",
                             params = list(as.integer(student_id), quarter, subject))
      
      if (existing$count == 0) {
        # Student not enrolled, proceed with insertion
        result <- dbExecute(con, 
                            "INSERT INTO remedial_enrollments (student_id, quarter, subject, total_attendance) 
           VALUES (?, ?, ?, ?)",
                            params = list(as.integer(student_id), quarter, subject, as.integer(initial_attendance)))
        
        if (result == 1) {
          success_count <- success_count + 1
          cat("Successfully enrolled student:", student_id, "with attendance:", initial_attendance, "\n")
        }
      } else {
        duplicate_count <- duplicate_count + 1
        cat("Student already enrolled:", student_id, "\n")
      }
      
    }, error = function(e) {
      # Log the actual error for debugging
      cat("Error enrolling student", student_id, ":", e$message, "\n")
    })
  }
  
  return(list(success = success_count, duplicates = duplicate_count))
}

# Update attendance (changed to set attendance directly rather than increment)
update_attendance <- function(con, enrollment_id, attendance) {
  dbExecute(con, 
            "UPDATE remedial_enrollments 
     SET total_attendance = ?, updated_at = CURRENT_TIMESTAMP 
     WHERE enrollment_id = ?",
            params = list(attendance, enrollment_id))
}

# Set attendance for multiple students (new function for direct setting)
set_attendance_for_students <- function(con, enrollment_ids, new_attendance) {
  success_count <- 0
  
  for (enrollment_id in enrollment_ids) {
    tryCatch({
      # Update with new attendance value directly
      dbExecute(con, 
                "UPDATE remedial_enrollments 
         SET total_attendance = ?, updated_at = CURRENT_TIMESTAMP 
         WHERE enrollment_id = ?",
                params = list(new_attendance, enrollment_id))
      
      success_count <- success_count + 1
      cat("Set attendance for enrollment_id", enrollment_id, "to:", new_attendance, "\n")
      
    }, error = function(e) {
      cat("Error setting attendance for enrollment", enrollment_id, ":", e$message, "\n")
    })
  }
  
  return(success_count)
}

# Remove enrollment
remove_enrollment <- function(con, enrollment_id) {
  dbExecute(con, 
            "UPDATE remedial_enrollments 
     SET status = 'Inactive', updated_at = CURRENT_TIMESTAMP 
     WHERE enrollment_id = ?",
            params = list(enrollment_id))
}

# =============================================================================
# UI MODULE
# =============================================================================

student_enrollment_ui <- function(id) {
  ns <- NS(id)
  
  tabsetPanel(
    id = ns("main_tabs"),
    type = "tabs",
    
    # Student Registry Tab
    tabPanel(
      title = "Student Registry",
      value = "registry",
      icon = icon("users"),
      br(),
      fluidRow(
        box(
          title = "Student Registry", status = "primary", solidHeader = TRUE, width = 12,
          p("Complete student registry with test scores and proficiency levels."),
          DT::dataTableOutput(ns("remedial_student_registry"))
        )
      )
    ),
    
    # Class Enrollment Tab
    tabPanel(
      title = "Enroll Students",
      value = "enrollment",
      icon = icon("chalkboard"),
      br(),
      fluidRow(
        box(
          title = "Enrollment Controls", status = "info", solidHeader = TRUE, width = 4,
          h4("Step 1: Select Quarter and Subject"),
          selectInput(ns("quarter"), "Quarter:", 
                      choices = c("Q1", "Q2", "Q3", "Q4"), 
                      selected = "Q1"),
          selectInput(ns("subject"), "Subject:", 
                      choices = c("English" = "english", 
                                  "Math" = "math", 
                                  "Science" = "science"), 
                      selected = "english"),
          
          hr(),
          h4("Step 2: Filter Students"),
          selectInput(ns("grade_filter"), "Filter by Grade Level:", 
                      choices = c("All Grades" = "all"),
                      selected = "all"),
          numericInput(ns("score_threshold"), "Show students below score:", 
                       value = 75, min = 0, max = 100, step = 5),
          
          hr(),
          h4("Step 3: Sort Students"),
          selectInput(ns("sort_field"), "Sort by:", 
                      choices = c("Student Name" = "student_name",
                                  "Grade Level" = "grade_level",
                                  "Math Average" = "math_avg",
                                  "Science Average" = "sci_avg",
                                  "English Average" = "eng_avg",
                                  "Overall Average" = "overall_avg"), 
                      selected = "overall_avg"),
          selectInput(ns("sort_order"), "Sort Order:", 
                      choices = c("Ascending (Low to High)" = "asc", 
                                  "Descending (High to Low)" = "desc"), 
                      selected = "asc"),
          
          hr(),
          h4("Step 4: Set Initial Attendance"),
          numericInput(ns("initial_attendance"), "Initial Attendance Count:", 
                       value = 0, min = 0, max = 100, step = 1,
                       width = "100%"),
          helpText("Set the starting attendance count for newly enrolled students."),
          
          hr(),
          h4("Step 5: Enroll Students"),
          actionButton(ns("enroll_btn"), "Enroll Selected Students", 
                       class = "btn-success", style = "width: 100%;"),
          br(), br(),
          actionButton(ns("refresh_data"), "Refresh Data", 
                       class = "btn-info", style = "width: 100%;")
        ),
        
        box(
          title = "Student Selection", status = "primary", solidHeader = TRUE, width = 8,
          p("Select students to enroll in the chosen remedial class."),
          DT::dataTableOutput(ns("enrollment_selection_table"))
        )
      ),
      
      fluidRow(
        box(
          title = "Current Enrollment Status", status = "success", solidHeader = TRUE, width = 12,
          h4(textOutput(ns("enrollment_title"))),
          DT::dataTableOutput(ns("current_class_enrollment"))
        )
      )
    ),
    
    # Manage Enrollments Tab (UPDATED)
    tabPanel(
      title = "Manage Classes",
      value = "manage",
      icon = icon("edit"),
      br(),
      fluidRow(
        box(
          title = "Manage Enrollments & Attendance", status = "warning", solidHeader = TRUE, width = 12,
          h4("Update Attendance and Remove Students"),
          
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
            column(6,
                   actionButton(ns("set_attendance_all"), "Set Attendance for All Students", 
                                class = "btn-success", style = "width: 100%; margin-top: 10px;")
            ),
            column(6,
                   actionButton(ns("set_attendance_selected"), "Set Attendance for Selected", 
                                class = "btn-warning", style = "width: 100%; margin-top: 10px;")
            )
          ),
          
          hr(),
          p("• Click on attendance numbers to edit directly"),
          p("• Select students and use 'Set Attendance for Selected' to update their attendance"),
          p("• Use 'Set Attendance for All Students' to set everyone's attendance to the same value"),
          p("• Use Remove buttons to unenroll students"),
          h4(textOutput(ns("manage_title"))),
          DT::dataTableOutput(ns("attendance_management_table"))
        )
      )
    ),
    
    # View All Classes Tab
    tabPanel(
      title = "Class Overview",
      value = "view",
      icon = icon("eye"),
      br(),
      fluidRow(
        box(
          title = "All Remedial Class Enrollments", status = "primary", solidHeader = TRUE, width = 12,
          
          fluidRow(
            column(6,
                   checkboxInput(ns("show_inactive"), "Show inactive enrollments", value = FALSE)
            ),
            column(6,
                   downloadButton(ns("download_enrollments"), "Download All Data", 
                                  class = "btn-success", style = "float: right;")
            )
          ),
          
          hr(),
          DT::dataTableOutput(ns("all_enrollments_overview"))
        )
      )
    )
  )
}

# =============================================================================
# SERVER MODULE
# =============================================================================

student_enrollment_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize database
    create_enrollment_table(con)
    
    # Reactive values
    values <- reactiveValues(
      student_data = NULL,
      enrollment_data = NULL,
      last_refresh = Sys.time()
    )
    
    # Load initial data
    observe({
      values$student_data <- get_student_registry(con)
      
      # Update grade filter choices
      grades <- sort(unique(values$student_data$grade_level))
      grade_choices <- c("All Grades" = "all")
      for (grade in grades) {
        grade_choices[paste("Grade", grade)] <- as.character(grade)
      }
      updateSelectInput(session, "grade_filter", choices = grade_choices)
    })
    
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
    
    # Student Registry Table
    output$remedial_student_registry <- DT::renderDataTable({
      req(values$student_data)
      
      display_data <- values$student_data %>%
        select(student_id, student_name, grade_level, section, Total_score_per,
               RMA_before_math_proficiency, before_reading_proficiency,
               starts_with("testq"), math_avg, sci_avg, eng_avg, overall_avg)
      
      DT::datatable(
        display_data,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE
        ),
        caption = "Complete Student Registry"
      ) %>%
        DT::formatRound(columns = c("Total_score_per", "math_avg", "sci_avg", "eng_avg", "overall_avg"), digits = 1)
    })
    
    # Filtered and sorted students for enrollment
    filtered_students <- reactive({
      req(values$student_data)
      
      data <- values$student_data
      
      # Apply grade filter
      if (input$grade_filter != "all") {
        data <- data %>% filter(grade_level == as.numeric(input$grade_filter))
      }
      
      # Apply score threshold filter based on subject
      score_col <- case_when(
        input$subject == "math" ~ "math_avg",
        input$subject == "science" ~ "sci_avg",
        input$subject == "english" ~ "eng_avg",
        TRUE ~ "overall_avg"
      )
      
      data <- data %>% filter(.data[[score_col]] < input$score_threshold | is.na(.data[[score_col]]))
      
      # Apply sorting
      sort_col <- input$sort_field
      if (input$sort_order == "desc") {
        data <- data %>% arrange(desc(.data[[sort_col]]))
      } else {
        data <- data %>% arrange(.data[[sort_col]])
      }
      
      return(data)
    })
    
    # Selection table for enrollment
    output$enrollment_selection_table <- DT::renderDataTable({
      req(filtered_students())
      
      display_data <- filtered_students() %>%
        select(student_id, student_name, grade_level, section, 
               math_avg, sci_avg, eng_avg, overall_avg)
      
      DT::datatable(
        display_data,
        selection = 'multiple',
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = TRUE,
          ordering = FALSE
        ),
        caption = paste("Select students for", toupper(input$subject), "remedial class in", input$quarter)
      ) %>%
        DT::formatRound(columns = c("math_avg", "sci_avg", "eng_avg", "overall_avg"), digits = 1)
    })
    
    # Handle enrollment
    observeEvent(input$enroll_btn, {
      selected_rows <- input$enrollment_selection_table_rows_selected
      
      if (length(selected_rows) > 0) {
        # Fix: Extract individual student IDs properly
        selected_student_data <- filtered_students()[selected_rows, ]
        selected_students <- selected_student_data$student_id
        
        # Get the initial attendance value
        initial_attendance <- input$initial_attendance
        
        # Validate attendance input
        if (is.na(initial_attendance) || initial_attendance < 0) {
          showNotification("Please enter a valid attendance count (0 or greater)", type = "error")
          return()
        }
        
        # Debug: Print what we're getting
        cat("Selected rows:", selected_rows, "\n")
        cat("Selected student IDs:", selected_students, "\n")
        cat("Initial attendance:", initial_attendance, "\n")
        
        result <- enroll_students(con, selected_students, input$quarter, input$subject, initial_attendance)
        
        if (result$success > 0) {
          showNotification(
            paste("Successfully enrolled", result$success, "students in", 
                  toupper(input$subject), "for", input$quarter, 
                  "with initial attendance of", initial_attendance),
            type = "message"
          )
        }
        
        if (result$duplicates > 0) {
          showNotification(
            paste(result$duplicates, "students were already enrolled in this class"),
            type = "warning"
          )
        }
        
        if (result$success == 0 && result$duplicates == 0) {
          showNotification("No students could be enrolled. Please check the data.", type = "error")
        }
        
        # Refresh enrollment data
        values$last_refresh <- Sys.time()
      } else {
        showNotification("Please select students to enroll", type = "warning")
      }
    })
    
    # Current enrollment status
    current_enrollment <- reactive({
      values$last_refresh  # Dependency to trigger refresh
      get_enrollment_data(con, input$quarter, input$subject)
    })
    
    output$enrollment_title <- renderText({
      enrolled_count <- nrow(current_enrollment())
      paste("Currently Enrolled in", toupper(input$subject), "-", input$quarter, ":", enrolled_count, "students")
    })
    
    output$current_class_enrollment <- DT::renderDataTable({
      enrollment_data <- current_enrollment()
      
      if (nrow(enrollment_data) > 0) {
        display_data <- enrollment_data %>%
          select(student_name, grade_level, section, enrollment_date, total_attendance, status)
        
        DT::datatable(
          display_data,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            searching = TRUE
          )
        )
      } else {
        empty_df <- data.frame(Message = "No students currently enrolled in this class")
        DT::datatable(
          empty_df,
          options = list(searching = FALSE, paging = FALSE, info = FALSE)
        )
      }
    })
    
    # Management table (UPDATED with grade level filtering)
    manage_enrollment <- reactive({
      input$refresh_manage  # Dependency for refresh
      values$last_refresh   # Additional dependency
      get_enrollment_data(con, input$manage_quarter, input$manage_subject, input$manage_grade)
    })
    
    output$manage_title <- renderText({
      enrollment_data <- manage_enrollment()
      grade_text <- if(input$manage_grade == "all") "All Grades" else paste("Grade", input$manage_grade)
      paste("Managing", toupper(input$manage_subject), "-", input$manage_quarter, 
            "- (", grade_text, "):", nrow(enrollment_data), "students")
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
          enrollment_data %>% select(student_name, grade_level, section, enrollment_date, total_attendance, Actions),
          editable = list(target = 'cell', disable = list(columns = c(0:3, 5))),
          escape = FALSE,
          selection = 'multiple',
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            searching = TRUE
          ),
          caption = "Select students to set attendance, or click attendance numbers to edit directly"
        )
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
          values$last_refresh <- Sys.time()
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
      values$last_refresh <- Sys.time()
    })
    
    # Handle setting attendance for all students (UPDATED)
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
        values$last_refresh <- Sys.time()
      } else {
        showNotification("No students to update", type = "warning")
      }
    })
    
    # Handle setting attendance for selected students (UPDATED)
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
        values$last_refresh <- Sys.time()
      } else {
        showNotification("Please select students first", type = "warning")
      }
    })
    
    # Overview table for all enrollments
    output$all_enrollments_overview <- DT::renderDataTable({
      all_enrollments <- if (input$show_inactive) {
        dbGetQuery(con, "
          SELECT 
            e.enrollment_id,
            e.quarter,
            e.subject,
            s.student_name,
            s.grade_level,
            s.section,
            e.enrollment_date,
            e.total_attendance,
            e.status
          FROM remedial_enrollments e
          LEFT JOIN full_student_registry s ON e.student_id = s.student_id
          ORDER BY e.quarter, e.subject, s.grade_level, s.student_name
        ")
      } else {
        get_enrollment_data(con)
      }
      
      if (nrow(all_enrollments) > 0) {
        DT::datatable(
          all_enrollments %>% select(-enrollment_id),
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            searching = TRUE
          ),
          caption = "All Remedial Class Enrollments"
        )
      } else {
        empty_df <- data.frame(Message = "No enrollment data found")
        DT::datatable(
          empty_df,
          options = list(searching = FALSE, paging = FALSE, info = FALSE)
        )
      }
    })
    
    # Download handler
    output$download_enrollments <- downloadHandler(
      filename = function() {
        paste("remedial_enrollments_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        all_data <- dbGetQuery(con, "
          SELECT 
            e.quarter,
            e.subject,
            s.student_name,
            s.grade_level,
            s.section,
            e.enrollment_date,
            e.total_attendance,
            e.status,
            s.Total_score_per,
            s.RMA_before_math_proficiency,
            s.before_reading_proficiency
          FROM remedial_enrollments e
          LEFT JOIN full_student_registry s ON e.student_id = s.student_id
          ORDER BY e.quarter, e.subject, s.grade_level, s.student_name
        ")
        write.csv(all_data, file, row.names = FALSE)
      }
    )
    
    # Refresh data
    observeEvent(input$refresh_data, {
      values$student_data <- get_student_registry(con)
      values$last_refresh <- Sys.time()
      showNotification("Data refreshed successfully", type = "message")
    })
  })
}