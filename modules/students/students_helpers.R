# UI Helper Functions
create_student_modal <- function(ns) {
  bsModal("student_modal", "Student Information", ns("add_student"), size = "large",
          fluidRow(
            column(6,
                   textInput(ns("student_name"), "Student Name:", ""),
                   numericInput(ns("grade_level"), "Grade Level:", value = 7, min = 1, max = 12),
                   textInput(ns("section"), "Section:", ""),
                   textInput(ns("LRN"), "LRN:", "")
            ),
            column(6,
                   textInput(ns("address"), "Address:", ""),
                   textAreaInput(ns("narrative"), "Narrative:", "", rows = 3),
                   textInput(ns("parent_name"), "Parent Name:", ""),
                   textInput(ns("parent_contact"), "Parent Contact:", "")
            )
          ),
          br(),
          checkboxInput(ns("consent_given"), "Consent Given", FALSE),
          
          footer = tagList(
            actionButton(ns("save_student"), "Save", class = "btn-primary"),
            actionButton(ns("cancel_student"), "Cancel", class = "btn-default")
          )
  )
}

create_analytics_filters <- function(ns) {
  fluidRow(
    column(3,
           selectInput(ns("filter_grade"), "Grade Level:",
                       choices = NULL, selected = NULL)
    ),
    column(3,
           selectInput(ns("filter_subject"), "Subject:",
                       choices = NULL, selected = NULL)
    ),
    column(3,
           selectInput(ns("filter_competency"), "Learning Competency:",
                       choices = NULL, selected = NULL)
    ),
    column(3,
           selectInput(ns("filter_student"), "Student:",
                       choices = NULL, selected = NULL)
    )
  )
}

create_analytics_dashboard <- function(ns) {
  tagList(
    # Summary Cards
    fluidRow(
      column(3,
             div(class = "value-box",
                 h3(textOutput(ns("total_sessions"))),
                 p("Total Sessions")
             )
      ),
      column(9,
             wellPanel(
               h4("Sessions by Subject", style = "color: #2c3e50;"),
               withSpinner(plotlyOutput(ns("sessions_by_subject_chart")))
             )
      )
    ),
    
    br(),
    
    fluidRow(
      column(6,
             wellPanel(
               h4("Sessions by Learning Competency", style = "color: #2c3e50;"),
               withSpinner(plotlyOutput(ns("sessions_by_competency_chart")))
             )
      ),
      column(6,
             wellPanel(
               h4("Assessment Scores", style = "color: #2c3e50;"),
               withSpinner(plotlyOutput(ns("student_assessment_chart")))
             )
      )
    )
  )
}

# Data Processing Helper Functions
handle_file_upload <- function(upload_file) {
  req(upload_file)
  
  ext <- tools::file_ext(upload_file$name)
  
  tryCatch({
    if (ext == "csv") {
      df <- read_csv(upload_file$datapath, show_col_types = FALSE)
    } else if (ext %in% c("xls", "xlsx")) {
      df <- read_excel(upload_file$datapath)
    } else {
      showNotification("Unsupported file type.", type = "error")
      return(NULL)
    }
    
    # Standardize columns
    df <- standardize_student_columns(df)
    showNotification("File uploaded successfully. Review preview before saving.", type = "message")
    return(df)
    
  }, error = function(e) {
    showNotification(paste("Error reading file:", e$message), type = "error")
    return(NULL)
  })
}

standardize_student_columns <- function(df) {
  required_cols <- c("student_name", "grade_level", "section", "LRN", 
                     "address", "narrative", "parent_name", "parent_contact", "consent_given")
  
  # Add missing columns as NA
  for (col in required_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  
  # Ensure correct column order
  df <- df[, required_cols]
  
  # Ensure consent_given is logical
  if (!is.logical(df$consent_given)) {
    df$consent_given <- as.logical(df$consent_given)
  }
  
  return(df)
}

process_student_upload <- function(con, uploaded_data, shared_values) {
  # Get existing LRNs
  existing_lrns <- dbGetQuery(con, "SELECT LRN FROM students_registry")$LRN
  
  # Filter out duplicates
  new_df <- uploaded_data[!uploaded_data$LRN %in% existing_lrns | is.na(uploaded_data$LRN), ]
  skipped_count <- nrow(uploaded_data) - nrow(new_df)
  
  if (nrow(new_df) == 0) {
    showNotification("All records are duplicates or missing LRN. No new students added.", type = "warning")
    return()
  }
  
  # Insert each row
  success_count <- 0
  for (i in 1:nrow(new_df)) {
    result <- insert_data(con, "students_registry", new_df[i, ])
    if (result$success) {
      success_count <- success_count + 1
    }
  }
  
  showNotification(
    paste0("Upload complete: ", success_count, " added, ", skipped_count, " skipped."),
    type = "message"
  )
  
  # Trigger refresh
  shared_values$refresh_trigger <- shared_values$refresh_trigger + 1
}

render_students_table <- function(students_data) {
  display_data <- students_data %>%
    select(student_name, grade_level, section, LRN, address, parent_name, parent_contact, consent_given)
  
  DT::datatable(display_data,
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE,
                selection = "single") %>%
    DT::formatStyle(columns = "consent_given", 
                    backgroundColor = DT::styleEqual("TRUE", "#d4edda"))
}

handle_student_selection <- function(selected_row, students_data, values, session) {
  student_data <- students_data[selected_row, ]
  values$editing_id <- student_data$id
  
  # Populate modal fields
  updateTextInput(session, "student_name", value = student_data$student_name)
  updateNumericInput(session, "grade_level", value = student_data$grade_level)
  updateTextInput(session, "section", value = student_data$section)
  updateTextInput(session, "LRN", value = student_data$LRN)
  updateTextInput(session, "address", value = student_data$address)
  updateTextAreaInput(session, "narrative", value = student_data$narrative)
  updateTextInput(session, "parent_name", value = student_data$parent_name)
  updateTextInput(session, "parent_contact", value = student_data$parent_contact)
  updateCheckboxInput(session, "consent_given", value = as.logical(student_data$consent_given))
  
  toggleModal(session, "student_modal", toggle = "open")
}

open_student_modal <- function(values, session, mode = "add") {
  values$editing_id <- NULL
  
  # Clear modal fields
  updateTextInput(session, "student_name", value = "")
  updateNumericInput(session, "grade_level", value = 7)
  updateTextInput(session, "section", value = "")
  updateTextInput(session, "LRN", value = "")
  updateTextInput(session, "address", value = "")
  updateTextAreaInput(session, "narrative", value = "")
  updateTextInput(session, "parent_name", value = "")
  updateTextInput(session, "parent_contact", value = "")
  updateCheckboxInput(session, "consent_given", value = FALSE)
  
  toggleModal(session, "student_modal", toggle = "open")
}

save_student_data <- function(input, con, values, shared_values, session) {
  req(input$student_name, input$LRN)
  
  student_data <- data.frame(
    student_name = input$student_name,
    grade_level = input$grade_level,
    section = input$section,
    LRN = input$LRN,
    address = input$address,
    narrative = input$narrative,
    parent_name = input$parent_name,
    parent_contact = input$parent_contact,
    consent_given = input$consent_given,
    stringsAsFactors = FALSE
  )
  
  if (is.null(values$editing_id)) {
    result <- insert_data(con, "students_registry", student_data)
  } else {
    result <- update_data(con, "students_registry", student_data, "id", values$editing_id)
  }
  
  if (result$success) {
    showNotification(result$message, type = "message")
    shared_values$refresh_trigger <- shared_values$refresh_trigger + 1
    toggleModal(session, "student_modal", toggle = "close")
  } else {
    showNotification(result$message, type = "error")
  }
}

# Analytics Helper Functions
update_filter_choices <- function(input, output, session, con, students_data) {
  # Grade levels
  grades <- sort(unique(students_data$grade_level))
  updateSelectInput(session, "filter_grade", 
                    choices = c("All" = "", grades))
  
  # Subjects from enrollment
  subjects <- dbGetQuery(con, "SELECT DISTINCT subject FROM enrollment_table")$subject
  updateSelectInput(session, "filter_subject",
                    choices = c("All" = "", subjects))
  
  # Learning competencies
  competencies <- dbGetQuery(con, "SELECT DISTINCT learning_competency FROM teacher_sessions WHERE learning_competency IS NOT NULL")$learning_competency
  updateSelectInput(session, "filter_competency",
                    choices = c("All" = "", competencies))
  
  # Students
  students <- students_data$student_name
  updateSelectInput(session, "filter_student",
                    choices = c("All" = "", students))
}

build_filter_conditions <- function(input) {
  conditions <- c()
  
  if (!is.null(input$filter_grade) && input$filter_grade != "") {
    conditions <- c(conditions, paste0("sr.grade_level = ", input$filter_grade))
  }
  
  if (!is.null(input$filter_subject) && input$filter_subject != "") {
    conditions <- c(conditions, paste0("ts.subject = '", input$filter_subject, "'"))
  }
  
  if (!is.null(input$filter_competency) && input$filter_competency != "") {
    conditions <- c(conditions, paste0("ts.learning_competency = '", input$filter_competency, "'"))
  }
  
  if (!is.null(input$filter_student) && input$filter_student != "") {
    conditions <- c(conditions, paste0("sr.student_name = '", input$filter_student, "'"))
  }
  
  if (length(conditions) > 0) {
    return(paste0("WHERE ", paste(conditions, collapse = " AND ")))
  } else {
    return("")
  }
}

calculate_total_sessions <- function(con, input) {
  filter_conditions <- build_filter_conditions(input)
  
  query <- paste0("
    SELECT COUNT(*) as count 
    FROM teacher_sessions ts
    JOIN students_registry sr ON ts.list_of_students LIKE '%' || sr.student_name || '%'
    ", filter_conditions)
  
  result <- dbGetQuery(con, query)$count
  if (is.na(result)) result <- 0
  result
}

create_sessions_by_subject_chart <- function(con, input) {
  filter_conditions <- build_filter_conditions(input)
  
  query <- paste0("
    SELECT ts.subject, COUNT(*) as session_count
    FROM teacher_sessions ts
    JOIN students_registry sr ON ts.list_of_students LIKE '%' || sr.student_name || '%'
    ", filter_conditions, "
    GROUP BY ts.subject
    ORDER BY session_count DESC
  ")
  
  subject_data <- dbGetQuery(con, query)
  
  if (nrow(subject_data) > 0) {
    p <- plot_ly(subject_data, 
                 x = ~reorder(subject, session_count), 
                 y = ~session_count,
                 type = 'bar',
                 marker = list(color = '#3498db'),
                 hovertemplate = '<b>%{x}</b><br>Sessions: %{y}<extra></extra>') %>%
      layout(title = "",
             xaxis = list(title = "Subject"),
             yaxis = list(title = "Number of Sessions"))
    return(p)
  } else {
    return(plotly_empty() %>% layout(title = "No data available"))
  }
}

create_sessions_by_competency_chart <- function(con, input) {
  filter_conditions <- build_filter_conditions(input)
  
  query <- paste0("
    SELECT ts.learning_competency, COUNT(*) as session_count
    FROM teacher_sessions ts
    JOIN students_registry sr ON ts.list_of_students LIKE '%' || sr.student_name || '%'
    ", filter_conditions, "
    AND ts.learning_competency IS NOT NULL
    GROUP BY ts.learning_competency
    ORDER BY session_count DESC
    LIMIT 10
  ")
  
  competency_data <- dbGetQuery(con, query)
  
  if (nrow(competency_data) > 0) {
    p <- plot_ly(competency_data, 
                 labels = ~learning_competency, 
                 values = ~session_count,
                 type = 'pie',
                 hovertemplate = '<b>%{label}</b><br>Sessions: %{value}<extra></extra>') %>%
      layout(title = "")
    return(p)
  } else {
    return(plotly_empty() %>% layout(title = "No data available"))
  }
}

create_student_assessment_chart <- function(con, input) {
  student_filter <- if (!is.null(input$filter_student) && input$filter_student != "") {
    paste0("WHERE sr.student_name = '", input$filter_student, "'")
  } else {
    ""
  }
  
  query <- paste0("
    SELECT at.subject, 
           AVG(at.preassessment_score) as avg_pre,
           AVG(at.postassessment_score) as avg_post
    FROM assessment_table at
    JOIN students_registry sr ON at.student_LRN = sr.LRN
    ", student_filter, "
    GROUP BY at.subject
  ")
  
  assessment_data <- dbGetQuery(con, query)
  
  if (nrow(assessment_data) > 0) {
    p <- plot_ly(assessment_data) %>%
      add_trace(x = ~subject, y = ~avg_pre, type = 'bar', name = 'Pre-Assessment',
                marker = list(color = '#e74c3c')) %>%
      add_trace(x = ~subject, y = ~avg_post, type = 'bar', name = 'Post-Assessment',
                marker = list(color = '#27ae60')) %>%
      layout(title = "",
             xaxis = list(title = "Subject"),
             yaxis = list(title = "Average Score"),
             barmode = 'group')
    return(p)
  } else {
    return(plotly_empty() %>% layout(title = "No assessment data available"))
  }
}