
# Source required helper files
source("modules/enrollment/enrollment_dashboard/ai_helpers.R")
source("modules/enrollment/enrollment_dashboard/report_document_helpers.R")

# Generate dashboard summary for AI analysis
create_dashboard_summary <- function(dashboard_data) {
  data <- dashboard_data
  
  # Calculate summary statistics
  grade_dist <- table(data$grade_level)
  subject_dist <- table(tools::toTitleCase(data$subject))
  
  # Calculate average sessions by subject
  avg_sessions_df <- data %>% 
    group_by(subject) %>% 
    summarise(avg = round(mean(total_attendance, na.rm = TRUE), 1), .groups = 'drop')
  
  # Convert to named vector
  avg_sessions <- avg_sessions_df$avg
  names(avg_sessions) <- tools::toTitleCase(avg_sessions_df$subject)
  
  list(
    total_enrolled = nrow(data),
    active_students = sum(data$status == "Active", na.rm = TRUE),
    passed_students = sum(data$status == "Passed", na.rm = TRUE),
    avg_attendance = round(mean(data$total_attendance, na.rm = TRUE), 1),
    grade_dist = grade_dist,
    subject_dist = subject_dist,
    avg_sessions = avg_sessions,
    data = data
  )
}

# Enhanced report generation handler
handle_report_generation <- function(dashboard_data, app_title = "Tutoring Monitoring System") {
  
  tryCatch({
    summary_data <- create_dashboard_summary(dashboard_data)
    
    # Validate data before processing
    if (summary_data$total_enrolled == 0) {
      stop("No student data available for analysis")
    }
    
    # Generate AI analysis with better error handling
    ai_analysis <- gemini_analysis(summary_data)
    
    # Create temporary file
    temp_file <- tempfile(fileext = ".docx")
    
    # Generate Word document
    generate_word_report(summary_data, ai_analysis, temp_file, app_title)
    
    return(list(
      success = TRUE,
      file_path = temp_file,
      ai_analysis = ai_analysis,
      message = "Report generated successfully!"
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = e$message,
      message = paste("Report generation failed:", e$message)
    ))
  })
}