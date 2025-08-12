# Enhanced Word Document Generation Function (No Page Breaks)
generate_word_report <- function(dashboard_summary, ai_sections, output_path, app_title) {
  tryCatch({
    doc <- read_docx()
    
    # Header
    doc <- doc %>%
      body_add_par("Enrollment, Attendance and Completion Monitoring", style = "heading 1") %>%
      body_add_par("Introduction and Enrollment Criteria", style = "heading 2") %>%
      body_add_par(paste("Generated on:", format(Sys.Date(), "%B %d, %Y")), style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # Executive Summary
    doc <- doc %>%
      body_add_par("EXECUTIVE SUMMARY", style = "heading 2") %>%
      body_add_par(ai_sections$executive_summary, style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # Sections
    doc <- add_key_metrics_section(doc, dashboard_summary)
    doc <- add_grade_analysis_section(doc, dashboard_summary, ai_sections$grade_analysis)
    doc <- add_subject_analysis_section(doc, dashboard_summary, ai_sections$subject_analysis)
    
    # Attendance Insights
    doc <- doc %>%
      body_add_par("ATTENDANCE PATTERNS & INSIGHTS", style = "heading 2") %>%
      body_add_par(ai_sections$attendance_insights, style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # Recommendations
    doc <- doc %>%
      body_add_par("STRATEGIC RECOMMENDATIONS", style = "heading 2") %>%
      body_add_par(ai_sections$recommendations, style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # Detailed Class Lists
    doc <- add_detailed_class_lists(doc, dashboard_summary)
    
    # Footer
    doc <- add_report_footer(doc, app_title)
    
    # Save
    print(doc, target = output_path)
    message("✓ Word document successfully generated at: ", output_path)
    return(TRUE)
    
  }, error = function(e) {
    message("❌ Error in generate_word_report: ", e$message)
    print(traceback())
    stop(e)
  })
}

# Helper: Key Metrics Section
add_key_metrics_section <- function(doc, dashboard_summary) {
  doc <- doc %>%
    body_add_par("KEY PROGRAM METRICS", style = "heading 2")
  
  pass_rate <- round((dashboard_summary$passed_students / dashboard_summary$total_enrolled) * 100, 1)
  active_rate <- round((dashboard_summary$active_students / dashboard_summary$total_enrolled) * 100, 1)
  
  metrics_data <- data.frame(
    Metric = c("Total Enrolled Students", "Currently Active Students", "Students Who Passed", 
               "Average Attendance (Sessions)", "Success Rate", "Active Retention Rate"),
    Value = c(
      dashboard_summary$total_enrolled,
      dashboard_summary$active_students, 
      dashboard_summary$passed_students,
      dashboard_summary$avg_attendance,
      paste0(pass_rate, "%"),
      paste0(active_rate, "%")
    ),
    stringsAsFactors = FALSE
  )
  
  metrics_table <- flextable(metrics_data) %>%
    theme_booktabs() %>%
    autofit() %>%
    bg(part = "header", bg = "#3498db") %>%
    color(part = "header", color = "white") %>%
    bold(part = "header")
  
  doc <- doc %>%
    body_add_flextable(metrics_table) %>%
    body_add_par("", style = "Normal")
  
  return(doc)
}

# Helper: Grade Analysis
add_grade_analysis_section <- function(doc, dashboard_summary, grade_analysis_text) {
  doc <- doc %>%
    body_add_par("GRADE LEVEL ENROLLMENT ANALYSIS", style = "heading 2") %>%
    body_add_par(grade_analysis_text, style = "Normal") %>%
    body_add_par("", style = "Normal")
  
  if (length(dashboard_summary$grade_dist) > 0) {
    grade_data <- data.frame(
      Grade_Level = paste("Grade", names(dashboard_summary$grade_dist)),
      Students_Enrolled = as.numeric(dashboard_summary$grade_dist),
      Percentage = paste0(round((as.numeric(dashboard_summary$grade_dist) / dashboard_summary$total_enrolled) * 100, 1), "%"),
      stringsAsFactors = FALSE
    )
    
    grade_table <- flextable(grade_data) %>%
      theme_booktabs() %>%
      autofit() %>%
      bg(part = "header", bg = "#9b59b6") %>%
      color(part = "header", color = "white") %>%
      bold(part = "header")
    
    doc <- doc %>%
      body_add_flextable(grade_table) %>%
      body_add_par("", style = "Normal")
  }
  
  return(doc)
}

# Helper: Subject Analysis
add_subject_analysis_section <- function(doc, dashboard_summary, subject_analysis_text) {
  doc <- doc %>%
    body_add_par("SUBJECT PERFORMANCE ANALYSIS", style = "heading 2") %>%
    body_add_par(subject_analysis_text, style = "Normal") %>%
    body_add_par("", style = "Normal")
  
  if (length(dashboard_summary$subject_dist) > 0) {
    subject_names <- names(dashboard_summary$subject_dist)
    avg_sessions_aligned <- numeric(length(subject_names))
    
    for (i in seq_along(subject_names)) {
      subject_name <- subject_names[i]
      matching_avg <- dashboard_summary$avg_sessions[names(dashboard_summary$avg_sessions) == tools::toTitleCase(subject_name)]
      avg_sessions_aligned[i] <- ifelse(length(matching_avg) > 0, matching_avg, 0)
    }
    
    subject_data <- data.frame(
      Subject = tools::toTitleCase(subject_names),
      Students_Enrolled = as.numeric(dashboard_summary$subject_dist),
      Average_Sessions = round(avg_sessions_aligned, 1),
      Engagement_Level = ifelse(avg_sessions_aligned > mean(avg_sessions_aligned, na.rm = TRUE), "High", "Moderate"),
      stringsAsFactors = FALSE
    )
    
    subject_table <- flextable(subject_data) %>%
      theme_booktabs() %>%
      autofit() %>%
      bg(part = "header", bg = "#e67e22") %>%
      color(part = "header", color = "white") %>%
      bold(part = "header") %>%
      bg(i = ~ Engagement_Level == "High", j = "Engagement_Level", bg = "#d5f4e6") %>%
      bg(i = ~ Engagement_Level == "Moderate", j = "Engagement_Level", bg = "#fff3cd")
    
    doc <- doc %>%
      body_add_flextable(subject_table) %>%
      body_add_par("", style = "Normal")
  }
  
  return(doc)
}

# Helper: Detailed Class Lists
add_detailed_class_lists <- function(doc, dashboard_summary) {
  doc <- doc %>%
    body_add_par("DETAILED ENROLLMENT BY CLASS", style = "heading 2") %>%
    body_add_par("The following section provides comprehensive enrollment details organized by quarter, subject, and grade level, including individual student attendance patterns.", style = "Normal") %>%
    body_add_par("", style = "Normal")
  
  grouped_data <- dashboard_summary$data %>%
    group_by(quarter, subject, grade_level) %>%
    group_split()
  
  for (group in grouped_data) {
    if (nrow(group) > 0) {
      quarter <- group$quarter[1]
      subject <- tools::toTitleCase(group$subject[1])
      grade <- group$grade_level[1]
      
      doc <- doc %>%
        body_add_par(paste("Grade", grade, "-", subject, "-", quarter, 
                           "(", nrow(group), "students)"), style = "heading 3")
      
      class_summary <- group %>%
        summarise(
          total_students = n(),
          active_students = sum(status == "Active", na.rm = TRUE),
          passed_students = sum(status == "Passed", na.rm = TRUE),
          avg_attendance = round(mean(total_attendance, na.rm = TRUE), 1),
          .groups = 'drop'
        )
      
      doc <- doc %>%
        body_add_par(paste0("Class Summary: ", class_summary$active_students, " active, ", 
                            class_summary$passed_students, " passed, average ", 
                            class_summary$avg_attendance, " sessions attended"), style = "Normal")
      
      class_data <- group %>%
        select(student_name, section, total_attendance, status) %>%
        arrange(desc(total_attendance), section, student_name) %>%
        slice_head(n = 20)
      
      names(class_data) <- c("Student Name", "Section", "Sessions Attended", "Status")
      
      class_table <- flextable(class_data) %>%
        theme_booktabs() %>%
        autofit() %>%
        bg(part = "header", bg = "#34495e") %>%
        color(part = "header", color = "white") %>%
        bold(part = "header") %>%
        bg(i = ~ Status == "Active", j = "Status", bg = "#d4edda") %>%
        bg(i = ~ Status == "Passed", j = "Status", bg = "#cce5ff")
      
      doc <- doc %>%
        body_add_flextable(class_table)
      
      if (nrow(group) > 20) {
        doc <- doc %>%
          body_add_par(paste("(Showing top 20 of", nrow(group), "students by attendance)"), style = "Normal")
      }
      
      doc <- doc %>% body_add_par("", style = "Normal")
    }
  }
  
  return(doc)
}

# Helper: Footer
add_report_footer <- function(doc, app_title) {
  doc <- doc %>%
    body_add_par("---", style = "Normal") %>%
    body_add_par("REPORT DETAILS", style = "heading 3") %>%
    body_add_par(paste("Report generated by:", app_title), style = "Normal") %>%
    body_add_par(paste("Generated on:", format(Sys.time(), "%B %d, %Y at %I:%M %p")), style = "Normal") %>%
    body_add_par("AI Analysis powered by Google Gemini", style = "Normal") %>%
    body_add_par("This report provides comprehensive analysis of tutoring program data with actionable insights for program improvement.", style = "Normal")
  
  return(doc)
}
