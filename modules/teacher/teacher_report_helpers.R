# =============================================================================
# TEACHERS AI REPORT GENERATION HELPERS
# =============================================================================

# =============================================================================
# 1. TEACHERS DASHBOARD SUMMARY CREATOR
# =============================================================================

create_teachers_dashboard_summary <- function(teachers_data, sessions_data = NULL) {
  # Basic teacher statistics
  total_teachers <- nrow(teachers_data)
  active_teachers <- sum(teachers_data$teacher_status == "Active", na.rm = TRUE)
  inactive_teachers <- sum(teachers_data$teacher_status == "Inactive", na.rm = TRUE)
  on_leave_teachers <- sum(teachers_data$teacher_status == "On Leave", na.rm = TRUE)
  
  # Subject and grade distribution from teacher registry
  subject_dist <- teachers_data %>%
    filter(!is.na(subject_handled) & subject_handled != "") %>%
    count(subject_handled, sort = TRUE)
  
  grade_dist <- teachers_data %>%
    filter(!is.na(year_level)) %>%
    count(year_level, sort = TRUE)
  
  # Session-based analytics (if sessions data is provided)
  session_analytics <- if (!is.null(sessions_data) && nrow(sessions_data) > 0) {
    # Total sessions and hours
    total_sessions <- nrow(sessions_data)
    total_hours <- sum(sessions_data$duration, na.rm = TRUE)
    avg_session_duration <- mean(sessions_data$duration, na.rm = TRUE)
    
    # Teacher performance metrics
    teacher_performance <- sessions_data %>%
      group_by(teacher_id) %>%
      summarise(
        total_sessions = n(),
        total_hours = sum(duration, na.rm = TRUE),
        avg_duration = mean(duration, na.rm = TRUE),
        subjects_taught = n_distinct(subject, na.rm = TRUE),
        competencies_covered = n_distinct(learning_competency, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Subject distribution from sessions
    session_subject_dist <- sessions_data %>%
      filter(!is.na(subject) & subject != "") %>%
      count(subject, sort = TRUE)
    
    # Grade distribution from sessions
    session_grade_dist <- sessions_data %>%
      filter(!is.na(grade_level)) %>%
      count(grade_level, sort = TRUE)
    
    # Learning competency analysis
    competency_dist <- sessions_data %>%
      filter(!is.na(learning_competency) & learning_competency != "") %>%
      count(learning_competency, sort = TRUE) %>%
      head(15)  # Top 15 competencies
    
    # Top performers
    top_performers <- teacher_performance %>%
      arrange(desc(total_hours)) %>%
      head(10)
    
    # Workload distribution analysis
    workload_analysis <- teacher_performance %>%
      mutate(
        workload_category = case_when(
          total_hours >= quantile(total_hours, 0.75, na.rm = TRUE) ~ "High",
          total_hours >= quantile(total_hours, 0.25, na.rm = TRUE) ~ "Medium",
          TRUE ~ "Low"
        )
      ) %>%
      count(workload_category)
    
    list(
      total_sessions = total_sessions,
      total_hours = total_hours,
      avg_session_duration = avg_session_duration,
      teacher_performance = teacher_performance,
      session_subject_dist = session_subject_dist,
      session_grade_dist = session_grade_dist,
      competency_dist = competency_dist,
      top_performers = top_performers,
      workload_analysis = workload_analysis
    )
  } else {
    NULL
  }
  
  # Engagement metrics (if sessions data available)
  engagement_metrics <- if (!is.null(sessions_data) && nrow(sessions_data) > 0) {
    # Teachers with recent activity (last 30 days)
    recent_cutoff <- Sys.Date() - 30
    if ("session_date" %in% names(sessions_data)) {
      recent_active <- sessions_data %>%
        filter(as.Date(session_date) >= recent_cutoff) %>%
        distinct(teacher_id) %>%
        nrow()
    } else {
      recent_active <- NA
    }
    
    # Average sessions per teacher
    avg_sessions_per_teacher <- if (nrow(session_analytics$teacher_performance) > 0) {
      mean(session_analytics$teacher_performance$total_sessions, na.rm = TRUE)
    } else {
      0
    }
    
    list(
      recent_active_teachers = recent_active,
      avg_sessions_per_teacher = avg_sessions_per_teacher
    )
  } else {
    list(
      recent_active_teachers = NA,
      avg_sessions_per_teacher = 0
    )
  }
  
  list(
    module_type = "teachers",
    total_teachers = total_teachers,
    active_teachers = active_teachers,
    inactive_teachers = inactive_teachers,
    on_leave_teachers = on_leave_teachers,
    subject_dist = subject_dist,
    grade_dist = grade_dist,
    session_analytics = session_analytics,
    engagement_metrics = engagement_metrics,
    data = teachers_data,
    sessions_data = sessions_data
  )
}

# =============================================================================
# 2. TEACHERS AI ANALYSIS FUNCTIONS
# =============================================================================

teachers_gemini_analysis <- function(teachers_summary) {
  message("Starting comprehensive Teachers AI analysis...")
  
  # Initialize results with fallback content
  results <- list(
    executive_summary = "Executive Summary: Teacher performance analysis reveals teaching capacity, workload distribution, and professional development insights across the faculty.",
    performance_analysis = "Performance analysis shows teaching hour distribution, session engagement, and productivity patterns among faculty members.",
    workload_distribution = "Workload analysis indicates teaching load balance, capacity utilization, and resource allocation patterns.",
    competency_analysis = "Learning competency analysis reveals curriculum coverage, specialization areas, and educational focus patterns.",
    engagement_insights = "Teacher engagement insights show participation patterns, activity levels, and professional commitment indicators.",
    recommendations = "1. Balance teaching workloads across faculty\n2. Optimize subject assignment strategies\n3. Enhance professional development programs\n4. Improve teacher engagement initiatives\n5. Develop performance recognition systems"
  )
  
  # Generate each section with proper error handling
  tryCatch({
    message("Generating executive summary...")
    exec_result <- analyze_teachers_executive_summary(teachers_summary)
    if (!is.null(exec_result)) results$executive_summary <- exec_result
  }, error = function(e) {
    message("Executive summary generation failed: ", e$message)
  })
  
  tryCatch({
    message("Analyzing teacher performance...")
    performance_result <- analyze_teacher_performance_patterns(teachers_summary)
    if (!is.null(performance_result)) results$performance_analysis <- performance_result
  }, error = function(e) {
    message("Performance analysis failed: ", e$message)
  })
  
  tryCatch({
    message("Analyzing workload distribution...")
    workload_result <- analyze_teacher_workload_distribution(teachers_summary)
    if (!is.null(workload_result)) results$workload_distribution <- workload_result
  }, error = function(e) {
    message("Workload analysis failed: ", e$message)
  })
  
  tryCatch({
    message("Analyzing learning competencies...")
    competency_result <- analyze_learning_competency_coverage(teachers_summary)
    if (!is.null(competency_result)) results$competency_analysis <- competency_result
  }, error = function(e) {
    message("Competency analysis failed: ", e$message)
  })
  
  tryCatch({
    message("Analyzing teacher engagement...")
    engagement_result <- analyze_teacher_engagement_patterns(teachers_summary)
    if (!is.null(engagement_result)) results$engagement_insights <- engagement_result
  }, error = function(e) {
    message("Engagement analysis failed: ", e$message)
  })
  
  tryCatch({
    message("Generating strategic recommendations...")
    rec_result <- generate_teachers_recommendations(teachers_summary)
    if (!is.null(rec_result)) results$recommendations <- rec_result
  }, error = function(e) {
    message("Recommendations generation failed: ", e$message)
  })
  
  message("Teachers AI analysis completed!")
  return(results)
}

# Individual analysis functions
analyze_teachers_executive_summary <- function(teachers_summary) {
  # Prepare data for prompt
  total_hours <- if (!is.null(teachers_summary$session_analytics)) {
    teachers_summary$session_analytics$total_hours
  } else {
    0
  }
  
  total_sessions <- if (!is.null(teachers_summary$session_analytics)) {
    teachers_summary$session_analytics$total_sessions
  } else {
    0
  }
  
  avg_hours_per_teacher <- if (teachers_summary$active_teachers > 0 && total_hours > 0) {
    round(total_hours / teachers_summary$active_teachers, 1)
  } else {
    0
  }
  
  prompt <- paste0(
    "As an educational administrator, write a comprehensive 3-4 paragraph executive summary for a teacher performance and capacity analysis report.\n\n",
    "TEACHER DATA:\n",
    "• Total Teachers: ", teachers_summary$total_teachers, "\n",
    "• Active Teachers: ", teachers_summary$active_teachers, "\n",
    "• Inactive/On Leave: ", teachers_summary$inactive_teachers + teachers_summary$on_leave_teachers, "\n",
    "• Total Teaching Sessions: ", total_sessions, "\n",
    "• Total Teaching Hours: ", total_hours, "\n",
    "• Average Hours per Active Teacher: ", avg_hours_per_teacher, "\n",
    "• Subject Areas Covered: ", nrow(teachers_summary$subject_dist), "\n",
    "• Grade Levels Served: ", nrow(teachers_summary$grade_dist), "\n\n",
    "REQUIREMENTS:\n",
    "- Start with an overview of the teaching faculty's scope and capacity\n",
    "- Highlight key performance metrics and workload distribution patterns\n",
    "- Discuss teacher engagement levels and professional contribution\n",
    "- Conclude with overall faculty effectiveness and strategic priorities\n",
    "- Use professional, stakeholder-appropriate language\n",
    "- Keep between 280-350 words\n\n",
    "Write the executive summary now:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 10))
}

analyze_teacher_performance_patterns <- function(teachers_summary) {
  if (is.null(teachers_summary$session_analytics)) {
    return("No session data available for performance analysis.")
  }
  
  session_data <- teachers_summary$session_analytics
  top_performers_count <- nrow(session_data$top_performers)
  
  prompt <- paste0(
    "Analyze teacher performance patterns and productivity distribution:\n\n",
    "PERFORMANCE METRICS:\n",
    "• Total Teaching Sessions: ", session_data$total_sessions, "\n",
    "• Total Teaching Hours: ", session_data$total_hours, "\n",
    "• Average Session Duration: ", round(session_data$avg_session_duration, 1), " hours\n",
    "• Top Performing Teachers: ", top_performers_count, "\n",
    "• Teachers with High Workload: ", session_data$workload_analysis$n[session_data$workload_analysis$workload_category == "High"], "\n",
    "• Active Teachers Contributing: ", nrow(session_data$teacher_performance), "\n\n",
    "Provide comprehensive analysis (220-280 words) covering:\n",
    "1. Distribution of teaching hours and session patterns across faculty\n",
    "2. Identification of high-performing and highly engaged teachers\n",
    "3. Session quality indicators and duration consistency\n",
    "4. Performance variations and capacity utilization\n",
    "5. Professional contribution patterns and teaching effectiveness\n",
    "6. Opportunities for performance optimization and development\n\n",
    "Focus on actionable insights for educational leadership and faculty development:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 9))
}

analyze_teacher_workload_distribution <- function(teachers_summary) {
  if (is.null(teachers_summary$session_analytics)) {
    return("No session data available for workload analysis.")
  }
  
  session_data <- teachers_summary$session_analytics
  workload_data <- session_data$workload_analysis
  
  high_workload <- workload_data$n[workload_data$workload_category == "High"]
  medium_workload <- workload_data$n[workload_data$workload_category == "Medium"] 
  low_workload <- workload_data$n[workload_data$workload_category == "Low"]
  
  prompt <- paste0(
    "Analyze teacher workload distribution and capacity management:\n\n",
    "WORKLOAD DISTRIBUTION:\n",
    "• High Workload Teachers: ", ifelse(length(high_workload) > 0, high_workload, 0), "\n",
    "• Medium Workload Teachers: ", ifelse(length(medium_workload) > 0, medium_workload, 0), "\n",
    "• Low Workload Teachers: ", ifelse(length(low_workload) > 0, low_workload, 0), "\n",
    "• Average Sessions per Teacher: ", round(teachers_summary$engagement_metrics$avg_sessions_per_teacher, 1), "\n",
    "• Total Active Teaching Faculty: ", teachers_summary$active_teachers, "\n",
    "• Subject Specializations: ", nrow(teachers_summary$subject_dist), " areas\n\n",
    "Provide detailed analysis (200-260 words) addressing:\n",
    "1. Current workload balance and distribution patterns\n",
    "2. Identification of overburdened vs underutilized teachers\n",
    "3. Capacity optimization opportunities and resource reallocation\n",
    "4. Impact of workload imbalance on teaching quality\n",
    "5. Sustainable teaching load recommendations\n",
    "6. Strategies for better workload management and faculty support\n\n",
    "Focus on workload equity and sustainable teaching practices:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 9))
}

analyze_learning_competency_coverage <- function(teachers_summary) {
  if (is.null(teachers_summary$session_analytics) || 
      is.null(teachers_summary$session_analytics$competency_dist)) {
    return("No learning competency data available for analysis.")
  }
  
  competency_data <- teachers_summary$session_analytics$competency_dist
  total_competencies <- nrow(competency_data)
  
  prompt <- paste0(
    "Analyze learning competency coverage and curriculum delivery patterns:\n\n",
    "COMPETENCY METRICS:\n",
    "• Total Learning Competencies Covered: ", total_competencies, "\n",
    "• Subject Areas with Sessions: ", nrow(teachers_summary$session_analytics$session_subject_dist), "\n",
    "• Grade Levels with Active Sessions: ", nrow(teachers_summary$session_analytics$session_grade_dist), "\n",
    "• Total Teaching Sessions: ", teachers_summary$session_analytics$total_sessions, "\n",
    "• Teachers Contributing to Competency Coverage: ", nrow(teachers_summary$session_analytics$teacher_performance), "\n\n",
    "TOP COVERED COMPETENCIES:\n",
    paste(head(competency_data$learning_competency, 5), collapse = "; "), "\n\n",
    "Provide comprehensive analysis (200-260 words) covering:\n",
    "1. Breadth and depth of learning competency coverage\n",
    "2. Most emphasized vs least covered competency areas\n",
    "3. Curriculum delivery consistency and gaps\n",
    "4. Teacher specialization in specific competency areas\n",
    "5. Alignment with educational standards and requirements\n",
    "6. Recommendations for improved competency coverage\n\n",
    "Focus on curriculum effectiveness and educational quality assurance:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 9))
}

analyze_teacher_engagement_patterns <- function(teachers_summary) {
  engagement_data <- teachers_summary$engagement_metrics
  
  # Calculate engagement rate
  engagement_rate <- if (teachers_summary$active_teachers > 0 && !is.null(teachers_summary$session_analytics)) {
    (nrow(teachers_summary$session_analytics$teacher_performance) / teachers_summary$active_teachers) * 100
  } else {
    0
  }
  
  prompt <- paste0(
    "Analyze teacher engagement patterns and professional participation:\n\n",
    "ENGAGEMENT METRICS:\n",
    "• Active Teachers: ", teachers_summary$active_teachers, "\n",
    "• Teachers with Teaching Sessions: ", ifelse(!is.null(teachers_summary$session_analytics), nrow(teachers_summary$session_analytics$teacher_performance), 0), "\n",
    "• Engagement Rate: ", round(engagement_rate, 1), "% of active teachers\n",
    "• Recently Active Teachers (30 days): ", ifelse(is.na(engagement_data$recent_active_teachers), "Data not available", engagement_data$recent_active_teachers), "\n",
    "• Average Sessions per Engaged Teacher: ", round(engagement_data$avg_sessions_per_teacher, 1), "\n",
    "• Teachers on Leave/Inactive: ", teachers_summary$inactive_teachers + teachers_summary$on_leave_teachers, "\n\n",
    "Provide detailed analysis (200-260 words) addressing:\n",
    "1. Overall teacher engagement levels and participation rates\n",
    "2. Patterns of active vs inactive teaching faculty\n",
    "3. Professional commitment indicators and consistency\n",
    "4. Factors influencing teacher engagement and participation\n",
    "5. Impact of engagement on educational quality and outcomes\n",
    "6. Strategies to improve teacher engagement and retention\n\n",
    "Focus on building a more engaged and committed teaching faculty:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 9))
}

generate_teachers_recommendations <- function(teachers_summary) {
  # Prepare key metrics for recommendations
  engagement_rate <- if (teachers_summary$active_teachers > 0 && !is.null(teachers_summary$session_analytics)) {
    (nrow(teachers_summary$session_analytics$teacher_performance) / teachers_summary$active_teachers) * 100
  } else {
    0
  }
  
  total_hours <- if (!is.null(teachers_summary$session_analytics)) {
    teachers_summary$session_analytics$total_hours
  } else {
    0
  }
  
  prompt <- paste0(
    "Based on this teacher performance and capacity analysis, provide 6-8 specific, actionable recommendations:\n\n",
    "FACULTY PERFORMANCE INDICATORS:\n",
    "• Total Teaching Faculty: ", teachers_summary$total_teachers, "\n",
    "• Active Teachers: ", teachers_summary$active_teachers, "\n",
    "• Teacher Engagement Rate: ", round(engagement_rate, 1), "%\n",
    "• Total Teaching Hours: ", total_hours, "\n",
    "• Subject Areas Covered: ", nrow(teachers_summary$subject_dist), "\n",
    "• Grade Levels Served: ", nrow(teachers_summary$grade_dist), "\n",
    "• Inactive/On Leave Teachers: ", teachers_summary$inactive_teachers + teachers_summary$on_leave_teachers, "\n\n",
    "Generate numbered recommendations (280-350 words total) focusing on:\n",
    "- Strategies to improve teacher engagement and participation\n",
    "- Workload balancing and capacity optimization approaches\n",
    "- Professional development and skill enhancement programs\n",
    "- Performance recognition and motivation systems\n",
    "- Resource allocation and support improvements\n",
    "- Faculty retention and recruitment strategies\n",
    "- Technology integration for enhanced teaching effectiveness\n",
    "- Long-term faculty development and succession planning\n\n",
    "Format as numbered list with brief explanations. Be specific and actionable for educational leadership:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 10))
}

# =============================================================================
# 3. TEACHERS WORD DOCUMENT GENERATION
# =============================================================================

generate_teachers_word_report <- function(teachers_summary, ai_sections, output_path, app_title = "Project ARAL Management System") {
  tryCatch({
    # Load required library
    if (!requireNamespace("officer", quietly = TRUE)) {
      stop("officer package is required for Word document generation")
    }
    if (!requireNamespace("flextable", quietly = TRUE)) {
      stop("flextable package is required for table formatting")
    }
    
    library(officer)
    library(flextable)
    
    doc <- read_docx()
    
    # Header
    doc <- doc %>%
      body_add_par("Teacher Performance & Capacity Analysis Report", style = "heading 1") %>%
      body_add_par("Comprehensive Assessment of Faculty Engagement and Educational Effectiveness", style = "heading 2") %>%
      body_add_par(paste("Generated on:", format(Sys.Date(), "%B %d, %Y")), style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # Executive Summary
    doc <- doc %>%
      body_add_par("EXECUTIVE SUMMARY", style = "heading 2") %>%
      body_add_par(ai_sections$executive_summary, style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # Key Teacher Metrics
    doc <- add_teacher_metrics_section(doc, teachers_summary)
    
    # Performance Analysis
    doc <- doc %>%
      body_add_par("TEACHER PERFORMANCE ANALYSIS", style = "heading 2") %>%
      body_add_par(ai_sections$performance_analysis, style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # Add performance breakdown table
    doc <- add_performance_breakdown_section(doc, teachers_summary)
    
    # Workload Distribution
    doc <- doc %>%
      body_add_par("WORKLOAD DISTRIBUTION ANALYSIS", style = "heading 2") %>%
      body_add_par(ai_sections$workload_distribution, style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # Workload analysis table
    doc <- add_workload_analysis_section(doc, teachers_summary)
    
    # Competency Analysis
    doc <- doc %>%
      body_add_par("LEARNING COMPETENCY COVERAGE", style = "heading 2") %>%
      body_add_par(ai_sections$competency_analysis, style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # Competency coverage table
    doc <- add_competency_coverage_section(doc, teachers_summary)
    
    # Engagement Insights
    doc <- doc %>%
      body_add_par("TEACHER ENGAGEMENT INSIGHTS", style = "heading 2") %>%
      body_add_par(ai_sections$engagement_insights, style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # Strategic Recommendations
    doc <- doc %>%
      body_add_par("STRATEGIC RECOMMENDATIONS", style = "heading 2") %>%
      body_add_par(ai_sections$recommendations, style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # Detailed Faculty Directory
    doc <- add_detailed_faculty_directory(doc, teachers_summary)
    
    # Footer
    doc <- add_teacher_report_footer(doc, app_title)
    
    # Save
    print(doc, target = output_path)
    message("✓ Teachers Word document successfully generated at: ", output_path)
    return(TRUE)
    
  }, error = function(e) {
    message("❌ Error in generate_teachers_word_report: ", e$message)
    print(traceback())
    stop(e)
  })
}

# Helper functions for Word document sections
add_teacher_metrics_section <- function(doc, teachers_summary) {
  doc <- doc %>%
    body_add_par("KEY TEACHER METRICS", style = "heading 2")
  
  total_hours <- if (!is.null(teachers_summary$session_analytics)) {
    teachers_summary$session_analytics$total_hours
  } else {
    0
  }
  
  total_sessions <- if (!is.null(teachers_summary$session_analytics)) {
    teachers_summary$session_analytics$total_sessions
  } else {
    0
  }
  
  avg_hours_per_teacher <- if (teachers_summary$active_teachers > 0 && total_hours > 0) {
    round(total_hours / teachers_summary$active_teachers, 1)
  } else {
    0
  }
  
  engagement_rate <- if (teachers_summary$active_teachers > 0 && !is.null(teachers_summary$session_analytics)) {
    round((nrow(teachers_summary$session_analytics$teacher_performance) / teachers_summary$active_teachers) * 100, 1)
  } else {
    0
  }
  
  metrics_data <- data.frame(
    Metric = c("Total Teachers", "Active Teachers", "Inactive/On Leave", 
               "Total Teaching Sessions", "Total Teaching Hours", "Average Hours per Teacher",
               "Teacher Engagement Rate", "Subject Areas Covered"),
    Value = c(
      as.character(teachers_summary$total_teachers),
      as.character(teachers_summary$active_teachers),
      as.character(teachers_summary$inactive_teachers + teachers_summary$on_leave_teachers),
      as.character(total_sessions),
      paste0(total_hours, " hours"),
      paste0(avg_hours_per_teacher, " hours"),
      paste0(engagement_rate, "% actively teaching"),
      paste0(nrow(teachers_summary$subject_dist), " subjects")
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

add_performance_breakdown_section <- function(doc, teachers_summary) {
  if (!is.null(teachers_summary$session_analytics) && 
      nrow(teachers_summary$session_analytics$top_performers) > 0) {
    doc <- doc %>%
      body_add_par("Top Performing Teachers", style = "heading 3")
    
    performance_data <- teachers_summary$session_analytics$top_performers %>%
      head(10) %>%
      mutate(
        Teacher_ID = teacher_id,
        Total_Sessions = total_sessions,
        Total_Hours = paste0(round(total_hours, 1), " hrs"),
        Avg_Duration = paste0(round(avg_duration, 1), " hrs"),
        Subjects_Taught = subjects_taught,
        Competencies = competencies_covered
      ) %>%
      select(Teacher_ID, Total_Sessions, Total_Hours, Avg_Duration, Subjects_Taught, Competencies)
    
    performance_table <- flextable(performance_data) %>%
      theme_booktabs() %>%
      autofit() %>%
      bg(part = "header", bg = "#2ecc71") %>%
      color(part = "header", color = "white") %>%
      bold(part = "header")
    
    doc <- doc %>%
      body_add_flextable(performance_table) %>%
      body_add_par("", style = "Normal")
  }
  
  return(doc)
}

add_workload_analysis_section <- function(doc, teachers_summary) {
  if (!is.null(teachers_summary$session_analytics) && 
      nrow(teachers_summary$session_analytics$workload_analysis) > 0) {
    doc <- doc %>%
      body_add_par("Workload Distribution", style = "heading 3")
    
    workload_data <- teachers_summary$session_analytics$workload_analysis %>%
      mutate(
        Workload_Category = workload_category,
        Number_of_Teachers = n,
        Percentage = paste0(round((n / sum(n)) * 100, 1), "%")
      ) %>%
      select(Workload_Category, Number_of_Teachers, Percentage)
    
    workload_table <- flextable(workload_data) %>%
      theme_booktabs() %>%
      autofit() %>%
      bg(part = "header", bg = "#e67e22") %>%
      color(part = "header", color = "white") %>%
      bold(part = "header")
    
    doc <- doc %>%
      body_add_flextable(workload_table) %>%
      body_add_par("", style = "Normal")
  }
  
  return(doc)
}

add_competency_coverage_section <- function(doc, teachers_summary) {
  if (!is.null(teachers_summary$session_analytics) && 
      !is.null(teachers_summary$session_analytics$competency_dist) &&
      nrow(teachers_summary$session_analytics$competency_dist) > 0) {
    doc <- doc %>%
      body_add_par("Top Learning Competencies Covered", style = "heading 3")
    
    competency_data <- teachers_summary$session_analytics$competency_dist %>%
      head(15) %>%
      mutate(
        Learning_Competency = learning_competency,
        Sessions_Count = n,
        Coverage_Frequency = paste0(round((n / sum(n)) * 100, 1), "%")
      ) %>%
      select(Learning_Competency, Sessions_Count, Coverage_Frequency)
    
    competency_table <- flextable(competency_data) %>%
      theme_booktabs() %>%
      autofit() %>%
      bg(part = "header", bg = "#9b59b6") %>%
      color(part = "header", color = "white") %>%
      bold(part = "header")
    
    doc <- doc %>%
      body_add_flextable(competency_table) %>%
      body_add_par("", style = "Normal")
  }
  
  return(doc)
}

add_detailed_faculty_directory <- function(doc, teachers_summary) {
  doc <- doc %>%
    body_add_par("DETAILED FACULTY DIRECTORY", style = "heading 2") %>%
    body_add_par("Complete listing of all teaching faculty organized by status.", style = "Normal") %>%
    body_add_par("", style = "Normal")
  
  if (nrow(teachers_summary$data) > 0) {
    # Group by status
    statuses <- c("Active", "Inactive", "On Leave")
    
    for (status in statuses) {
      status_teachers <- teachers_summary$data %>%
        filter(teacher_status == !!status) %>%
        arrange(name)
      
      if (nrow(status_teachers) > 0) {
        doc <- doc %>%
          body_add_par(paste(status, "Teachers"), style = "heading 3")
        
        status_data <- status_teachers %>%
          select(
            Teacher_Name = name,
            Teacher_ID = teacherID,
            Subject = subject_handled,
            Year_Level = year_level,
            Status = teacher_status
          ) %>%
          mutate(
            Teacher_Name = ifelse(is.na(Teacher_Name), "N/A", Teacher_Name),
            Teacher_ID = ifelse(is.na(Teacher_ID), "N/A", Teacher_ID),
            Subject = ifelse(is.na(Subject) | Subject == "", "N/A", Subject),
            Year_Level = ifelse(is.na(Year_Level), "N/A", as.character(Year_Level))
          )
        
        status_table <- flextable(status_data) %>%
          theme_booktabs() %>%
          autofit() %>%
          bg(part = "header", bg = "#34495e") %>%
          color(part = "header", color = "white") %>%
          bold(part = "header")
        
        doc <- doc %>%
          body_add_flextable(status_table) %>%
          body_add_par("", style = "Normal")
      }
    }
  }
  
  return(doc)
}

add_teacher_report_footer <- function(doc, app_title) {
  doc <- doc %>%
    body_add_par("---", style = "Normal") %>%
    body_add_par("REPORT DETAILS", style = "heading 3") %>%
    body_add_par(paste("Report generated by:", app_title), style = "Normal") %>%
    body_add_par(paste("Generated on:", format(Sys.time(), "%B %d, %Y at %I:%M %p")), style = "Normal") %>%
    body_add_par("AI Analysis powered by Google Gemini", style = "Normal") %>%
    body_add_par("This teacher performance report provides comprehensive analysis of faculty engagement, workload distribution, competency coverage, and strategic recommendations for educational excellence.", style = "Normal")
  
  return(doc)
}

# =============================================================================
# 4. MAIN TEACHERS REPORT GENERATION HANDLER
# =============================================================================

handle_teachers_report_generation <- function(teachers_data, sessions_data = NULL, app_title = "Project ARAL Management System") {
  
  tryCatch({
    # Validate input data first
    if (is.null(teachers_data) || !is.data.frame(teachers_data)) {
      stop("Invalid teachers data: must be a data frame")
    }
    
    if (nrow(teachers_data) == 0) {
      stop("No teacher data available for analysis")
    }
    
    # Clean the teachers data before processing
    teachers_data_clean <- teachers_data %>%
      mutate(
        # Ensure required columns exist and are properly formatted
        name = ifelse(is.na(name), "", as.character(name)),
        teacherID = ifelse(is.na(teacherID), "", as.character(teacherID)),
        subject_handled = ifelse(is.na(subject_handled), "", as.character(subject_handled)),
        year_level = ifelse(is.na(year_level), 0, as.numeric(year_level)),
        teacher_status = ifelse(is.na(teacher_status), "Unknown", as.character(teacher_status))
      )
    
    # Clean sessions data if provided
    sessions_data_clean <- if (!is.null(sessions_data) && is.data.frame(sessions_data) && nrow(sessions_data) > 0) {
      sessions_data %>%
        mutate(
          teacher_id = ifelse(is.na(teacher_id), "", as.character(teacher_id)),
          subject = ifelse(is.na(subject), "", as.character(subject)),
          grade_level = ifelse(is.na(grade_level), 0, as.numeric(grade_level)),
          learning_competency = ifelse(is.na(learning_competency), "", as.character(learning_competency)),
          duration = ifelse(is.na(duration), 0, as.numeric(duration))
        ) %>%
        filter(duration > 0)  # Only include sessions with valid duration
    } else {
      NULL
    }
    
    message("Creating teachers dashboard summary...")
    summary_data <- create_teachers_dashboard_summary(teachers_data_clean, sessions_data_clean)
    
    # Validate summary data
    if (summary_data$total_teachers == 0) {
      stop("No valid teacher data found after cleaning")
    }
    
    message("Starting AI analysis...")
    # Generate AI analysis with better error handling
    ai_analysis <- teachers_gemini_analysis(summary_data)
    
    # Create temporary file
    temp_file <- tempfile(fileext = ".docx")
    
    message("Generating Word document...")
    # Generate Word document
    generate_teachers_word_report(summary_data, ai_analysis, temp_file, app_title)
    
    return(list(
      success = TRUE,
      file_path = temp_file,
      ai_analysis = ai_analysis,
      message = "Teachers report generated successfully!"
    ))
    
  }, error = function(e) {
    message("Error in handle_teachers_report_generation: ", e$message)
    return(list(
      success = FALSE,
      error = e$message,
      message = paste("Teachers report generation failed:", e$message)
    ))
  })
}