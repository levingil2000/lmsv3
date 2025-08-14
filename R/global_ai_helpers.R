# =============================================================================
# GLOBAL AI REPORT GENERATION SYSTEM
# =============================================================================
# This system provides a unified approach to generate AI-powered reports
# for different modules (Students, Teachers, Partners, Facilities)

# =============================================================================
# 1. CORE AI HELPER FUNCTIONS (Enhanced from your original)
# =============================================================================

# Safe Gemini API caller with retry logic
safe_gemini_call <- function(prompt, max_retries = 3, wait_time = 8) {
  for (attempt in 1:max_retries) {
    tryCatch({
      message(paste("Gemini API attempt", attempt, "- waiting", wait_time, "seconds..."))
      
      response <- gemini(prompt = prompt)
      Sys.sleep(wait_time)
      
      if (is.character(response) && length(response) > 0 && nchar(trimws(response)) > 20) {
        message(paste("✓ Gemini API call successful on attempt", attempt))
        return(trimws(response))
      } else {
        warning(paste("Invalid response on attempt", attempt, "- response:", toString(response)))
      }
      
    }, error = function(e) {
      warning(paste("Gemini API error on attempt", attempt, ":", e$message))
      
      if (attempt < max_retries) {
        wait_time_retry <- wait_time + (attempt * 2)
        message(paste("Retrying in", wait_time_retry, "seconds..."))
        Sys.sleep(wait_time_retry)
      }
    })
  }
  
  warning("All Gemini API attempts failed")
  return(NULL)
}

# =============================================================================
# 2. GLOBAL REPORT ORCHESTRATOR
# =============================================================================

# Global function to generate AI-powered reports for different modules
# 
# Args:
#   module_type: 'students', 'teachers', 'partners', 'facilities'
#   data: Module-specific data
#   additional_params: Additional parameters specific to each module
# 
# Returns:
#   List with success status, file path, and AI analysis
generate_ai_report <- function(module_type, data, additional_params = list()) {
  
  message(paste("Starting AI report generation for:", module_type))
  
  tryCatch({
    # Create module-specific summary
    summary_data <- switch(module_type,
                           "students" = create_student_summary(data),
                           "teachers" = create_teacher_summary(data, additional_params),
                           "partners" = create_partner_summary(data),
                           "facilities" = create_facility_summary(data),
                           stop("Unknown module type: ", module_type)
    )
    
    # Generate AI analysis
    ai_analysis <- switch(module_type,
                          "students" = analyze_student_data(summary_data),
                          "teachers" = analyze_teacher_data(summary_data),
                          "partners" = analyze_partner_data(summary_data),
                          "facilities" = analyze_facility_data(summary_data),
                          stop("Unknown module type for analysis: ", module_type)
    )
    
    # Generate Word document
    temp_file <- tempfile(fileext = ".docx")
    
    success <- switch(module_type,
                      "students" = generate_student_word_report(summary_data, ai_analysis, temp_file),
                      "teachers" = generate_teacher_word_report(summary_data, ai_analysis, temp_file),
                      "partners" = generate_partner_word_report(summary_data, ai_analysis, temp_file),
                      "facilities" = generate_facility_word_report(summary_data, ai_analysis, temp_file),
                      FALSE
    )
    
    if (success) {
      return(list(
        success = TRUE,
        file_path = temp_file,
        ai_analysis = ai_analysis,
        message = paste(tools::toTitleCase(module_type), "report generated successfully!")
      ))
    } else {
      stop("Word document generation failed")
    }
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = e$message,
      message = paste("Report generation failed:", e$message)
    ))
  })
}

# =============================================================================
# 3. MODULE-SPECIFIC SUMMARY CREATORS
# =============================================================================

# Student summary (your existing logic)
create_student_summary <- function(dashboard_data) {
  data <- dashboard_data
  
  grade_dist <- table(data$grade_level)
  subject_dist <- table(tools::toTitleCase(data$subject))
  
  avg_sessions_df <- data %>% 
    group_by(subject) %>% 
    summarise(avg = round(mean(total_attendance, na.rm = TRUE), 1), .groups = 'drop')
  
  avg_sessions <- avg_sessions_df$avg
  names(avg_sessions) <- tools::toTitleCase(avg_sessions_df$subject)
  
  list(
    module_type = "students",
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

# Teacher summary creator
create_teacher_summary <- function(teacher_data, additional_params = list()) {
  sessions_data <- additional_params$sessions_data
  
  # Calculate teacher statistics
  teacher_stats <- if (!is.null(sessions_data)) {
    sessions_data %>%
      group_by(teacher_id) %>%
      summarise(
        total_sessions = n(),
        total_hours = sum(duration, na.rm = TRUE),
        avg_duration = mean(duration, na.rm = TRUE),
        subjects_taught = n_distinct(subject, na.rm = TRUE),
        .groups = 'drop'
      )
  } else {
    data.frame()
  }
  
  # Subject distribution
  subject_dist <- if (!is.null(sessions_data)) {
    table(sessions_data$subject)
  } else {
    table(teacher_data$subject_handled)
  }
  
  # Grade distribution  
  grade_dist <- if (!is.null(sessions_data)) {
    table(sessions_data$grade_level)
  } else {
    table(teacher_data$year_level)
  }
  
  list(
    module_type = "teachers",
    total_teachers = nrow(teacher_data),
    active_teachers = sum(teacher_data$teacher_status == "Active", na.rm = TRUE),
    inactive_teachers = sum(teacher_data$teacher_status == "Inactive", na.rm = TRUE),
    on_leave_teachers = sum(teacher_data$teacher_status == "On Leave", na.rm = TRUE),
    subject_dist = subject_dist,
    grade_dist = grade_dist,
    teacher_stats = teacher_stats,
    sessions_data = sessions_data,
    data = teacher_data
  )
}

# Partner summary creator
create_partner_summary <- function(partner_data) {
  # Calculate donation statistics
  cash_donations <- sum(grepl("cash|money|₱|peso", tolower(partner_data$service_cash_fixture_provided)), na.rm = TRUE)
  service_donations <- sum(!grepl("cash|money|₱|peso", tolower(partner_data$service_cash_fixture_provided)) & 
                             !is.na(partner_data$service_cash_fixture_provided) &
                             partner_data$service_cash_fixture_provided != "", na.rm = TRUE)
  
  # Monthly trends
  monthly_trends <- partner_data %>%
    filter(!is.na(date_given) & !is.na(amount) & amount > 0) %>%
    mutate(year_month = format(as.Date(date_given), "%Y-%m")) %>%
    group_by(year_month) %>%
    summarise(
      total_amount = sum(amount, na.rm = TRUE),
      count = n(),
      .groups = 'drop'
    )
  
  # Top contributors
  top_partners <- partner_data %>%
    filter(!is.na(amount) & amount > 0) %>%
    arrange(desc(amount)) %>%
    head(10)
  
  list(
    module_type = "partners",
    total_partners = nrow(partner_data),
    total_amount = sum(partner_data$amount, na.rm = TRUE),
    cash_donations = cash_donations,
    service_donations = service_donations,
    avg_contribution = round(mean(partner_data$amount, na.rm = TRUE), 2),
    monthly_trends = monthly_trends,
    top_partners = top_partners,
    data = partner_data
  )
}

# Facility summary creator
create_facility_summary <- function(facility_data) {
  # Status distribution
  status_dist <- table(facility_data$status)
  
  # Type distribution
  type_dist <- table(facility_data$type)
  
  # Value statistics
  total_value <- sum(facility_data$amount, na.rm = TRUE)
  avg_value <- mean(facility_data$amount, na.rm = TRUE)
  
  # High-value facilities
  high_value <- facility_data %>%
    filter(!is.na(amount) & amount > 0) %>%
    arrange(desc(amount)) %>%
    head(10)
  
  list(
    module_type = "facilities",
    total_facilities = nrow(facility_data),
    available_facilities = sum(facility_data$status == "Available", na.rm = TRUE),
    in_use_facilities = sum(facility_data$status == "In Use", na.rm = TRUE),
    maintenance_facilities = sum(facility_data$status == "Under Maintenance", na.rm = TRUE),
    damaged_facilities = sum(facility_data$status == "Damaged", na.rm = TRUE),
    status_dist = status_dist,
    type_dist = type_dist,
    total_value = total_value,
    avg_value = avg_value,
    high_value = high_value,
    data = facility_data
  )
}

# =============================================================================
# 4. MODULE-SPECIFIC AI ANALYSIS FUNCTIONS
# =============================================================================

# Student analysis (your existing logic)
analyze_student_data <- function(summary_data) {
  results <- list(
    executive_summary = "Executive Summary: Analysis of the tutoring program data shows enrollment and performance patterns across multiple grade levels and subjects.",
    grade_analysis = "Grade level analysis shows distribution of students across different academic levels.",
    subject_analysis = "Subject analysis reveals enrollment patterns and engagement levels across different academic areas.",
    attendance_insights = "Attendance patterns provide insights into student engagement and program effectiveness.",
    recommendations = "1. Monitor student progress regularly\n2. Assess resource allocation\n3. Enhance student support programs\n4. Evaluate program effectiveness\n5. Implement targeted interventions"
  )
  
  # Generate each section (using your existing functions if they exist)
  tryCatch({
    if (exists("analyze_executive_summary")) {
      exec_result <- analyze_executive_summary(summary_data)
      if (!is.null(exec_result)) results$executive_summary <- exec_result
    }
  }, error = function(e) message("Executive summary generation failed: ", e$message))
  
  tryCatch({
    if (exists("analyze_grade_distribution")) {
      grade_result <- analyze_grade_distribution(summary_data$grade_dist)
      if (!is.null(grade_result)) results$grade_analysis <- grade_result
    }
  }, error = function(e) message("Grade analysis failed: ", e$message))
  
  tryCatch({
    if (exists("analyze_subject_performance")) {
      subject_result <- analyze_subject_performance(summary_data$subject_dist, summary_data$avg_sessions)
      if (!is.null(subject_result)) results$subject_analysis <- subject_result
    }
  }, error = function(e) message("Subject analysis failed: ", e$message))
  
  tryCatch({
    if (exists("analyze_attendance_patterns")) {
      attendance_result <- analyze_attendance_patterns(summary_data)
      if (!is.null(attendance_result)) results$attendance_insights <- attendance_result
    }
  }, error = function(e) message("Attendance analysis failed: ", e$message))
  
  tryCatch({
    if (exists("generate_recommendations")) {
      rec_result <- generate_recommendations(summary_data)
      if (!is.null(rec_result)) results$recommendations <- rec_result
    }
  }, error = function(e) message("Recommendations generation failed: ", e$message))
  
  return(results)
}

# Teacher analysis
analyze_teacher_data <- function(summary_data) {
  results <- list(
    executive_summary = "Executive Summary: Teacher performance analysis reveals teaching patterns, subject distribution, and resource utilization.",
    performance_analysis = "Teacher performance shows distribution of teaching hours and session engagement across the faculty.",
    subject_distribution = "Subject teaching distribution indicates coverage and specialization patterns.",
    workload_insights = "Workload analysis provides insights into teacher utilization and capacity.",
    recommendations = "1. Balance teaching loads\n2. Optimize subject assignments\n3. Enhance professional development\n4. Monitor teacher wellbeing\n5. Improve resource allocation"
  )
  
  # Executive Summary
  tryCatch({
    total_hours <- sum(summary_data$teacher_stats$total_hours, na.rm = TRUE)
    avg_hours_per_teacher <- round(total_hours / summary_data$active_teachers, 1)
    
    prompt <- paste0(
      "As an educational administrator, write a comprehensive executive summary for a teacher performance report.\n\n",
      "TEACHER DATA:\n",
      "• Total Teachers: ", summary_data$total_teachers, "\n",
      "• Active Teachers: ", summary_data$active_teachers, "\n",
      "• Total Teaching Hours: ", total_hours, "\n",
      "• Average Hours per Teacher: ", avg_hours_per_teacher, "\n\n",
      "Write a 3-4 paragraph executive summary focusing on:\n",
      "- Overview of teaching capacity and utilization\n",
      "- Key performance indicators and distribution patterns\n",
      "- Teacher engagement and workload balance\n",
      "- Overall faculty effectiveness assessment\n",
      "Keep between 250-350 words:"
    )
    
    exec_result <- safe_gemini_call(prompt, max_retries = 3, wait_time = 10)
    if (!is.null(exec_result)) results$executive_summary <- exec_result
  }, error = function(e) message("Teacher executive summary failed: ", e$message))
  
  # Performance Analysis
  tryCatch({
    if (nrow(summary_data$teacher_stats) > 0) {
      top_performers <- summary_data$teacher_stats %>% arrange(desc(total_hours)) %>% head(5)
      
      prompt <- paste0(
        "Analyze teacher performance distribution:\n\n",
        "PERFORMANCE METRICS:\n",
        "• Total Sessions Conducted: ", sum(summary_data$teacher_stats$total_sessions, na.rm = TRUE), "\n",
        "• Average Session Duration: ", round(mean(summary_data$teacher_stats$avg_duration, na.rm = TRUE), 1), " hours\n",
        "• Teachers with High Engagement: ", sum(summary_data$teacher_stats$total_hours > mean(summary_data$teacher_stats$total_hours, na.rm = TRUE), na.rm = TRUE), "\n\n",
        "Provide analysis (200-250 words) covering:\n",
        "1. Distribution of teaching hours and session patterns\n",
        "2. Identification of high-performing teachers\n",
        "3. Workload balance across the faculty\n",
        "4. Session quality and duration patterns\n",
        "5. Recommendations for performance optimization"
      )
      
      perf_result <- safe_gemini_call(prompt, max_retries = 3, wait_time = 9)
      if (!is.null(perf_result)) results$performance_analysis <- perf_result
    }
  }, error = function(e) message("Teacher performance analysis failed: ", e$message))
  
  # Subject Distribution Analysis
  tryCatch({
    if (length(summary_data$subject_dist) > 0) {
      subject_text <- paste(paste(names(summary_data$subject_dist), ":", summary_data$subject_dist, "sessions"), collapse = "; ")
      
      prompt <- paste0(
        "Analyze subject teaching distribution:\n\n",
        "SUBJECT COVERAGE: ", subject_text, "\n\n",
        "Provide analysis (150-200 words) addressing:\n",
        "1. Which subjects receive the most teaching attention\n",
        "2. Potential gaps in subject coverage\n",
        "3. Teacher specialization patterns\n",
        "4. Balance of core vs supplementary subjects\n",
        "5. Recommendations for improved subject distribution"
      )
      
      subject_result <- safe_gemini_call(prompt, max_retries = 3, wait_time = 8)
      if (!is.null(subject_result)) results$subject_distribution <- subject_result
    }
  }, error = function(e) message("Subject distribution analysis failed: ", e$message))
  
  return(results)
}

# Partner analysis
analyze_partner_data <- function(summary_data) {
  results <- list(
    executive_summary = "Executive Summary: Partnership analysis reveals contribution patterns, donor engagement, and resource development trends.",
    contribution_analysis = "Contribution analysis shows donation patterns and partner engagement levels.",
    partnership_trends = "Partnership trends indicate growth patterns and sustainability indicators.",
    donor_insights = "Donor insights provide understanding of contribution types and partnership dynamics.",
    recommendations = "1. Diversify funding sources\n2. Strengthen partner relationships\n3. Improve recognition programs\n4. Enhance stewardship practices\n5. Develop sustainable partnerships"
  )
  
  # Generate AI analysis for partners
  tryCatch({
    prompt <- paste0(
      "As a partnership development specialist, write an executive summary for a donor/partner report.\n\n",
      "PARTNERSHIP DATA:\n",
      "• Total Partners: ", summary_data$total_partners, "\n",
      "• Total Contributions: ₱", format(summary_data$total_amount, big.mark = ","), "\n",
      "• Cash Donations: ", summary_data$cash_donations, "\n",
      "• Service/Fixture Donations: ", summary_data$service_donations, "\n",
      "• Average Contribution: ₱", format(summary_data$avg_contribution, big.mark = ","), "\n\n",
      "Write a 3-4 paragraph executive summary focusing on:\n",
      "- Overview of partnership program and total impact\n",
      "- Diversity of contribution types and partner engagement\n",
      "- Trends in donor behavior and giving patterns\n",
      "- Strategic value and sustainability of partnerships\n",
      "Keep between 250-350 words:"
    )
    
    exec_result <- safe_gemini_call(prompt, max_retries = 3, wait_time = 10)
    if (!is.null(exec_result)) results$executive_summary <- exec_result
  }, error = function(e) message("Partner executive summary failed: ", e$message))
  
  return(results)
}

# Facility analysis
analyze_facility_data <- function(summary_data) {
  results <- list(
    executive_summary = "Executive Summary: Facility management analysis reveals asset utilization, maintenance needs, and resource optimization opportunities.",
    utilization_analysis = "Facility utilization shows patterns of usage, availability, and operational efficiency.",
    maintenance_insights = "Maintenance analysis indicates facility condition and upkeep requirements.",
    asset_valuation = "Asset valuation provides understanding of facility value and investment priorities.",
    recommendations = "1. Optimize facility utilization\n2. Implement preventive maintenance\n3. Upgrade critical assets\n4. Monitor facility conditions\n5. Plan strategic investments"
  )
  
  # Generate AI analysis for facilities
  tryCatch({
    utilization_rate <- round((summary_data$in_use_facilities / summary_data$total_facilities) * 100, 1)
    
    prompt <- paste0(
      "As a facility manager, write an executive summary for a facilities management report.\n\n",
      "FACILITY DATA:\n",
      "• Total Facilities: ", summary_data$total_facilities, "\n",
      "• Available: ", summary_data$available_facilities, "\n",
      "• In Use: ", summary_data$in_use_facilities, " (", utilization_rate, "% utilization)\n",
      "• Under Maintenance: ", summary_data$maintenance_facilities, "\n",
      "• Damaged: ", summary_data$damaged_facilities, "\n",
      "• Total Asset Value: ₱", format(summary_data$total_value, big.mark = ","), "\n\n",
      "Write a 3-4 paragraph executive summary focusing on:\n",
      "- Overview of facility inventory and current status\n",
      "- Utilization rates and operational efficiency\n",
      "- Maintenance needs and asset condition\n",
      "- Strategic recommendations for facility optimization\n",
      "Keep between 250-350 words:"
    )
    
    exec_result <- safe_gemini_call(prompt, max_retries = 3, wait_time = 10)
    if (!is.null(exec_result)) results$executive_summary <- exec_result
  }, error = function(e) message("Facility executive summary failed: ", e$message))
  
  return(results)
}

# =============================================================================
# 5. WORD DOCUMENT GENERATORS (Module-specific)
# =============================================================================

# Student Word report (uses your existing function)
generate_student_word_report <- function(summary_data, ai_sections, output_path) {
  # Use your existing generate_word_report function if it exists
  tryCatch({
    if (exists("generate_word_report")) {
      generate_word_report(summary_data, ai_sections, output_path, "Project ARAL Management System")
      return(TRUE)
    } else {
      # Basic fallback implementation
      message("generate_word_report function not found, using basic implementation")
      return(FALSE)
    }
  }, error = function(e) {
    message("Error in student word report: ", e$message)
    return(FALSE)
  })
}

# Teacher Word report
generate_teacher_word_report <- function(summary_data, ai_sections, output_path) {
  # This function would need officer package loaded
  if (!requireNamespace("officer", quietly = TRUE)) {
    message("officer package required for Word document generation")
    return(FALSE)
  }
  
  tryCatch({
    doc <- officer::read_docx()
    
    # Header
    doc <- doc %>%
      officer::body_add_par("Teacher Performance & Analytics Report", style = "heading 1") %>%
      officer::body_add_par(paste("Generated on:", format(Sys.Date(), "%B %d, %Y")), style = "Normal") %>%
      officer::body_add_par("", style = "Normal")
    
    # Executive Summary
    doc <- doc %>%
      officer::body_add_par("EXECUTIVE SUMMARY", style = "heading 2") %>%
      officer::body_add_par(ai_sections$executive_summary, style = "Normal") %>%
      officer::body_add_par("", style = "Normal")
    
    # Save
    print(doc, target = output_path)
    message("✓ Teacher Word document successfully generated")
    return(TRUE)
    
  }, error = function(e) {
    message("❌ Error in teacher word report: ", e$message)
    return(FALSE)
  })
}

# Partner Word report
generate_partner_word_report <- function(summary_data, ai_sections, output_path) {
  # This function would need officer package loaded
  if (!requireNamespace("officer", quietly = TRUE)) {
    message("officer package required for Word document generation")
    return(FALSE)
  }
  
  tryCatch({
    doc <- officer::read_docx()
    
    # Header
    doc <- doc %>%
      officer::body_add_par("Partnership & Donor Relations Report", style = "heading 1") %>%
      officer::body_add_par(paste("Generated on:", format(Sys.Date(), "%B %d, %Y")), style = "Normal") %>%
      officer::body_add_par("", style = "Normal")
    
    # Executive Summary
    doc <- doc %>%
      officer::body_add_par("EXECUTIVE SUMMARY", style = "heading 2") %>%
      officer::body_add_par(ai_sections$executive_summary, style = "Normal")
    
    # Save
    print(doc, target = output_path)
    message("✓ Partner Word document successfully generated")
    return(TRUE)
    
  }, error = function(e) {
    message("❌ Error in partner word report: ", e$message)
    return(FALSE)
  })
}

# Facility Word report
generate_facility_word_report <- function(summary_data, ai_sections, output_path) {
  # This function would need officer package loaded
  if (!requireNamespace("officer", quietly = TRUE)) {
    message("officer package required for Word document generation")
    return(FALSE)
  }
  
  tryCatch({
    doc <- officer::read_docx()
    
    # Header
    doc <- doc %>%
      officer::body_add_par("Facilities Management Report", style = "heading 1") %>%
      officer::body_add_par(paste("Generated on:", format(Sys.Date(), "%B %d, %Y")), style = "Normal") %>%
      officer::body_add_par("", style = "Normal")
    
    # Executive Summary
    doc <- doc %>%
      officer::body_add_par("EXECUTIVE SUMMARY", style = "heading 2") %>%
      officer::body_add_par(ai_sections$executive_summary, style = "Normal")
    
    # Save
    print(doc, target = output_path)
    message("✓ Facility Word document successfully generated")
    return(TRUE)
    
  }, error = function(e) {
    message("❌ Error in facility word report: ", e$message)
    return(FALSE)
  })
}

# =============================================================================
# 7. INTEGRATION FUNCTIONS FOR EXISTING MODULES
# =============================================================================

# Function to integrate with your existing student module
handle_student_report_generation <- function(dashboard_data, app_title = "Project ARAL Management System") {
  return(generate_ai_report("students", dashboard_data))
}

# Functions for your other modules
handle_teacher_report_generation <- function(teacher_data, sessions_data = NULL) {
  additional_params <- list(sessions_data = sessions_data)
  return(generate_ai_report("teachers", teacher_data, additional_params))
}

handle_partner_report_generation <- function(partner_data) {
  return(generate_ai_report("partners", partner_data))
}

handle_facility_report_generation <- function(facility_data) {
  return(generate_ai_report("facilities", facility_data))
}