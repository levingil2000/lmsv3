#Ai helper functions

# Safe Gemini API caller with retry logic and proper wait times
safe_gemini_call <- function(prompt, max_retries = 3, wait_time = 8) {
  
  for (attempt in 1:max_retries) {
    tryCatch({
      # Add more wait time for complex analysis
      message(paste("Gemini API attempt", attempt, "- waiting", wait_time, "seconds..."))
      
      # Make the API call with explicit model specification
      response <- gemini(
        prompt = prompt  # Try the experimental model first
      )
      
      # Wait for response processing
      Sys.sleep(wait_time)
      
      # Validate response
      if (is.character(response) && length(response) > 0 && nchar(trimws(response)) > 20) {
        message(paste("✓ Gemini API call successful on attempt", attempt))
        return(trimws(response))
      } else {
        warning(paste("Invalid response on attempt", attempt, "- response:", toString(response)))
      }
      
    }, error = function(e) {
      warning(paste("Gemini API error on attempt", attempt, ":", e$message))
      
      # If it's a 404 error, try different model
      if (grepl("404", e$message, ignore.case = TRUE)) {
        tryCatch({
          message("Trying gemini-1.5-flash model due to 404 error...")
          response <- gemini(
            prompt = prompt
          )
          Sys.sleep(wait_time)
          
          if (is.character(response) && length(response) > 0 && nchar(trimws(response)) > 20) {
            message("✓ Gemini API call successful with fallback model")
            return(trimws(response))
          }
        }, error = function(e2) {
          warning(paste("Fallback model also failed:", e2$message))
        })
      }
      
      if (attempt < max_retries) {
        wait_time_retry <- wait_time + (attempt * 2)  # Increase wait time with each retry
        message(paste("Retrying in", wait_time_retry, "seconds..."))
        Sys.sleep(wait_time_retry)
      }
    })
  }
  
  # If all attempts failed, return NULL
  warning("All Gemini API attempts failed")
  return(NULL)
}

# Main AI analysis orchestrator function with enhanced error handling
gemini_analysis <- function(dashboard_summary) {
  
  message("Starting comprehensive AI analysis...")
  
  # Initialize results with fallback content
  results <- list(
    executive_summary = "Executive Summary: Analysis of the tutoring program data shows enrollment and performance patterns across multiple grade levels and subjects.",
    grade_analysis = "Grade level analysis shows distribution of students across different academic levels.",
    subject_analysis = "Subject analysis reveals enrollment patterns and engagement levels across different academic areas.",
    attendance_insights = "Attendance patterns provide insights into student engagement and program effectiveness.",
    recommendations = "1. Monitor student progress regularly\n2. Assess resource allocation\n3. Enhance student support programs\n4. Evaluate program effectiveness\n5. Implement targeted interventions"
  )
  
  # Generate each section with proper error handling
  tryCatch({
    message("Generating executive summary...")
    exec_result <- analyze_executive_summary(dashboard_summary)
    if (!is.null(exec_result)) results$executive_summary <- exec_result
  }, error = function(e) {
    message("Executive summary generation failed: ", e$message)
  })
  
  tryCatch({
    message("Analyzing grade distribution...")
    grade_result <- analyze_grade_distribution(dashboard_summary$grade_dist)
    if (!is.null(grade_result)) results$grade_analysis <- grade_result
  }, error = function(e) {
    message("Grade analysis failed: ", e$message)
  })
  
  tryCatch({
    message("Analyzing subject performance...")
    subject_result <- analyze_subject_performance(dashboard_summary$subject_dist, dashboard_summary$avg_sessions)
    if (!is.null(subject_result)) results$subject_analysis <- subject_result
  }, error = function(e) {
    message("Subject analysis failed: ", e$message)
  })
  
  tryCatch({
    message("Analyzing attendance patterns...")
    attendance_result <- analyze_attendance_patterns(dashboard_summary)
    if (!is.null(attendance_result)) results$attendance_insights <- attendance_result
  }, error = function(e) {
    message("Attendance analysis failed: ", e$message)
  })
  
  tryCatch({
    message("Generating recommendations...")
    rec_result <- generate_recommendations(dashboard_summary)
    if (!is.null(rec_result)) results$recommendations <- rec_result
  }, error = function(e) {
    message("Recommendations generation failed: ", e$message)
  })
  
  message("AI analysis completed!")
  return(results)
}

# Enhanced analysis functions with better prompts
analyze_executive_summary <- function(dashboard_summary) {
  prompt <- paste0(
    "As an educational data analyst, write a comprehensive 3-4 paragraph executive summary for a tutoring program report.\n\n",
    "PROGRAM DATA:\n",
    "• Total Students: ", dashboard_summary$total_enrolled, "\n",
    "• Active Students: ", dashboard_summary$active_students, " (", 
    round((dashboard_summary$active_students/dashboard_summary$total_enrolled)*100, 1), "%)\n",
    "• Students Who Passed: ", dashboard_summary$passed_students, " (", 
    round((dashboard_summary$passed_students/dashboard_summary$total_enrolled)*100, 1), "%)\n",
    "• Average Session Attendance: ", dashboard_summary$avg_attendance, " sessions per student\n\n",
    "REQUIREMENTS:\n",
    "- Start with an overview of the program's scope and current status\n",
    "- Highlight key performance indicators and success rates\n",
    "- Discuss student engagement and retention patterns\n",
    "- Conclude with overall program effectiveness assessment\n",
    "- Use professional, data-driven language suitable for stakeholders\n",
    "- Keep between 200-300 words\n\n",
    "Write the executive summary now:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 10))
}

analyze_grade_distribution <- function(grade_dist) {
  if (length(grade_dist) == 0) return("No grade distribution data available.")
  
  grade_text <- paste(paste("Grade", names(grade_dist), ":", grade_dist, "students"), collapse = "; ")
  total_students <- sum(grade_dist)
  
  prompt <- paste0(
    "Analyze this grade-level enrollment distribution for a tutoring program:\n\n",
    "ENROLLMENT DATA: ", grade_text, "\n",
    "TOTAL STUDENTS: ", total_students, "\n\n",
    "Provide a detailed analysis (150-200 words) covering:\n",
    "1. Which grade levels show the highest and lowest enrollment numbers\n",
    "2. What enrollment patterns suggest about academic support needs\n",
    "3. Potential reasons for distribution patterns\n",
    "4. Implications for staffing and resource allocation\n",
    "5. Recommendations for addressing any imbalances\n\n",
    "Use specific numbers from the data and provide actionable insights:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 8))
}

analyze_subject_performance <- function(subject_dist, avg_sessions) {
  if (length(subject_dist) == 0) return("No subject distribution data available.")
  
  subject_text <- paste(paste(names(subject_dist), ":", subject_dist, "students"), collapse = "; ")
  
  if (length(avg_sessions) > 0) {
    sessions_text <- paste(paste(names(avg_sessions), ":", round(avg_sessions, 1), "avg sessions"), collapse = "; ")
  } else {
    sessions_text <- "No session data available"
  }
  
  prompt <- paste0(
    "Analyze subject enrollment and attendance patterns for this tutoring program:\n\n",
    "SUBJECT ENROLLMENT: ", subject_text, "\n",
    "AVERAGE SESSION ATTENDANCE: ", sessions_text, "\n\n",
    "Provide comprehensive analysis (200-250 words) addressing:\n",
    "1. Which subjects have the highest demand and why this might be\n",
    "2. Correlation between subject enrollment and average attendance\n",
    "3. Subjects showing strong vs. weak student engagement\n",
    "4. What these patterns reveal about student academic needs\n",
    "5. Strategic recommendations for program improvement\n\n",
    "Include specific data points and provide educational insights:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 9))
}

analyze_attendance_patterns <- function(dashboard_summary) {
  pass_rate <- round((dashboard_summary$passed_students / dashboard_summary$total_enrolled) * 100, 1)
  active_rate <- round((dashboard_summary$active_students / dashboard_summary$total_enrolled) * 100, 1)
  completion_rate <- round(((dashboard_summary$passed_students + dashboard_summary$active_students) / dashboard_summary$total_enrolled) * 100, 1)
  
  prompt <- paste0(
    "Analyze attendance and completion patterns for this tutoring program:\n\n",
    "PROGRAM METRICS:\n",
    "• Total Enrolled: ", dashboard_summary$total_enrolled, " students\n",
    "• Pass Rate: ", pass_rate, "% (", dashboard_summary$passed_students, " students completed successfully)\n",
    "• Currently Active: ", active_rate, "% (", dashboard_summary$active_students, " students still enrolled)\n",
    "• Overall Engagement Rate: ", completion_rate, "% (active + passed students)\n",
    "• Average Sessions Attended: ", dashboard_summary$avg_attendance, " per student\n\n",
    "Provide detailed analysis (200-250 words) covering:\n",
    "1. Program effectiveness based on pass and retention rates\n",
    "2. Student engagement levels and what they indicate\n",
    "3. Attendance patterns and their relationship to success\n",
    "4. Identification of potential at-risk indicators\n",
    "5. Recommendations for improving retention and completion rates\n\n",
    "Focus on actionable insights for program administrators:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 9))
}

generate_recommendations <- function(dashboard_summary) {
  pass_rate <- round((dashboard_summary$passed_students / dashboard_summary$total_enrolled) * 100, 1)
  active_rate <- round((dashboard_summary$active_students / dashboard_summary$total_enrolled) * 100, 1)
  
  prompt <- paste0(
    "Based on this tutoring program data, provide 6-8 specific, actionable recommendations:\n\n",
    "PROGRAM PERFORMANCE:\n",
    "• Total Enrollment: ", dashboard_summary$total_enrolled, " students\n",
    "• Success Rate: ", pass_rate, "% completion rate\n",
    "• Active Retention: ", active_rate, "% currently active\n",
    "• Average Engagement: ", dashboard_summary$avg_attendance, " sessions per student\n\n",
    "Generate numbered recommendations (200-300 words total) focusing on:\n",
    "- Strategies to improve student retention and completion rates\n",
    "- Optimizing resource allocation based on enrollment patterns\n",
    "- Enhancing student engagement and attendance\n",
    "- Addressing any identified performance gaps\n",
    "- Implementing data-driven program improvements\n",
    "- Supporting at-risk student populations\n\n",
    "Format as numbered list with brief explanations. Be specific and actionable:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 10))
}