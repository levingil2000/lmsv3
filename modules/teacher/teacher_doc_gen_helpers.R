# teacher_report_generator.R
# Auto Report Generator for Teacher Analytics

generate_teacher_report <- function(con, filters = list()) {
  # Build filter conditions
  build_report_filter <- function(filter_list) {
    conditions <- c()
    
    if (!is.null(filter_list$grade_level) && filter_list$grade_level != "") {
      conditions <- c(conditions, paste0("ts.grade_level = ", filter_list$grade_level))
    }
    
    if (!is.null(filter_list$subject) && filter_list$subject != "") {
      conditions <- c(conditions, paste0("ts.subject = '", filter_list$subject, "'"))
    }
    
    if (!is.null(filter_list$competency) && filter_list$competency != "") {
      conditions <- c(conditions, paste0("ts.learning_competency = '", filter_list$competency, "'"))
    }
    
    if (!is.null(filter_list$date_from) && !is.null(filter_list$date_to)) {
      conditions <- c(conditions, paste0("DATE(ts.session_date) BETWEEN '", 
                                         filter_list$date_from, "' AND '", filter_list$date_to, "'"))
    }
    
    if (length(conditions) > 0) {
      return(paste0("WHERE tr.teacher_status = 'Active' AND ", paste(conditions, collapse = " AND ")))
    } else {
      return("WHERE tr.teacher_status = 'Active'")
    }
  }
  
  filter_conditions <- build_report_filter(filters)
  
  # 1. Get Teaching Hours Data
  hours_query <- paste0("
    SELECT tr.name, 
           tr.subject_handled,
           SUM(ts.duration) as total_hours,
           COUNT(ts.id) as total_sessions,
           ROUND(AVG(ts.duration), 2) as avg_duration
    FROM teacher_registry tr
    LEFT JOIN teacher_sessions ts ON tr.teacherID = ts.teacher_id
    ", filter_conditions, "
    GROUP BY tr.name, tr.subject_handled
    HAVING total_hours > 0
    ORDER BY total_hours DESC
  ")
  
  hours_data <- dbGetQuery(con, hours_query)
  
  # 2. Get Learning Competency Data
  competency_query <- paste0("
    SELECT learning_competency, 
           COUNT(*) as frequency,
           COUNT(DISTINCT ts.teacher_id) as teachers_involved
    FROM teacher_sessions ts
    JOIN teacher_registry tr ON tr.teacherID = ts.teacher_id
    ", filter_conditions, " 
    AND learning_competency IS NOT NULL AND learning_competency != ''
    GROUP BY learning_competency
    ORDER BY frequency DESC
    LIMIT 10
  ")
  
  competency_data <- dbGetQuery(con, competency_query)
  
  # 3. Get Sentiment Analysis Data
  remarks_query <- paste0("
    SELECT remarks_observation
    FROM teacher_sessions ts
    JOIN teacher_registry tr ON tr.teacherID = ts.teacher_id
    ", filter_conditions, "
    AND remarks_observation IS NOT NULL AND remarks_observation != ''
  ")
  
  reflection_query <- paste0("
    SELECT assessment_reflection
    FROM teacher_sessions ts
    JOIN teacher_registry tr ON tr.teacherID = ts.teacher_id
    ", filter_conditions, "
    AND assessment_reflection IS NOT NULL AND assessment_reflection != ''
  ")
  
  remarks_data <- dbGetQuery(con, remarks_query)
  reflections_data <- dbGetQuery(con, reflection_query)
  
  # 4. Perform Sentiment Analysis
  analyze_sentiment <- function(text_data) {
    if (nrow(text_data) > 0 && !all(is.na(text_data[[1]]))) {
      clean_text <- text_data[[1]][!is.na(text_data[[1]]) & text_data[[1]] != ""]
      if (length(clean_text) > 0) {
        tryCatch({
          sentiment_results <- sentiment(clean_text)
          avg_sentiment <- mean(sentiment_results$sentiment, na.rm = TRUE)
          return(pmax(-1, pmin(1, avg_sentiment)))
        }, error = function(e) {
          return(0)
        })
      }
    }
    return(0)
  }
  
  remarks_sentiment <- analyze_sentiment(remarks_data)
  reflections_sentiment <- analyze_sentiment(reflections_data)
  
  # 5. Get Top Performing Teachers
  top_teachers_query <- paste0("
    SELECT tr.name,
           tr.subject_handled,
           COUNT(ts.id) as sessions,
           SUM(ts.duration) as total_hours,
           ROUND(AVG(ts.duration), 2) as avg_session_length
    FROM teacher_registry tr
    JOIN teacher_sessions ts ON tr.teacherID = ts.teacher_id
    ", filter_conditions, "
    GROUP BY tr.name, tr.subject_handled
    ORDER BY total_hours DESC
    LIMIT 5
  ")
  
  top_teachers <- dbGetQuery(con, top_teachers_query)
  
  # 6. Generate Summary Statistics
  total_teachers <- nrow(dbGetQuery(con, paste0("SELECT DISTINCT tr.teacherID FROM teacher_registry tr 
                                                JOIN teacher_sessions ts ON tr.teacherID = ts.teacher_id ", 
                                                filter_conditions)))
  
  total_sessions <- nrow(dbGetQuery(con, paste0("SELECT ts.id FROM teacher_sessions ts
                                                JOIN teacher_registry tr ON tr.teacherID = ts.teacher_id ",
                                                filter_conditions)))
  
  total_hours <- dbGetQuery(con, paste0("SELECT SUM(ts.duration) as total FROM teacher_sessions ts
                                        JOIN teacher_registry tr ON tr.teacherID = ts.teacher_id ",
                                        filter_conditions))$total
  
  if (is.na(total_hours)) total_hours <- 0
  
  # 7. Create data summary for AI analysis
  data_summary <- list(
    summary_stats = list(
      total_teachers = total_teachers,
      total_sessions = total_sessions,
      total_hours = round(total_hours, 2),
      avg_hours_per_teacher = if(total_teachers > 0) round(total_hours / total_teachers, 2) else 0
    ),
    top_teachers = top_teachers,
    competency_distribution = competency_data,
    sentiment_analysis = list(
      remarks_sentiment = remarks_sentiment,
      reflections_sentiment = reflections_sentiment,
      remarks_interpretation = case_when(
        remarks_sentiment > 0.1 ~ "Positive",
        remarks_sentiment < -0.1 ~ "Negative",
        TRUE ~ "Neutral"
      ),
      reflections_interpretation = case_when(
        reflections_sentiment > 0.1 ~ "Positive",
        reflections_sentiment < -0.1 ~ "Negative",
        TRUE ~ "Neutral"
      )
    ),
    hours_distribution = hours_data
  )
  
  # 8. Generate AI-powered insights using Gemini
  prompt <- sprintf("
  Based on the following teacher analytics data, generate a comprehensive educational report with insights, recommendations, and analysis. 
  
  SUMMARY STATISTICS:
  - Total Active Teachers: %d
  - Total Teaching Sessions: %d
  - Total Teaching Hours: %.2f
  - Average Hours per Teacher: %.2f
  
  TOP PERFORMING TEACHERS (by hours):
  %s
  
  LEARNING COMPETENCY DISTRIBUTION:
  %s
  
  SENTIMENT ANALYSIS:
  - Teacher Remarks Sentiment: %s (Score: %.3f)
  - Assessment Reflections Sentiment: %s (Score: %.3f)
  
  Please provide:
  1. Executive Summary
  2. Key Performance Insights
  3. Teacher Performance Analysis
  4. Learning Competency Analysis
  5. Sentiment Analysis Interpretation
  6. Recommendations for Improvement
  7. Action Items
  
  Format the response in a professional report style suitable for educational administrators.
  ",
                    data_summary$summary_stats$total_teachers,
                    data_summary$summary_stats$total_sessions,
                    data_summary$summary_stats$total_hours,
                    data_summary$summary_stats$avg_hours_per_teacher,
                    paste(apply(top_teachers, 1, function(x) paste(x["name"], "-", x["subject_handled"], "(", x["total_hours"], "hrs)")), collapse = "\n"),
                    paste(apply(competency_data[1:min(5, nrow(competency_data)), ], 1, function(x) paste(x["learning_competency"], "(", x["frequency"], "sessions)")), collapse = "\n"),
                    data_summary$sentiment_analysis$remarks_interpretation,
                    data_summary$sentiment_analysis$remarks_sentiment,
                    data_summary$sentiment_analysis$reflections_interpretation,
                    data_summary$sentiment_analysis$reflections_sentiment
  )
  
  # Generate content using Gemini
  tryCatch({
    ai_insights <- gemini(prompt)
  }, error = function(e) {
    ai_insights <- paste0("Unable to generate AI insights. Error: ", e$message)
  })
  
  # 9. Create Word Document
  doc <- read_docx()
  
  # Add title
  filter_title <- ""
  if (!is.null(report_filters$subject) && report_filters$subject != "") {
    filter_title <- paste0(" - ", report_filters$subject)
  }
  if (!is.null(report_filters$grade_level) && report_filters$grade_level != "") {
    filter_title <- paste0(filter_title, " - Grade ", report_filters$grade_level)
  }
  
  doc <- doc %>%
    body_add_par(paste0("Teacher Analytics Report", filter_title), style = "heading 1") %>%
    body_add_par(paste0("Generated on: ", Sys.Date()), style = "Normal") %>%
    body_add_break()
  
  # Add AI-generated insights
  doc <- doc %>%
    body_add_par("AI-Generated Insights and Analysis", style = "heading 2") %>%
    body_add_par(ai_insights, style = "Normal") %>%
    body_add_break()
  
  # Add data tables
  # Top Teachers Table
  if (nrow(top_teachers) > 0) {
    top_teachers_ft <- top_teachers %>%
      flextable() %>%
      set_header_labels(
        name = "Teacher Name",
        subject_handled = "Subject",
        sessions = "Sessions",
        total_hours = "Total Hours",
        avg_session_length = "Avg Session Length"
      ) %>%
      theme_vanilla() %>%
      autofit()
    
    doc <- doc %>%
      body_add_par("Top Performing Teachers", style = "heading 2") %>%
      body_add_flextable(top_teachers_ft) %>%
      body_add_break()
  }
  
  # Learning Competencies Table
  if (nrow(competency_data) > 0) {
    competency_ft <- competency_data %>%
      flextable() %>%
      set_header_labels(
        learning_competency = "Learning Competency",
        frequency = "Sessions Count",
        teachers_involved = "Teachers Involved"
      ) %>%
      theme_vanilla() %>%
      autofit()
    
    doc <- doc %>%
      body_add_par("Learning Competency Distribution", style = "heading 2") %>%
      body_add_flextable(competency_ft) %>%
      body_add_break()
  }
  
  # All Teachers Performance Table
  if (nrow(hours_data) > 0) {
    hours_ft <- hours_data %>%
      flextable() %>%
      set_header_labels(
        name = "Teacher Name",
        subject_handled = "Subject",
        total_hours = "Total Hours",
        total_sessions = "Sessions",
        avg_duration = "Avg Duration"
      ) %>%
      theme_vanilla() %>%
      autofit()
    
    doc <- doc %>%
      body_add_par("All Teachers Performance Summary", style = "heading 2") %>%
      body_add_flextable(hours_ft)
  }
  
  # Generate filename
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0("Teacher_Analytics_Report_", timestamp, ".docx")
  
  # Save document
  print(doc, target = filename)
  
  return(list(
    success = TRUE,
    filename = filename,
    message = paste0("Report generated successfully: ", filename),
    data_summary = data_summary
  ))
}

# Helper function for report generation with error handling
safe_generate_report <- function(con, filters = list()) {
  tryCatch({
    generate_teacher_report(con, filters)
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste0("Error generating report: ", e$message),
      filename = NULL
    ))
  })
}