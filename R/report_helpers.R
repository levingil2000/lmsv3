# Report Helper Functions

#' Generate comprehensive dashboard report
generate_dashboard_report <- function(con) {
  report_data <- list()
  
  # Basic statistics
  report_data$stats <- get_dashboard_stats(con)
  
  # Monthly session trends
  report_data$monthly_sessions <- dbGetQuery(con, "
    SELECT 
      strftime('%Y-%m', date) as month,
      COUNT(*) as session_count,
      SUM(duration) as total_hours
    FROM teacher_sessions 
    GROUP BY strftime('%Y-%m', date)
    ORDER BY month DESC
    LIMIT 12
  ")
  
  # Subject distribution
  report_data$subject_distribution <- dbGetQuery(con, "
    SELECT 
      subject,
      COUNT(*) as session_count,
      SUM(duration) as total_hours
    FROM teacher_sessions 
    WHERE subject IS NOT NULL
    GROUP BY subject
    ORDER BY session_count DESC
  ")
  
  # Teacher performance
  report_data$teacher_performance <- get_top_teachers(con)
  
  # Assessment summary
  report_data$assessment_summary <- dbGetQuery(con, "
    SELECT 
      subject,
      COUNT(*) as total_assessments,
      AVG(preassessment_score) as avg_pre,
      AVG(postassessment_score) as avg_post,
      COUNT(CASE WHEN postassessment_score > preassessment_score THEN 1 END) as improved_count
    FROM assessment_table 
    WHERE preassessment_score IS NOT NULL AND postassessment_score IS NOT NULL
    GROUP BY subject
  ")
  
  return(report_data)
}

#' Generate student progress report
generate_student_report <- function(con, student_lrn) {
  student_info <- dbGetQuery(con, "
    SELECT * FROM students_registry WHERE LRN = ?
  ", params = list(student_lrn))
  
  if (nrow(student_info) == 0) {
    return(NULL)
  }
  
  report_data <- list()
  report_data$student_info <- student_info
  
  # Enrollment information
  report_data$enrollments <- dbGetQuery(con, "
    SELECT * FROM enrollment_table WHERE LRN = ?
    ORDER BY date_start DESC
  ", params = list(student_lrn))
  
  # Session attendance
  report_data$sessions_attended <- dbGetQuery(con, "
    SELECT 
      ts.date,
      ts.subject,
      ts.learning_competency,
      ts.duration,
      tr.name as teacher_name
    FROM teacher_sessions ts
    LEFT JOIN teacher_registry tr ON ts.teacher_id = tr.teacherID
    WHERE ts.list_of_students LIKE ?
    ORDER BY ts.date DESC
  ", params = list(paste0("%", student_info$student_name, "%")))
  
  # Assessment results
  report_data$assessments <- dbGetQuery(con, "
    SELECT 
      subject,
      learning_competency,
      preassessment_score,
      postassessment_score,
      (postassessment_score - preassessment_score) as improvement
    FROM assessment_table 
    WHERE student_LRN = ?
    ORDER BY created_at DESC
  ", params = list(student_lrn))
  
  return(report_data)
}

#' Generate teacher performance report
generate_teacher_report <- function(con, teacher_id) {
  teacher_info <- dbGetQuery(con, "
    SELECT * FROM teacher_registry WHERE teacherID = ?
  ", params = list(teacher_id))
  
  if (nrow(teacher_info) == 0) {
    return(NULL)
  }
  
  report_data <- list()
  report_data$teacher_info <- teacher_info
  
  # Session statistics
  report_data$session_stats <- dbGetQuery(con, "
    SELECT 
      COUNT(*) as total_sessions,
      SUM(duration) as total_hours,
      AVG(duration) as avg_session_duration,
      MIN(date) as first_session,
      MAX(date) as last_session
    FROM teacher_sessions 
    WHERE teacher_id = ?
  ", params = list(teacher_id))
  
  # Subject breakdown
  report_data$subject_breakdown <- dbGetQuery(con, "
    SELECT 
      subject,
      COUNT(*) as session_count,
      SUM(duration) as total_hours
    FROM teacher_sessions 
    WHERE teacher_id = ? AND subject IS NOT NULL
    GROUP BY subject
    ORDER BY session_count DESC
  ", params = list(teacher_id))
  
  # Monthly activity
  report_data$monthly_activity <- dbGetQuery(con, "
    SELECT 
      strftime('%Y-%m', date) as month,
      COUNT(*) as session_count,
      SUM(duration) as total_hours
    FROM teacher_sessions 
    WHERE teacher_id = ?
    GROUP BY strftime('%Y-%m', date)
    ORDER BY month DESC
    LIMIT 12
  ", params = list(teacher_id))
  
  # Learning competencies taught
  report_data$competencies_taught <- dbGetQuery(con, "
    SELECT 
      learning_competency,
      COUNT(*) as times_taught,
      SUM(duration) as total_hours
    FROM teacher_sessions 
    WHERE teacher_id = ? AND learning_competency IS NOT NULL
    GROUP BY learning_competency
    ORDER BY times_taught DESC
  ", params = list(teacher_id))
  
  return(report_data)
}

#' Generate assessment performance report
generate_assessment_report <- function(con, filters = list()) {
  where_conditions <- c("1=1")  # Base condition
  
  if (!is.null(filters$subject) && filters$subject != "") {
    where_conditions <- c(where_conditions, paste0("subject = '", filters$subject, "'"))
  }
  
  if (!is.null(filters$grade_level) && filters$grade_level != "") {
    where_conditions <- c(where_conditions, paste0("grade_level = ", filters$grade_level))
  }
  
  if (!is.null(filters$learning_competency) && filters$learning_competency != "") {
    where_conditions <- c(where_conditions, paste0("learning_competency = '", filters$learning_competency, "'"))
  }
  
  where_clause <- paste("WHERE", paste(where_conditions, collapse = " AND "))
  
  report_data <- list()
  
  # Overall statistics
  report_data$overall_stats <- dbGetQuery(con, paste0("
    SELECT 
      COUNT(*) as total_assessments,
      AVG(preassessment_score) as avg_pre_score,
      AVG(postassessment_score) as avg_post_score,
      AVG(postassessment_score - preassessment_score) as avg_improvement,
      COUNT(CASE WHEN postassessment_score > preassessment_score THEN 1 END) as students_improved,
      COUNT(CASE WHEN postassessment_score < preassessment_score THEN 1 END) as students_declined,
      COUNT(CASE WHEN postassessment_score = preassessment_score THEN 1 END) as students_same
    FROM assessment_table 
    ", where_clause, "
    AND preassessment_score IS NOT NULL AND postassessment_score IS NOT NULL
  "))
  
  # Subject-wise performance
  report_data$subject_performance <- dbGetQuery(con, paste0("
    SELECT 
      subject,
      COUNT(*) as assessment_count,
      AVG(preassessment_score) as avg_pre_score,
      AVG(postassessment_score) as avg_post_score,
      AVG(postassessment_score - preassessment_score) as avg_improvement,
      MIN(preassessment_score) as min_pre_score,
      MAX(postassessment_score) as max_post_score
    FROM assessment_table 
    ", where_clause, "
    AND preassessment_score IS NOT NULL AND postassessment_score IS NOT NULL
    GROUP BY subject
    ORDER BY avg_improvement DESC
  "))
  
  # Grade-wise performance
  report_data$grade_performance <- dbGetQuery(con, paste0("
    SELECT 
      grade_level,
      COUNT(*) as assessment_count,
      AVG(preassessment_score) as avg_pre_score,
      AVG(postassessment_score) as avg_post_score,
      AVG(postassessment_score - preassessment_score) as avg_improvement
    FROM assessment_table 
    ", where_clause, "
    AND preassessment_score IS NOT NULL AND postassessment_score IS NOT NULL
    GROUP BY grade_level
    ORDER BY grade_level
  "))
  
  # Top and bottom performers
  report_data$top_performers <- dbGetQuery(con, paste0("
    SELECT 
      sr.student_name,
      at.subject,
      at.preassessment_score,
      at.postassessment_score,
      (at.postassessment_score - at.preassessment_score) as improvement
    FROM assessment_table at
    JOIN students_registry sr ON at.student_LRN = sr.LRN
    ", where_clause, "
    AND at.preassessment_score IS NOT NULL AND at.postassessment_score IS NOT NULL
    ORDER BY improvement DESC
    LIMIT 10
  "))
  
  report_data$bottom_performers <- dbGetQuery(con, paste0("
    SELECT 
      sr.student_name,
      at.subject,
      at.preassessment_score,
      at.postassessment_score,
      (at.postassessment_score - at.preassessment_score) as improvement
    FROM assessment_table at
    JOIN students_registry sr ON at.student_LRN = sr.LRN
    ", where_clause, "
    AND at.preassessment_score IS NOT NULL AND at.postassessment_score IS NOT NULL
    ORDER BY improvement ASC
    LIMIT 10
  "))
  
  return(report_data)
}

#' Generate facility inventory report
generate_facility_report <- function(con) {
  report_data <- list()
  
  # Overall statistics
  report_data$overview <- dbGetQuery(con, "
    SELECT 
      COUNT(*) as total_facilities,
      COUNT(CASE WHEN status = 'Available' THEN 1 END) as available_count,
      COUNT(CASE WHEN status = 'In Use' THEN 1 END) as in_use_count,
      COUNT(CASE WHEN status = 'Under Maintenance' THEN 1 END) as maintenance_count,
      COUNT(CASE WHEN status = 'Damaged' THEN 1 END) as damaged_count,
      SUM(amount) as total_value
    FROM facilities_table
  ")
  
  # By type breakdown
  report_data$by_type <- dbGetQuery(con, "
    SELECT 
      type,
      COUNT(*) as count,
      SUM(amount) as total_value,
      AVG(amount) as avg_value
    FROM facilities_table 
    WHERE type IS NOT NULL
    GROUP BY type
    ORDER BY count DESC
  ")
  
  # Status breakdown
  report_data$by_status <- dbGetQuery(con, "
    SELECT 
      status,
      COUNT(*) as count,
      SUM(amount) as total_value
    FROM facilities_table 
    GROUP BY status
  ")
  
  # High-value items
  report_data$high_value_items <- dbGetQuery(con, "
    SELECT 
      name,
      type,
      status,
      amount,
      description
    FROM facilities_table 
    WHERE amount > 0
    ORDER BY amount DESC
    LIMIT 20
  ")
  
  return(report_data)
}

#' Generate partner contribution report
generate_partner_report <- function(con) {
  report_data <- list()
  
  # Overall statistics
  report_data$overview <- dbGetQuery(con, "
    SELECT 
      COUNT(*) as total_partners,
      SUM(amount) as total_contributions,
      AVG(amount) as avg_contribution,
      MAX(amount) as highest_contribution,
      COUNT(CASE WHEN amount > 0 THEN 1 END) as monetary_contributors
    FROM partners_table
  ")
  
  # Top contributors
  report_data$top_contributors <- dbGetQuery(con, "
    SELECT 
      name,
      amount,
      service_cash_fixture_provided,
      date_given,
      contact_person_partner
    FROM partners_table 
    WHERE amount > 0
    ORDER BY amount DESC
    LIMIT 15
  ")
  
  # Monthly contributions
  report_data$monthly_trends <- dbGetQuery(con, "
    SELECT 
      strftime('%Y-%m', date_given) as month,
      COUNT(*) as partner_count,
      SUM(amount) as total_amount
    FROM partners_table 
    WHERE date_given IS NOT NULL
    GROUP BY strftime('%Y-%m', date_given)
    ORDER BY month DESC
    LIMIT 12
  ")
  
  # Contribution types analysis
  report_data$contribution_types <- dbGetQuery(con, "
    SELECT 
      service_cash_fixture_provided,
      COUNT(*) as frequency,
      SUM(amount) as total_value
    FROM partners_table 
    WHERE service_cash_fixture_provided IS NOT NULL AND service_cash_fixture_provided != ''
    GROUP BY service_cash_fixture_provided
    ORDER BY frequency DESC
  ")
  
  return(report_data)
}

#' Export data to CSV format
export_to_csv <- function(data, filename) {
  tryCatch({
    write.csv(data, filename, row.names = FALSE)
    return(list(success = TRUE, message = paste("Data exported to", filename)))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Export failed:", e$message)))
  })
}

#' Generate summary statistics for any table
generate_table_summary <- function(con, table_name) {
  # Get table structure
  structure <- dbGetQuery(con, paste0("PRAGMA table_info(", table_name, ")"))
  
  # Get basic counts
  total_records <- dbGetQuery(con, paste0("SELECT COUNT(*) as count FROM ", table_name))$count
  
  # Get date range if date columns exist
  date_columns <- structure$name[grepl("date|created_at|updated_at", structure$name, ignore.case = TRUE)]
  
  summary_data <- list(
    table_name = table_name,
    total_records = total_records,
    columns = nrow(structure),
    structure = structure
  )
  
  if (length(date_columns) > 0) {
    for (date_col in date_columns) {
      date_range <- dbGetQuery(con, paste0("
        SELECT 
          MIN(", date_col, ") as earliest,
          MAX(", date_col, ") as latest
        FROM ", table_name, "
        WHERE ", date_col, " IS NOT NULL
      "))
      summary_data[[paste0(date_col, "_range")]] <- date_range
    }
  }
  
  return(summary_data)
}

#' Validate data integrity across tables
validate_data_integrity <- function(con) {
  integrity_issues <- list()
  
  # Check for orphaned records in enrollment_table
  orphaned_enrollments <- dbGetQuery(con, "
    SELECT COUNT(*) as count 
    FROM enrollment_table et
    LEFT JOIN students_registry sr ON et.LRN = sr.LRN
    WHERE sr.LRN IS NULL
  ")$count
  
  if (orphaned_enrollments > 0) {
    integrity_issues <- c(integrity_issues, paste("Orphaned enrollments:", orphaned_enrollments))
  }
  
  # Check for orphaned sessions
  orphaned_sessions <- dbGetQuery(con, "
    SELECT COUNT(*) as count 
    FROM teacher_sessions ts
    LEFT JOIN teacher_registry tr ON ts.teacher_id = tr.teacherID
    WHERE tr.teacherID IS NULL
  ")$count
  
  if (orphaned_sessions > 0) {
    integrity_issues <- c(integrity_issues, paste("Orphaned sessions:", orphaned_sessions))
  }
  
  # Check for orphaned assessments
  orphaned_assessments <- dbGetQuery(con, "
    SELECT COUNT(*) as count 
    FROM assessment_table at
    LEFT JOIN students_registry sr ON at.student_LRN = sr.LRN
    WHERE sr.LRN IS NULL
  ")$count
  
  if (orphaned_assessments > 0) {
    integrity_issues <- c(integrity_issues, paste("Orphaned assessments:", orphaned_assessments))
  }
  
  # Check for duplicate LRNs
  duplicate_lrns <- dbGetQuery(con, "
    SELECT COUNT(*) as count 
    FROM (
      SELECT LRN 
      FROM students_registry 
      GROUP BY LRN 
      HAVING COUNT(*) > 1
    )
  ")$count
  
  if (duplicate_lrns > 0) {
    integrity_issues <- c(integrity_issues, paste("Duplicate LRNs:", duplicate_lrns))
  }
  
  if (length(integrity_issues) == 0) {
    return(list(status = "OK", message = "No data integrity issues found"))
  } else {
    return(list(status = "ISSUES", issues = integrity_issues))
  }
}