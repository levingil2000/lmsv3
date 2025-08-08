# Helper functions to stitch sessions, teachers and students

#' Get detailed session information with teacher and student details
get_session_details <- function(con, session_id = NULL) {
  base_query <- "
    SELECT 
      ts.*,
      tr.name as teacher_name,
      tr.subject_handled as teacher_subject
    FROM teacher_sessions ts
    LEFT JOIN teacher_registry tr ON ts.teacher_id = tr.teacherID
  "
  
  if (!is.null(session_id)) {
    query <- paste0(base_query, " WHERE ts.session_id = '", session_id, "'")
  } else {
    query <- base_query
  }
  
  dbGetQuery(con, query)
}

#' Get student participation in sessions
get_student_sessions <- function(con, student_lrn = NULL) {
  base_query <- "
    SELECT 
      ts.*,
      tr.name as teacher_name,
      sr.student_name,
      sr.grade_level as student_grade
    FROM teacher_sessions ts
    LEFT JOIN teacher_registry tr ON ts.teacher_id = tr.teacherID
    LEFT JOIN students_registry sr ON ts.list_of_students LIKE '%' || sr.student_name || '%'
  "
  
  if (!is.null(student_lrn)) {
    query <- paste0(base_query, " WHERE sr.LRN = '", student_lrn, "'")
  } else {
    query <- base_query
  }
  
  dbGetQuery(con, query)
}

#' Get teacher session summary with student count
get_teacher_session_summary <- function(con, teacher_id = NULL) {
  base_query <- "
    SELECT 
      tr.teacherID,
      tr.name as teacher_name,
      tr.subject_handled,
      COUNT(ts.id) as total_sessions,
      SUM(ts.duration) as total_hours,
      AVG(ts.duration) as avg_session_duration,
      COUNT(DISTINCT ts.subject) as subjects_taught
    FROM teacher_registry tr
    LEFT JOIN teacher_sessions ts ON tr.teacherID = ts.teacher_id
  "
  
  where_clause <- "WHERE tr.teacher_status = 'Active'"
  
  if (!is.null(teacher_id)) {
    where_clause <- paste0(where_clause, " AND tr.teacherID = '", teacher_id, "'")
  }
  
  group_clause <- "GROUP BY tr.teacherID, tr.name, tr.subject_handled"
  
  query <- paste(base_query, where_clause, group_clause)
  
  dbGetQuery(con, query)
}

#' Parse student list from session
parse_student_list <- function(student_list_string) {
  if (is.na(student_list_string) || student_list_string == "") {
    return(character(0))
  }
  
  # Split by comma and clean up
  students <- strsplit(student_list_string, ",")[[1]]
  students <- trimws(students)
  students <- students[students != ""]
  
  return(students)
}

#' Get students enrolled in a specific subject
get_students_by_subject <- function(con, subject) {
  query <- "
    SELECT DISTINCT 
      sr.student_name,
      sr.LRN,
      sr.grade_level,
      sr.section
    FROM students_registry sr
    JOIN enrollment_table et ON sr.LRN = et.LRN
    WHERE et.subject = ? AND et.status = 'Active'
  "
  
  dbGetQuery(con, query, params = list(subject))
}

#' Get learning competency progress for a student
get_student_competency_progress <- function(con, student_lrn) {
  query <- "
    SELECT 
      at.learning_competency,
      at.subject,
      at.preassessment_score,
      at.postassessment_score,
      COUNT(ts.id) as sessions_attended
    FROM assessment_table at
    LEFT JOIN students_registry sr ON at.student_LRN = sr.LRN
    LEFT JOIN teacher_sessions ts ON ts.list_of_students LIKE '%' || sr.student_name || '%' 
                                   AND ts.learning_competency = at.learning_competency
    WHERE at.student_LRN = ?
    GROUP BY at.learning_competency, at.subject, at.preassessment_score, at.postassessment_score
  "
  
  dbGetQuery(con, query, params = list(student_lrn))
}

#' Get session attendance summary
get_session_attendance_summary <- function(con, date_from = NULL, date_to = NULL) {
  base_query <- "
    SELECT 
      DATE(ts.date) as session_date,
      COUNT(ts.id) as total_sessions,
      SUM(ts.duration) as total_hours,
      GROUP_CONCAT(DISTINCT ts.subject) as subjects_covered,
      GROUP_CONCAT(DISTINCT tr.name) as teachers_involved
    FROM teacher_sessions ts
    LEFT JOIN teacher_registry tr ON ts.teacher_id = tr.teacherID
  "
  
  where_conditions <- c()
  
  if (!is.null(date_from)) {
    where_conditions <- c(where_conditions, paste0("ts.date >= '", date_from, "'"))
  }
  
  if (!is.null(date_to)) {
    where_conditions <- c(where_conditions, paste0("ts.date <= '", date_to, "'"))
  }
  
  if (length(where_conditions) > 0) {
    where_clause <- paste("WHERE", paste(where_conditions, collapse = " AND "))
  } else {
    where_clause <- ""
  }
  
  group_clause <- "GROUP BY DATE(ts.date) ORDER BY session_date DESC"
  
  query <- paste(base_query, where_clause, group_clause)
  
  dbGetQuery(con, query)
}

#' Validate student list in session
validate_student_list <- function(con, student_list_string) {
  students <- parse_student_list(student_list_string)
  
  if (length(students) == 0) {
    return(list(valid = TRUE, message = "No students listed", invalid_students = character(0)))
  }
  
  # Check if all students exist in the registry
  existing_students <- dbGetQuery(con, "SELECT student_name FROM students_registry")$student_name
  
  invalid_students <- students[!students %in% existing_students]
  
  if (length(invalid_students) > 0) {
    return(list(
      valid = FALSE, 
      message = paste("Invalid students:", paste(invalid_students, collapse = ", ")),
      invalid_students = invalid_students
    ))
  }
  
  return(list(valid = TRUE, message = "All students are valid", invalid_students = character(0)))
}

#' Get subject performance summary
get_subject_performance_summary <- function(con) {
  query <- "
    SELECT 
      at.subject,
      COUNT(*) as total_assessments,
      AVG(at.preassessment_score) as avg_pre_score,
      AVG(at.postassessment_score) as avg_post_score,
      AVG(at.postassessment_score - at.preassessment_score) as avg_improvement,
      COUNT(DISTINCT at.student_LRN) as unique_students,
      SUM(CASE WHEN at.postassessment_score > at.preassessment_score THEN 1 ELSE 0 END) as students_improved
    FROM assessment_table at
    WHERE at.preassessment_score IS NOT NULL AND at.postassessment_score IS NOT NULL
    GROUP BY at.subject
    ORDER BY avg_improvement DESC
  "
  
  dbGetQuery(con, query)
}