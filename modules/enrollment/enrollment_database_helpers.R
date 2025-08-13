##Database helpers for enrollment
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

# =============================================================================
# DATA RETRIEVAL FUNCTIONS
# =============================================================================

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
    WHERE e.status IN ('Active', 'Passed')
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
    WHERE e.status IN ('Active', 'Passed') AND e.quarter = ? AND e.subject = ?
    ORDER BY s.grade_level
  "
  result <- dbGetQuery(con, query, params = list(quarter, subject))
  return(result$grade_level)
}

# =============================================================================
# ENROLLMENT MANAGEMENT FUNCTIONS
# =============================================================================

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
         WHERE student_id = ? AND quarter = ? AND subject = ? AND status IN ('Active', 'Passed')",
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

# Remove enrollment
remove_enrollment <- function(con, enrollment_id) {
  dbExecute(con, 
            "UPDATE remedial_enrollments 
     SET status = 'Inactive', updated_at = CURRENT_TIMESTAMP 
     WHERE enrollment_id = ?",
            params = list(enrollment_id))
}

# =============================================================================
# ATTENDANCE MANAGEMENT FUNCTIONS
# =============================================================================

# Update attendance for a single student
update_attendance <- function(con, enrollment_id, attendance) {
  dbExecute(con, 
            "UPDATE remedial_enrollments 
     SET total_attendance = ?, updated_at = CURRENT_TIMESTAMP 
     WHERE enrollment_id = ?",
            params = list(attendance, enrollment_id))
}

# Set attendance for multiple students
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

# =============================================================================
# STATUS MANAGEMENT FUNCTIONS
# =============================================================================

# Mark students as passed
mark_students_passed <- function(con, enrollment_ids) {
  success_count <- 0
  
  for (enrollment_id in enrollment_ids) {
    tryCatch({
      dbExecute(con, 
                "UPDATE remedial_enrollments 
         SET status = 'Passed', updated_at = CURRENT_TIMESTAMP 
         WHERE enrollment_id = ? AND status = 'Active'",
                params = list(enrollment_id))
      
      success_count <- success_count + 1
      cat("Marked enrollment_id", enrollment_id, "as passed\n")
      
    }, error = function(e) {
      cat("Error marking enrollment", enrollment_id, "as passed:", e$message, "\n")
    })
  }
  
  return(success_count)
}

##Null coallescing function

# Null coalescing operator
`%||%` <- function(x, y) if(is.null(x) || is.na(x) || x == "") y else x
