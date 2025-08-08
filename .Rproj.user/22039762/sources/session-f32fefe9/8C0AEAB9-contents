# Database Helper Functions

source("R/initialize_sample_data.R")

#' Connect to SQLite database
db_connect <- function(db_path) {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Create tables if they don't exist
  create_tables(con)
  
  # Initialize sample data if tables are empty
  check_and_initialize_data(con)
  
  return(con)
}

#' Create all required tables
create_tables <- function(con) {
  
  # Students Registry Table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS students_registry (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      student_name TEXT NOT NULL,
      grade_level INTEGER NOT NULL,
      section TEXT,
      LRN TEXT UNIQUE,
      address TEXT,
      narrative TEXT,
      parent_name TEXT,
      parent_contact TEXT,
      consent_given BOOLEAN DEFAULT FALSE,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Enrollment Table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS enrollment_table (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      student_name TEXT NOT NULL,
      LRN TEXT NOT NULL,
      subject TEXT NOT NULL,
      preassessment REAL,
      date_start DATE,
      status TEXT DEFAULT 'Active',
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (LRN) REFERENCES students_registry(LRN)
    )
  ")
  
  # Teacher Registry Table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS teacher_registry (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL,
      teacherID TEXT UNIQUE NOT NULL,
      subject_handled TEXT,
      year_level INTEGER,
      teacher_status TEXT DEFAULT 'Active',
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Teacher Sessions Table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS teacher_sessions (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      session_id TEXT UNIQUE NOT NULL,
      teacher_id TEXT NOT NULL,
      date DATE NOT NULL,
      duration REAL NOT NULL,
      learning_competency TEXT,
      subject TEXT,
      grade_level INTEGER,
      list_of_students TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (teacher_id) REFERENCES teacher_registry(teacherID)
    )
  ")
  
  # Assessment Table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS assessment_table (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      assessment_id TEXT UNIQUE NOT NULL,
      student_LRN TEXT NOT NULL,
      subject TEXT NOT NULL,
      learning_competency TEXT,
      grade_level INTEGER,
      preassessment_score REAL,
      postassessment_score REAL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (student_LRN) REFERENCES students_registry(LRN)
    )
  ")
  
  # Facilities Table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS facilities_table (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      FR_ID TEXT UNIQUE NOT NULL,
      name TEXT NOT NULL,
      description TEXT,
      status TEXT DEFAULT 'Available',
      type TEXT,
      amount REAL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Partners Table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS partners_table (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      partner_ID TEXT UNIQUE NOT NULL,
      name TEXT NOT NULL,
      amount REAL,
      service_cash_fixture_provided TEXT,
      date_given DATE,
      contact_person_partner TEXT,
      contact_no_partner TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
}

#' Generic function to get all records from a table
get_table_data <- function(con, table_name) {
  query <- paste("SELECT * FROM", table_name)
  dbGetQuery(con, query)
}

#' Insert data into a table
insert_data <- function(con, table_name, data) {
  tryCatch({
    dbWriteTable(con, table_name, data, append = TRUE, row.names = FALSE)
    return(list(success = TRUE, message = "Data inserted successfully"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}

#' Update data in a table
update_data <- function(con, table_name, data, id_column, id_value) {
  tryCatch({
    # Build SET clause
    set_clause <- paste(names(data), "=", paste0("'", data, "'"), collapse = ", ")
    
    query <- paste0("UPDATE ", table_name, " SET ", set_clause, 
                    " WHERE ", id_column, " = '", id_value, "'")
    
    dbExecute(con, query)
    return(list(success = TRUE, message = "Data updated successfully"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}

#' Delete data from a table
delete_data <- function(con, table_name, id_column, id_value) {
  tryCatch({
    query <- paste0("DELETE FROM ", table_name, " WHERE ", id_column, " = '", id_value, "'")
    dbExecute(con, query)
    return(list(success = TRUE, message = "Data deleted successfully"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}

#' Get dashboard summary statistics
get_dashboard_stats <- function(con) {
  stats <- list()
  
  # Total students enrolled
  stats$total_students <- dbGetQuery(con, "SELECT COUNT(*) as count FROM students_registry")$count
  
  # Total teachers
  stats$total_teachers <- dbGetQuery(con, "SELECT COUNT(*) as count FROM teacher_registry WHERE teacher_status = 'Active'")$count
  
  # Total tutoring hours
  stats$total_hours <- dbGetQuery(con, "SELECT SUM(duration) as total FROM teacher_sessions")$total
  if(is.na(stats$total_hours)) stats$total_hours <- 0
  
  # Total graduates (assuming status = 'Completed' in enrollment)
  stats$total_graduates <- dbGetQuery(con, "SELECT COUNT(*) as count FROM enrollment_table WHERE status = 'Completed'")$count
  
  # Total amount from partners
  stats$total_partner_amount <- dbGetQuery(con, "SELECT SUM(amount) as total FROM partners_table")$total
  if(is.na(stats$total_partner_amount)) stats$total_partner_amount <- 0
  
  return(stats)
}

#' Get top 5 teachers with most tutor time
get_top_teachers <- function(con) {
  query <- "
    SELECT tr.name, SUM(ts.duration) as total_hours
    FROM teacher_registry tr
    LEFT JOIN teacher_sessions ts ON tr.teacherID = ts.teacher_id
    GROUP BY tr.name
    ORDER BY total_hours DESC
    LIMIT 5
  "
  dbGetQuery(con, query)
}

#' Get poor performing students
get_poor_performing_students <- function(con, learning_competency = NULL) {
  where_clause <- ""
  if (!is.null(learning_competency) && learning_competency != "") {
    where_clause <- paste0("WHERE learning_competency = '", learning_competency, "'")
  }
  
  query <- paste0("
    SELECT sr.student_name, at.subject, at.learning_competency, 
           at.preassessment_score, at.postassessment_score
    FROM assessment_table at
    JOIN students_registry sr ON at.student_LRN = sr.LRN
    ", where_clause, "
    ORDER BY at.preassessment_score ASC
    LIMIT 10
  ")
  
  dbGetQuery(con, query)
}