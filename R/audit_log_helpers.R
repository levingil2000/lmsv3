# WAL-Based Database Change Logging System
# Add this to your existing database helper functions

library(DBI)
library(RSQLite)
library(jsonlite)
library(lubridate)

#' Enhanced database connection with WAL logging enabled
db_connect_with_wal <- function(db_path) {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Enable WAL mode
  dbExecute(con, "PRAGMA journal_mode=WAL")
  
  # Optional: Set WAL checkpoint threshold (default is 1000 pages)
  dbExecute(con, "PRAGMA wal_autocheckpoint=100")
  
  # Create tables if they don't exist
  create_tables(con)
  
  # Create audit log table
  create_audit_log_table(con)
  
  return(con)
}

#' Create audit log table to store processed WAL entries
create_audit_log_table <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS audit_log (
      log_id INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
      table_name TEXT NOT NULL,
      operation TEXT NOT NULL,  -- INSERT, UPDATE, DELETE
      record_id TEXT,
      old_values TEXT,  -- JSON string
      new_values TEXT,  -- JSON string
      user_session TEXT,
      processed_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
}

#' Enhanced wrapper functions with automatic change logging
#' These replace your existing insert_data, update_data, delete_data functions

#' Insert data with logging
insert_data_with_log <- function(con, table_name, data, user_session = NULL) {
  tryCatch({
    # Get the record before insertion (for auto-increment ID capture)
    max_id_query <- paste0("SELECT MAX(id) as max_id FROM ", table_name)
    max_id_result <- dbGetQuery(con, max_id_query)
    next_id <- if(is.na(max_id_result$max_id)) 1 else max_id_result$max_id + 1
    
    # Perform the insertion
    dbWriteTable(con, table_name, data, append = TRUE, row.names = FALSE)
    
    # Log the change
    log_change(con, table_name, "INSERT", next_id, NULL, data, user_session)
    
    return(list(success = TRUE, message = "Data inserted successfully", record_id = next_id))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}

#' Update data with logging
update_data_with_log <- function(con, table_name, data, id_column, id_value, user_session = NULL) {
  tryCatch({
    # Get old values before update
    old_query <- paste0("SELECT * FROM ", table_name, " WHERE ", id_column, " = '", id_value, "'")
    old_data <- dbGetQuery(con, old_query)
    
    # Build SET clause
    set_clause <- paste(names(data), "=", paste0("'", data, "'"), collapse = ", ")
    query <- paste0("UPDATE ", table_name, " SET ", set_clause, 
                    " WHERE ", id_column, " = '", id_value, "'")
    
    # Perform the update
    dbExecute(con, query)
    
    # Log the change
    if(nrow(old_data) > 0) {
      log_change(con, table_name, "UPDATE", id_value, old_data[1,], data, user_session)
    }
    
    return(list(success = TRUE, message = "Data updated successfully"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}

#' Delete data with logging
delete_data_with_log <- function(con, table_name, id_column, id_value, user_session = NULL) {
  tryCatch({
    # Get old values before deletion
    old_query <- paste0("SELECT * FROM ", table_name, " WHERE ", id_column, " = '", id_value, "'")
    old_data <- dbGetQuery(con, old_query)
    
    # Perform the deletion
    query <- paste0("DELETE FROM ", table_name, " WHERE ", id_column, " = '", id_value, "'")
    dbExecute(con, query)
    
    # Log the change
    if(nrow(old_data) > 0) {
      log_change(con, table_name, "DELETE", id_value, old_data[1,], NULL, user_session)
    }
    
    return(list(success = TRUE, message = "Data deleted successfully"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}

#' Log database changes to audit table
log_change <- function(con, table_name, operation, record_id, old_values, new_values, user_session) {
  tryCatch({
    # Convert data frames/lists to JSON strings
    old_json <- if(!is.null(old_values)) toJSON(old_values, auto_unbox = TRUE) else NULL
    new_json <- if(!is.null(new_values)) toJSON(new_values, auto_unbox = TRUE) else NULL
    
    # Insert into audit log
    audit_data <- data.frame(
      table_name = table_name,
      operation = operation,
      record_id = as.character(record_id),
      old_values = old_json,
      new_values = new_json,
      user_session = user_session %||% "unknown",
      stringsAsFactors = FALSE
    )
    
    dbWriteTable(con, "audit_log", audit_data, append = TRUE, row.names = FALSE)
  }, error = function(e) {
    warning(paste("Failed to log change:", e$message))
  })
}

#' Export audit logs to JSON file
export_audit_logs <- function(con, start_date = NULL, end_date = NULL, output_file = NULL) {
  # Build query with optional date filtering
  query <- "SELECT * FROM audit_log"
  conditions <- c()
  
  if(!is.null(start_date)) {
    conditions <- c(conditions, paste0("DATE(timestamp) >= '", start_date, "'"))
  }
  if(!is.null(end_date)) {
    conditions <- c(conditions, paste0("DATE(timestamp) <= '", end_date, "'"))
  }
  
  if(length(conditions) > 0) {
    query <- paste(query, "WHERE", paste(conditions, collapse = " AND "))
  }
  
  query <- paste(query, "ORDER BY timestamp DESC")
  
  # Get the data
  audit_data <- dbGetQuery(con, query)
  
  # Create filename if not provided
  if(is.null(output_file)) {
    output_file <- paste0("audit_log_", format(Sys.Date(), "%Y%m%d"), ".json")
  }
  
  # Export to JSON
  write(toJSON(audit_data, pretty = TRUE), output_file)
  
  return(list(
    success = TRUE, 
    message = paste("Exported", nrow(audit_data), "records to", output_file),
    records = nrow(audit_data)
  ))
}

#' Get audit log summary statistics
get_audit_stats <- function(con, days_back = 30) {
  cutoff_date <- Sys.Date() - days(days_back)
  
  stats <- list()
  
  # Total changes in period
  total_query <- paste0("SELECT COUNT(*) as count FROM audit_log WHERE DATE(timestamp) >= '", cutoff_date, "'")
  stats$total_changes <- dbGetQuery(con, total_query)$count
  
  # Changes by operation
  op_query <- paste0("
    SELECT operation, COUNT(*) as count 
    FROM audit_log 
    WHERE DATE(timestamp) >= '", cutoff_date, "' 
    GROUP BY operation
  ")
  stats$by_operation <- dbGetQuery(con, op_query)
  
  # Changes by table
  table_query <- paste0("
    SELECT table_name, COUNT(*) as count 
    FROM audit_log 
    WHERE DATE(timestamp) >= '", cutoff_date, "' 
    GROUP BY table_name 
    ORDER BY count DESC
  ")
  stats$by_table <- dbGetQuery(con, table_query)
  
  # Daily activity
  daily_query <- paste0("
    SELECT DATE(timestamp) as date, COUNT(*) as changes 
    FROM audit_log 
    WHERE DATE(timestamp) >= '", cutoff_date, "' 
    GROUP BY DATE(timestamp) 
    ORDER BY date DESC
  ")
  stats$daily_activity <- dbGetQuery(con, daily_query)
  
  return(stats)
}

#' Helper function for null coalescing
`%||%` <- function(x, y) if(is.null(x)) y else x

#' Manual WAL checkpoint (optional - for advanced control)
checkpoint_wal <- function(con) {
  result <- dbGetQuery(con, "PRAGMA wal_checkpoint(FULL)")
  return(result)
}

#' Get WAL file info (for monitoring)
get_wal_info <- function(con) {
  info <- list()
  info$journal_mode <- dbGetQuery(con, "PRAGMA journal_mode")$journal_mode
  info$wal_checkpoint <- dbGetQuery(con, "PRAGMA wal_checkpoint")
  return(info)
}

# Usage examples for integration with your Shiny app:

#' Example: How to integrate with your existing functions
example_usage <- function() {
  # Replace your db_connect() call with:
  # con <- db_connect_with_wal("your_database.db")
  
  # Replace your data modification calls with:
  # insert_data_with_log(con, "students_registry", student_data, session$token)
  # update_data_with_log(con, "students_registry", updated_data, "id", student_id, session$token)
  # delete_data_with_log(con, "students_registry", "id", student_id, session$token)
  
  # Export logs periodically:
  # export_audit_logs(con, start_date = "2025-01-01", output_file = "monthly_audit.json")
  
  # Get audit statistics:
  # stats <- get_audit_stats(con, days_back = 30)
}

# For Shiny integration, you might want to add this to your server function:
# 
# # Periodic log export (every day at midnight or when app starts)
# observe({
#   invalidateLater(24 * 60 * 60 * 1000)  # 24 hours
#   export_audit_logs(con, start_date = Sys.Date() - 7)  # Export last 7 days
# })