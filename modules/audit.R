db_path <- "lms_database.sqlite"  # adjust your path
con <- NULL

# Startup configuration and logging initialization
initialize_app <- function() {
  cat("=== Education Management System Startup ===\n")
  cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Connect to database with WAL logging enabled
  con <<- db_connect_with_wal(db_path)
  
  # Check WAL status
  wal_status <- get_wal_info(con)
  cat("WAL Status:", wal_status$journal_mode, "\n")
  
  # Create startup log entry
  startup_log <- list(
    timestamp = Sys.time(),
    event = "APP_STARTUP",
    wal_enabled = wal_status$journal_mode == "wal",
    database_path = db_path,
    tables_count = length(dbListTables(con))
  )
  
  # Save startup log to file
  startup_log_file <- paste0("logs/startup_", format(Sys.Date(), "%Y%m%d"), ".json")
  dir.create("logs", showWarnings = FALSE, recursive = TRUE)
  
  if(file.exists(startup_log_file)) {
    existing_logs <- fromJSON(startup_log_file)
    all_logs <- c(existing_logs, list(startup_log))
  } else {
    all_logs <- list(startup_log)
  }
  
  write(toJSON(all_logs, pretty = TRUE, auto_unbox = TRUE), startup_log_file)
  cat("Startup logged to:", startup_log_file, "\n")
  
  # Export any pending audit logs from previous sessions
  export_result <- export_audit_logs(con, 
                                     start_date = Sys.Date() - 7, 
                                     output_file = paste0("logs/audit_weekly_", 
                                                          format(Sys.Date(), "%Y%m%d"), ".json"))
  
  cat("Audit export:", export_result$message, "\n")
  
  # Show audit statistics
  audit_stats <- get_audit_stats(con, days_back = 7)
  cat("Recent activity (7 days):", audit_stats$total_changes, "database changes\n")
  
  cat("=== Initialization Complete ===\n\n")
  
  return(con)
}

# Enhanced database connection wrapper with session tracking
get_db_connection <- function(session_id = NULL) {
  if(is.null(con) || !dbIsValid(con)) {
    con <<- initialize_app()
  }
  return(con)
}

# Wrapper functions that include session tracking
# Use these in your Shiny server instead of direct database calls

insert_student_with_log <- function(student_data, session) {
  con <- get_db_connection()
  result <- insert_data_with_log(con, "students_registry", student_data, session$token)
  return(result)
}

update_student_with_log <- function(student_data, student_id, session) {
  con <- get_db_connection()
  result <- update_data_with_log(con, "students_registry", student_data, "id", student_id, session$token)
  return(result)
}

delete_student_with_log <- function(student_id, session) {
  con <- get_db_connection()
  result <- delete_data_with_log(con, "students_registry", "id", student_id, session$token)
  return(result)
}

# Similar wrappers for other tables
insert_teacher_with_log <- function(teacher_data, session) {
  con <- get_db_connection()
  result <- insert_data_with_log(con, "teacher_registry", teacher_data, session$token)
  return(result)
}

insert_assessment_with_log <- function(assessment_data, session) {
  con <- get_db_connection()
  result <- insert_data_with_log(con, "assessment_table", assessment_data, session$token)
  return(result)
}

# Add more wrappers for other tables as needed...

# Shiny UI (basic structure - adapt to your existing UI)
ui <- dashboardPage(
  dashboardHeader(title = "Education Management System"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Students", tabName = "students", icon = icon("users")),
      menuItem("Teachers", tabName = "teachers", icon = icon("chalkboard-teacher")),
      menuItem("Audit Logs", tabName = "audit", icon = icon("history"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("totalStudents"),
                valueBoxOutput("totalTeachers"),
                valueBoxOutput("recentChanges")
              )
      ),
      
      # Students tab (your existing content)
      tabItem(tabName = "students",
              # Your existing student management UI
              h2("Student Management")
              # ... your existing UI elements
      ),
      
      # Audit logs tab
      tabItem(tabName = "audit",
              fluidRow(
                box(title = "Audit Log Controls", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(3, dateInput("audit_start_date", "Start Date", value = Sys.Date() - 7)),
                      column(3, dateInput("audit_end_date", "End Date", value = Sys.Date())),
                      column(3, br(), actionButton("export_audit", "Export Audit Logs", class = "btn-success")),
                      column(3, br(), actionButton("refresh_audit", "Refresh", class = "btn-info"))
                    )
                )
              ),
              fluidRow(
                box(title = "Recent Database Changes", status = "info", solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("audit_table")
                )
              ),
              fluidRow(
                box(title = "Activity Summary", status = "warning", solidHeader = TRUE, width = 6,
                    tableOutput("audit_summary")
                ),
                box(title = "Changes by Table", status = "success", solidHeader = TRUE, width = 6,
                    tableOutput("changes_by_table")
                )
              )
      )
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  # Initialize database connection on startup
  con <- initialize_app()
  
  # Reactive value to track audit log updates
  audit_trigger <- reactiveVal(0)
  
  # Dashboard value boxes
  output$totalStudents <- renderValueBox({
    stats <- get_dashboard_stats(con)
    valueBox(
      value = stats$total_students,
      subtitle = "Total Students",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$totalTeachers <- renderValueBox({
    stats <- get_dashboard_stats(con)
    valueBox(
      value = stats$total_teachers,
      subtitle = "Active Teachers",
      icon = icon("chalkboard-teacher"),
      color = "green"
    )
  })
  
  output$recentChanges <- renderValueBox({
    audit_stats <- get_audit_stats(con, days_back = 7)
    valueBox(
      value = audit_stats$total_changes,
      subtitle = "Changes (7 days)",
      icon = icon("history"),
      color = "yellow"
    )
  })
  
  # Audit log table
  output$audit_table <- DT::renderDataTable({
    audit_trigger()  # For reactivity
    
    query <- "
      SELECT 
        timestamp,
        table_name,
        operation,
        record_id,
        user_session
      FROM audit_log 
      ORDER BY timestamp DESC 
      LIMIT 100
    "
    audit_data <- dbGetQuery(con, query)
    
    DT::datatable(audit_data, 
                  options = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # Audit summary
  output$audit_summary <- renderTable({
    audit_stats <- get_audit_stats(con, days_back = 7)
    audit_stats$by_operation
  })
  
  # Changes by table
  output$changes_by_table <- renderTable({
    audit_stats <- get_audit_stats(con, days_back = 7)
    audit_stats$by_table
  })
  
  # Export audit logs button
  observeEvent(input$export_audit, {
    result <- export_audit_logs(con, 
                                start_date = input$audit_start_date,
                                end_date = input$audit_end_date,
                                output_file = paste0("logs/audit_export_", 
                                                     format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"))
    
    showNotification(result$message, type = "success", duration = 5)
  })
  
  # Refresh audit data
  observeEvent(input$refresh_audit, {
    audit_trigger(audit_trigger() + 1)
    showNotification("Audit data refreshed", type = "message", duration = 3)
  })
  
  # Example of using logged database operations
  # Replace your existing database calls with these logged versions
  
  # Example: When adding a new student
  observeEvent(input$add_student_button, {  # assuming you have this button
    student_data <- data.frame(
      student_name = input$student_name,
      grade_level = input$grade_level,
      section = input$section,
      LRN = input$student_lrn,
      # ... other fields
      stringsAsFactors = FALSE
    )
    
    result <- insert_student_with_log(student_data, session)
    
    if(result$success) {
      showNotification("Student added successfully", type = "success")
      audit_trigger(audit_trigger() + 1)  # Refresh audit display
    } else {
      showNotification(paste("Error:", result$message), type = "error")
    }
  })
  
  # Cleanup on session end
  session$onSessionEnded(function() {
    if(!is.null(con) && dbIsValid(con)) {
      # Final audit log export before closing
      export_audit_logs(con, 
                        start_date = Sys.Date(),
                        output_file = paste0("logs/session_end_", 
                                             format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"))
      
      dbDisconnect(con)
      cat("Database connection closed on session end\n")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# Optional: Add this to your global.R if you have one
onStop(function() {
  if(exists("con") && !is.null(con) && dbIsValid(con)) {
    # Export final audit logs
    export_audit_logs(con, 
                      start_date = Sys.Date(),
                      output_file = paste0("logs/app_shutdown_", 
                                           format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"))
    dbDisconnect(con)
    cat("App shutdown: Database connection closed and final audit exported\n")
  }
})