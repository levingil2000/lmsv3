# Student registry with update functionality and proper data refresh
student_registry_tab_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Student Registry",
    value = "registry",
    icon = icon("users"),
    br(),
    fluidRow(
      box(
        title = "Student Registry",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        p("Complete student registry with test scores and proficiency levels."),
        # Update buttons row
        fluidRow(
          column(6,
                 actionButton(
                   ns("soft_update"),
                   "Soft Update",
                   icon = icon("refresh"),
                   class = "btn-info",
                   style = "margin-right: 10px;"
                 ),
                 span("Updates existing entries and adds new students", style = "font-size: 12px; color: #666;")
          ),
          column(6,
                 actionButton(
                   ns("hard_update"),
                   "Hard Update",
                   icon = icon("exclamation-triangle"),
                   class = "btn-warning"
                 ),
                 span("Replaces ALL data (requires confirmation)", style = "font-size: 12px; color: #666;")
          )
        ),
        br(),
        # Status message
        uiOutput(ns("update_status")),
        br(),
        DT::dataTableOutput(ns("remedial_student_registry"))
      )
    )
  )
}

# =============================================================================
# SERVER MODULE
# =============================================================================
student_registry_tab_server <- function(id, student_data, con, refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for status messages
    status_message <- reactiveVal("")
    
    # Student Registry Table
    output$remedial_student_registry <- DT::renderDataTable({
      req(student_data())
      display_data <- student_data() %>%
        select(student_id, student_name, grade_level, section, 
               Total_score_per, RMA_before_math_proficiency, 
               before_reading_proficiency, starts_with("testq"))
      
      DT::datatable(
        display_data,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE
        ),
        caption = "Complete Student Registry"
      ) %>%
        DT::formatRound(columns = c("Total_score_per"), digits = 1)
    })
    
    # Status message output
    output$update_status <- renderUI({
      if (status_message() != "") {
        if (grepl("successfully|Success", status_message())) {
          div(class = "alert alert-success", status_message())
        } else if (grepl("Error|Failed", status_message())) {
          div(class = "alert alert-danger", status_message())
        } else {
          div(class = "alert alert-info", status_message())
        }
      }
    })
    
    # Function to read Excel file
    read_excel_data <- function() {
      tryCatch({
        if (!file.exists("full_student_registry.xlsx")) {
          stop("Excel file 'full_student_registry.xlsx' not found in root directory")
        }
        
        excel_data <- readxl::read_excel("full_student_registry.xlsx")
        
        # Remove student_id column if it exists in Excel (shouldn't happen but just in case)
        if ("student_id" %in% names(excel_data)) {
          excel_data <- excel_data %>% select(-student_id)
          message("Removed student_id column from Excel data (will be auto-generated)")
        }
        
        return(excel_data)
      }, error = function(e) {
        status_message(paste("Error reading Excel file:", e$message))
        return(NULL)
      })
    }
    
    # Soft Update - Updates existing entries and adds new ones
    observeEvent(input$soft_update, {
      status_message("Starting soft update...")
      
      excel_data <- read_excel_data()
      if (is.null(excel_data)) return()
      
      tryCatch({
        # Get existing student names from database
        existing_students <- DBI::dbGetQuery(con, "SELECT student_name FROM full_student_registry")$student_name
        
        # Separate existing and new students
        existing_updates <- excel_data %>%
          filter(student_name %in% existing_students)
        
        new_students <- excel_data %>%
          filter(!student_name %in% existing_students)
        
        updated_count <- 0
        added_count <- 0
        
        # Begin transaction for consistency
        DBI::dbBegin(con)
        
        # Update existing students
        if (nrow(existing_updates) > 0) {
          for (i in 1:nrow(existing_updates)) {
            student_name <- existing_updates$student_name[i]
            
            # Get columns to update (excluding student_name and student_id)
            update_cols <- names(existing_updates)[!names(existing_updates) %in% c("student_name", "student_id")]
            
            # Build the query and values more carefully
            set_parts <- paste0(update_cols, " = ?")
            set_clause <- paste(set_parts, collapse = ", ")
            query <- paste("UPDATE full_student_registry SET", set_clause, "WHERE student_name = ?")
            
            # Extract values as individual scalars
            values <- vector("list", length(update_cols) + 1)  # +1 for student_name
            
            for (j in seq_along(update_cols)) {
              col_name <- update_cols[j]
              values[[j]] <- as.character(existing_updates[i, col_name])
            }
            values[[length(values)]] <- as.character(student_name)  # WHERE clause value
            
            # Debug output (remove after testing)
            cat("Updating student:", student_name, "\n")
            cat("Query:", query, "\n")
            cat("Values length:", length(values), "\n")
            
            tryCatch({
              result <- DBI::dbExecute(con, query, values)
              if (result > 0) updated_count <- updated_count + 1
            }, error = function(e) {
              cat("Error updating student", student_name, ":", e$message, "\n")
              # Continue with next student rather than failing completely
            })
          }
        }
        
        # Add new students
        if (nrow(new_students) > 0) {
          DBI::dbWriteTable(con, "full_student_registry", new_students, 
                            append = TRUE, row.names = FALSE)
          added_count <- nrow(new_students)
        }
        
        # Commit transaction
        DBI::dbCommit(con)
        
        # Create status message
        status_parts <- c()
        if (updated_count > 0) status_parts <- c(status_parts, paste(updated_count, "students updated"))
        if (added_count > 0) status_parts <- c(status_parts, paste(added_count, "new students added"))
        
        if (length(status_parts) > 0) {
          status_message(paste("Soft update completed successfully.", paste(status_parts, collapse = ", "), "."))
        } else {
          status_message("No changes made - no matching or new students found.")
        }
        
        # CRITICAL: Trigger data refresh to sync with enrollment tab
        refresh_trigger(Sys.time())
        
      }, error = function(e) {
        # Rollback on error
        DBI::dbRollback(con)
        status_message(paste("Error during soft update:", e$message))
      })
    })
    
    # Hard Update - Replace all data (with confirmation)
    observeEvent(input$hard_update, {
      showModal(modalDialog(
        title = "Confirm Hard Update",
        HTML("<div class='alert alert-warning'>
               <strong>WARNING:</strong> This will delete ALL existing data in the student registry 
               and replace it with data from the Excel file. This action cannot be undone.
               <br><br>
               <strong>Note:</strong> This will also affect existing enrollments if student IDs change.
             </div>
             <p>Are you sure you want to proceed?</p>"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_hard_update"), "Yes, Replace All Data", 
                       class = "btn-danger")
        ),
        easyClose = FALSE
      ))
    })
    
    # Confirmed Hard Update
    observeEvent(input$confirm_hard_update, {
      removeModal()
      status_message("Starting hard update...")
      
      excel_data <- read_excel_data()
      if (is.null(excel_data)) return()
      
      tryCatch({
        # Begin transaction
        DBI::dbBegin(con)
        
        # Store existing enrollments temporarily (in case we need to preserve them)
        existing_enrollments <- DBI::dbGetQuery(con, "
          SELECT e.*, s.student_name 
          FROM remedial_enrollments e 
          LEFT JOIN full_student_registry s ON e.student_id = s.student_id
        ")
        
        # Delete all existing data
        DBI::dbExecute(con, "DELETE FROM full_student_registry")
        
        # Insert new data
        DBI::dbWriteTable(con, "full_student_registry", excel_data, 
                          append = TRUE, row.names = FALSE)
        
        # Try to reconnect enrollments by student name
        if (nrow(existing_enrollments) > 0) {
          new_student_mapping <- DBI::dbGetQuery(con, "SELECT student_id, student_name FROM full_student_registry")
          
          # Update enrollment student_ids based on name matching
          for (i in 1:nrow(existing_enrollments)) {
            old_enrollment <- existing_enrollments[i, ]
            new_student <- new_student_mapping[new_student_mapping$student_name == old_enrollment$student_name, ]
            
            if (nrow(new_student) == 1) {
              # Update the enrollment with new student_id
              DBI::dbExecute(con, "
                UPDATE remedial_enrollments 
                SET student_id = ? 
                WHERE enrollment_id = ?",
                             params = list(new_student$student_id, old_enrollment$enrollment_id))
            } else if (nrow(new_student) == 0) {
              # Student no longer exists, mark enrollment as inactive
              DBI::dbExecute(con, "
                UPDATE remedial_enrollments 
                SET status = 'Inactive' 
                WHERE enrollment_id = ?",
                             params = list(old_enrollment$enrollment_id))
            }
          }
        }
        
        # Commit transaction
        DBI::dbCommit(con)
        
        status_message(paste("Hard update completed successfully.", nrow(excel_data), 
                             "records inserted. Existing enrollments have been preserved where possible."))
        
        # CRITICAL: Trigger data refresh to sync with enrollment tab
        refresh_trigger(Sys.time())
        
      }, error = function(e) {
        # Rollback on error
        DBI::dbRollback(con)
        status_message(paste("Error during hard update:", e$message))
      })
    })
  })
}