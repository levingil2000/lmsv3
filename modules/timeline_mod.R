# timeline_ui.R

library(shiny)
library(DT)
library(dplyr)
library(RSQLite)

timeline_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Event Timeline Management"),
    
    tabsetPanel(
      # Events Management Tab
      tabPanel("Events Management",
               br(),
               fluidRow(
                 # Add/Edit Form
                 column(4,
                        wellPanel(
                          h4("Add/Edit Event"),
                          
                          # Hidden field for edit mode
                          textInput(ns("selected_rowid"), "", value = "", placeholder = ""),
                          
                          textInput(ns("event_id"), "Event ID", placeholder = "e.g., EVT001"),
                          dateInput(ns("event_date"), "Event Date", value = Sys.Date()),
                          textAreaInput(ns("event_description"), "Description", rows = 3),
                          textInput(ns("event_persons_involved"), "Persons Involved"),
                          selectInput(ns("event_status"), "Status",
                                      choices = c("Pending", "In Progress", "Completed", "Cancelled")),
                          textAreaInput(ns("event_remarks"), "Remarks", rows = 2),
                          
                          br(),
                          actionButton(ns("add_event"), "Add Event", class = "btn-primary"),
                          actionButton(ns("update_event"), "Update Event", class = "btn-warning"),
                          actionButton(ns("clear_form"), "Clear", class = "btn-secondary")
                        )
                 ),
                 
                 # Events Table
                 column(8,
                        wellPanel(
                          div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                              h4("Events List"),
                              downloadButton(ns("download_csv"), "Download CSV", class = "btn-success")
                          ),
                          
                          DT::dataTableOutput(ns("events_table")),
                          
                          br(),
                          actionButton(ns("delete_selected"), "Delete Selected", class = "btn-danger")
                        )
                 )
               )
      ),
      
      # Timeline Tab
      tabPanel("Timeline",
               br(),
               fluidRow(
                 column(12,
                        wellPanel(
                          h4("Events Timeline", style = "text-align: center;"),
                          htmlOutput(ns("timeline"))
                        )
                 )
               )
      )
    )
  )
}

timeline_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize database table
    observe({
      tryCatch({
        dbExecute(con, "
          CREATE TABLE IF NOT EXISTS events (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            event_id TEXT UNIQUE,
            event_date DATE,
            event_description TEXT,
            event_persons_involved TEXT,
            event_status TEXT,
            event_remarks TEXT,
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP
          )
        ")
      }, error = function(e) {
        showNotification(paste("Database error:", e$message), type = "error")
      })
    })
    
    # Reactive to load events
    events_data <- reactive({
      input$add_event
      input$update_event
      input$delete_selected
      
      tryCatch({
        dbGetQuery(con, "SELECT * FROM events ORDER BY event_date DESC")
      }, error = function(e) {
        showNotification(paste("Error loading events:", e$message), type = "error")
        data.frame()
      })
    })
    
    # Events table
    output$events_table <- DT::renderDataTable({
      data <- events_data()
      if(nrow(data) == 0) return(data.frame())
      
      # Display table without internal ID
      display_data <- data %>%
        select(event_id, event_date, event_description, event_persons_involved, 
               event_status, event_remarks) %>%
        rename(
          "Event ID" = event_id,
          "Date" = event_date,
          "Description" = event_description,
          "Persons Involved" = event_persons_involved,
          "Status" = event_status,
          "Remarks" = event_remarks
        )
      
      DT::datatable(display_data,
                    selection = 'single',
                    options = list(
                      pageLength = 10,
                      scrollX = TRUE
                    ),
                    rownames = FALSE
      )
    }, server = FALSE)
    
    # Add event
    observeEvent(input$add_event, {
      req(input$event_id, input$event_date, input$event_description, input$event_status)
      
      tryCatch({
        dbExecute(con, "
          INSERT INTO events (event_id, event_date, event_description, 
                            event_persons_involved, event_status, event_remarks)
          VALUES (?, ?, ?, ?, ?, ?)
        ", params = list(
          input$event_id,
          as.character(input$event_date),
          input$event_description,
          input$event_persons_involved,
          input$event_status,
          input$event_remarks
        ))
        
        showNotification("Event added successfully!", type = "success")
        clear_form()
        
      }, error = function(e) {
        showNotification(paste("Error adding event:", e$message), type = "error")
      })
    })
    
    # Load selected row for editing
    observeEvent(input$events_table_rows_selected, {
      if(length(input$events_table_rows_selected) > 0) {
        data <- events_data()
        selected_row <- input$events_table_rows_selected
        event <- data[selected_row, ]
        
        updateTextInput(session, "selected_rowid", value = as.character(event$id))
        updateTextInput(session, "event_id", value = event$event_id)
        updateDateInput(session, "event_date", value = as.Date(event$event_date))
        updateTextAreaInput(session, "event_description", value = event$event_description)
        updateTextInput(session, "event_persons_involved", value = event$event_persons_involved)
        updateSelectInput(session, "event_status", selected = event$event_status)
        updateTextAreaInput(session, "event_remarks", value = event$event_remarks)
      }
    })
    
    # Update event
    observeEvent(input$update_event, {
      req(input$selected_rowid, input$event_id, input$event_date, 
          input$event_description, input$event_status)
      
      tryCatch({
        dbExecute(con, "
          UPDATE events 
          SET event_id = ?, event_date = ?, event_description = ?, 
              event_persons_involved = ?, event_status = ?, event_remarks = ?
          WHERE id = ?
        ", params = list(
          input$event_id,
          as.character(input$event_date),
          input$event_description,
          input$event_persons_involved,
          input$event_status,
          input$event_remarks,
          as.integer(input$selected_rowid)
        ))
        
        showNotification("Event updated successfully!", type = "success")
        clear_form()
        
      }, error = function(e) {
        showNotification(paste("Error updating event:", e$message), type = "error")
      })
    })
    
    # Delete selected event
    observeEvent(input$delete_selected, {
      if(length(input$events_table_rows_selected) > 0) {
        data <- events_data()
        selected_row <- input$events_table_rows_selected
        event_to_delete <- data[selected_row, ]
        
        showModal(modalDialog(
          title = "Confirm Delete",
          paste("Are you sure you want to delete event:", event_to_delete$event_id, "?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_delete"), "Delete", class = "btn-danger")
          )
        ))
      } else {
        showNotification("Please select an event to delete", type = "warning")
      }
    })
    
    # Confirm delete
    observeEvent(input$confirm_delete, {
      if(length(input$events_table_rows_selected) > 0) {
        data <- events_data()
        selected_row <- input$events_table_rows_selected
        event_to_delete <- data[selected_row, ]
        
        tryCatch({
          dbExecute(con, "DELETE FROM events WHERE id = ?", 
                    params = list(event_to_delete$id))
          
          showNotification("Event deleted successfully!", type = "success")
          clear_form()
          removeModal()
          
        }, error = function(e) {
          showNotification(paste("Error deleting event:", e$message), type = "error")
        })
      }
    })
    
    # Clear form function
    clear_form <- function() {
      updateTextInput(session, "selected_rowid", value = "")
      updateTextInput(session, "event_id", value = "")
      updateDateInput(session, "event_date", value = Sys.Date())
      updateTextAreaInput(session, "event_description", value = "")
      updateTextInput(session, "event_persons_involved", value = "")
      updateSelectInput(session, "event_status", selected = "Pending")
      updateTextAreaInput(session, "event_remarks", value = "")
    }
    
    # Clear form button
    observeEvent(input$clear_form, {
      clear_form()
    })
    
    # Download CSV
    output$download_csv <- downloadHandler(
      filename = function() {
        paste("events_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- events_data()
        if(nrow(data) > 0) {
          export_data <- data %>%
            select(-id, -created_at) %>%
            rename(
              "Event ID" = event_id,
              "Date" = event_date,
              "Description" = event_description,
              "Persons Involved" = event_persons_involved,
              "Status" = event_status,
              "Remarks" = event_remarks
            )
          write.csv(export_data, file, row.names = FALSE)
        }
      }
    )
    
    # Timeline visualization as simple text output
    output$timeline <- renderText({
      data <- events_data()
      
      if(nrow(data) == 0) {
        return("No events to display.")
      }
      
      # Sort by date chronologically (oldest first)
      timeline_data <- data %>%
        arrange(event_date)
      
      # Create text timeline
      timeline_text <- ""
      
      for(i in 1:nrow(timeline_data)) {
        event <- timeline_data[i, ]
        
        timeline_text <- paste0(timeline_text,
                                "=====================================\n",
                                "EVENT ID: ", event$event_id, "\n",
                                "DATE: ", format(as.Date(event$event_date), "%B %d, %Y"), "\n",
                                "STATUS: ", event$event_status, "\n",
                                "DESCRIPTION: ", event$event_description, "\n"
        )
        
        # Add persons involved if not empty
        if(!is.na(event$event_persons_involved) && event$event_persons_involved != "") {
          timeline_text <- paste0(timeline_text,
                                  "PERSONS INVOLVED: ", event$event_persons_involved, "\n"
          )
        }
        
        # Add remarks if not empty
        if(!is.na(event$event_remarks) && event$event_remarks != "") {
          timeline_text <- paste0(timeline_text,
                                  "REMARKS: ", event$event_remarks, "\n"
          )
        }
        
        timeline_text <- paste0(timeline_text, "\n")
      }
      
      return(timeline_text)
    })
  })
}

# Usage example:
# In your main app UI: timeline_ui("timeline")
# In your main app server: timeline_server("timeline", con)