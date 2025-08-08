# Facilities Module

facilities_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1("Facilities Management", style = "color: #2c3e50; margin-bottom: 30px;"),
    
    fluidRow(
      column(12,
             actionButton(ns("add_facility"), "Add New Facility", 
                          class = "btn-primary", style = "margin-bottom: 15px;")
      )
    ),
    
    # Filters Row
    fluidRow(
      column(3,
             selectInput(ns("filter_status"), "Filter by Status:",
                         choices = c("All" = "", "Available", "In Use", "Under Maintenance", "Damaged"),
                         selected = "")
      ),
      column(3,
             selectInput(ns("filter_type"), "Filter by Type:",
                         choices = NULL, selected = NULL)
      ),
      column(6,
             searchInput(ns("search_facilities"), "Search Facilities:",
                         placeholder = "Search by name or description...")
      )
    ),
    
    br(),
    
    # Summary Cards
    fluidRow(
      column(3,
             div(class = "value-box",
                 h3(textOutput(ns("total_facilities"))),
                 p("Total Facilities")
             )
      ),
      column(3,
             div(class = "value-box",
                 h3(textOutput(ns("available_facilities"))),
                 p("Available")
             )
      ),
      column(3,
             div(class = "value-box",
                 h3(textOutput(ns("in_use_facilities"))),
                 p("In Use")
             )
      ),
      column(3,
             div(class = "value-box",
                 h3(textOutput(ns("total_value"))),
                 p("Total Value")
             )
      )
    ),
    
    br(),
    
    # Facilities Table
    fluidRow(
      column(12,
             withSpinner(DT::dataTableOutput(ns("facilities_table")))
      )
    ),
    
    # Add/Edit Facility Modal
    bsModal("facility_modal", "Facility Information", ns("add_facility"), size = "medium",
            fluidRow(
              column(6,
                     textInput(ns("FR_ID"), "Facility ID:", ""),
                     textInput(ns("facility_name"), "Facility Name:", ""),
                     textAreaInput(ns("description"), "Description:", "", rows = 3),
                     selectInput(ns("status"), "Status:",
                                 choices = c("Available", "In Use", "Under Maintenance", "Damaged"),
                                 selected = "Available")
              ),
              column(6,
                     textInput(ns("facility_type"), "Type:", ""),
                     numericInput(ns("amount"), "Amount/Value:", value = 0, min = 0)
              )
            ),
            
            footer = tagList(
              actionButton(ns("save_facility"), "Save", class = "btn-primary"),
              actionButton(ns("delete_facility"), "Delete", class = "btn-danger", style = "display: none;"),
              actionButton(ns("cancel_facility"), "Cancel", class = "btn-default")
            )
    )
  )
}

facilities_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      facilities_data = NULL,
      filtered_data = NULL,
      editing_id = NULL,
      refresh = 0
    )
    
    # Load facilities data
    observe({
      values$refresh  # Dependency for refresh
      values$facilities_data <- get_table_data(con, "facilities_table")
    })
    
    # Update filter choices
    observe({
      req(values$facilities_data)
      
      # Types
      types <- unique(values$facilities_data$type[!is.na(values$facilities_data$type)])
      updateSelectInput(session, "filter_type",
                        choices = c("All" = "", types))
    })
    
    # Filter facilities data
    observe({
      req(values$facilities_data)
      
      filtered <- values$facilities_data
      
      # Apply status filter
      if (!is.null(input$filter_status) && input$filter_status != "") {
        filtered <- filtered[filtered$status == input$filter_status, ]
      }
      
      # Apply type filter
      if (!is.null(input$filter_type) && input$filter_type != "") {
        filtered <- filtered[filtered$type == input$filter_type, ]
      }
      
      # Apply search filter
      if (!is.null(input$search_facilities) && input$search_facilities != "") {
        search_term <- tolower(input$search_facilities)
        filtered <- filtered[
          grepl(search_term, tolower(filtered$name)) | 
            grepl(search_term, tolower(filtered$description)),
        ]
      }
      
      values$filtered_data <- filtered
    })
    
    # Summary statistics
    output$total_facilities <- renderText({
      req(values$filtered_data)
      nrow(values$filtered_data)
    })
    
    output$available_facilities <- renderText({
      req(values$filtered_data)
      sum(values$filtered_data$status == "Available", na.rm = TRUE)
    })
    
    output$in_use_facilities <- renderText({
      req(values$filtered_data)
      sum(values$filtered_data$status == "In Use", na.rm = TRUE)
    })
    
    output$total_value <- renderText({
      req(values$filtered_data)
      total <- sum(values$filtered_data$amount, na.rm = TRUE)
      paste("₱", format(total, big.mark = ",", nsmall = 2))
    })
    
    # Facilities table
    output$facilities_table <- DT::renderDataTable({
      req(values$filtered_data)
      
      display_data <- values$filtered_data %>%
        select(FR_ID, name, description, status, type, amount) %>%
        mutate(amount = ifelse(is.na(amount), 0, amount))
      
      DT::datatable(display_data,
                    options = list(
                      pageLength = 10, 
                      scrollX = TRUE,
                      columnDefs = list(
                        list(targets = c(5), render = DT::JS(
                          "function(data, type, row, meta) {",
                          "return '₱' + parseFloat(data).toLocaleString();",
                          "}"
                        ))
                      )
                    ),
                    rownames = FALSE,
                    selection = "single") %>%
        DT::formatStyle(columns = "status",
                        backgroundColor = DT::styleEqual(
                          c("Available", "In Use", "Under Maintenance", "Damaged"),
                          c("#d4edda", "#fff3cd", "#cce5ff", "#f8d7da")
                        ))
    })
    
    # Handle row selection for editing
    observeEvent(input$facilities_table_rows_selected, {
      if (length(input$facilities_table_rows_selected) > 0) {
        selected_row <- input$facilities_table_rows_selected
        facility_data <- values$filtered_data[selected_row, ]
        
        values$editing_id <- facility_data$id
        
        # Populate modal fields
        updateTextInput(session, "FR_ID", value = facility_data$FR_ID)
        updateTextInput(session, "facility_name", value = facility_data$name)
        updateTextAreaInput(session, "description", value = facility_data$description)
        updateSelectInput(session, "status", selected = facility_data$status)
        updateTextInput(session, "facility_type", value = facility_data$type)
        updateNumericInput(session, "amount", value = ifelse(is.na(facility_data$amount), 0, facility_data$amount))
        
        # Show delete button
        shinyjs::show("delete_facility")
        
        toggleModal(session, "facility_modal", toggle = "open")
      }
    })
    
    # Handle add new facility
    observeEvent(input$add_facility, {
      values$editing_id <- NULL
      
      # Generate new facility ID
      facility_id <- paste0("FAC_", format(Sys.Date(), "%Y%m%d"), "_", sprintf("%04d", sample(1:9999, 1)))
      
      # Clear modal fields
      updateTextInput(session, "FR_ID", value = facility_id)
      updateTextInput(session, "facility_name", value = "")
      updateTextAreaInput(session, "description", value = "")
      updateSelectInput(session, "status", selected = "Available")
      updateTextInput(session, "facility_type", value = "")
      updateNumericInput(session, "amount", value = 0)
      
      # Hide delete button
      shinyjs::hide("delete_facility")
    })
    
    # Handle save facility
    observeEvent(input$save_facility, {
      req(input$FR_ID, input$facility_name)
      
      facility_data <- data.frame(
        FR_ID = input$FR_ID,
        name = input$facility_name,
        description = input$description,
        status = input$status,
        type = input$facility_type,
        amount = input$amount,
        stringsAsFactors = FALSE
      )
      
      if (is.null(values$editing_id)) {
        # Insert new facility
        result <- insert_data(con, "facilities_table", facility_data)
      } else {
        # Update existing facility
        result <- update_data(con, "facilities_table", facility_data, "id", values$editing_id)
      }
      
      if (result$success) {
        showNotification(result$message, type = "message")
        values$refresh <- values$refresh + 1
        toggleModal(session, "facility_modal", toggle = "close")
      } else {
        showNotification(result$message, type = "error")
      }
    })
    
    # Handle delete facility
    observeEvent(input$delete_facility, {
      req(values$editing_id)
      
      showModal(modalDialog(
        title = "Confirm Deletion",
        "Are you sure you want to delete this facility? This action cannot be undone.",
        footer = tagList(
          actionButton(ns("confirm_delete"), "Yes, Delete", class = "btn-danger"),
          modalButton("Cancel")
        )
      ))
    })
    
    # Handle confirm delete
    observeEvent(input$confirm_delete, {
      req(values$editing_id)
      
      result <- delete_data(con, "facilities_table", "id", values$editing_id)
      
      if (result$success) {
        showNotification(result$message, type = "message")
        values$refresh <- values$refresh + 1
        removeModal()
        toggleModal(session, "facility_modal", toggle = "close")
      } else {
        showNotification(result$message, type = "error")
      }
    })
    
    # Handle cancel
    observeEvent(input$cancel_facility, {
      toggleModal(session, "facility_modal", toggle = "close")
    })
  })
}