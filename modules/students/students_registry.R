students_registry_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    fluidRow(
      column(12,
             actionButton(ns("add_student"), "Add New Student", 
                          class = "btn-primary", style = "margin-bottom: 15px;")
      )
    ),
    
    fluidRow(
      column(12,
             withSpinner(DT::dataTableOutput(ns("students_table")))
      )
    ),
    
    fluidRow(
      column(6,
             actionButton(ns("add_student_bottom"), "Add New Student", 
                          class = "btn-primary", style = "margin-bottom: 15px;")
      ),
      column(6,
             fileInput(ns("upload_file"), "Upload Students (CSV/Excel)", 
                       accept = c(".csv", ".xls", ".xlsx")),
             actionButton(ns("process_upload"), "Append to Database", class = "btn-success")
      )
    ),
    
    # Add/Edit Student Modal
    create_student_modal(ns)
  )
}

students_registry_server <- function(id, con, shared_values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Local reactive values
    values <- reactiveValues(
      editing_id = NULL,
      uploaded_data = NULL
    )
    
    # Students table
    output$students_table <- DT::renderDataTable({
      req(shared_values$students_data)
      render_students_table(shared_values$students_data)
    })
    
    # File upload handling
    observeEvent(input$upload_file, {
      values$uploaded_data <- handle_file_upload(input$upload_file)
    })
    
    # Process uploaded data
    observeEvent(input$process_upload, {
      req(values$uploaded_data)
      process_student_upload(con, values$uploaded_data, shared_values)
      values$uploaded_data <- NULL
    })
    
    # Handle row selection for editing
    observeEvent(input$students_table_rows_selected, {
      if (length(input$students_table_rows_selected) > 0) {
        handle_student_selection(input$students_table_rows_selected, 
                                 shared_values$students_data, 
                                 values, session)
      }
    })
    
    # Handle add new student (both buttons)
    observeEvent(input$add_student, {
      open_student_modal(values, session, mode = "add")
    })
    
    observeEvent(input$add_student_bottom, {
      open_student_modal(values, session, mode = "add")
    })
    
    # Handle save student
    observeEvent(input$save_student, {
      save_student_data(input, con, values, shared_values, session)
    })
    
    # Handle cancel
    observeEvent(input$cancel_student, {
      toggleModal(session, "student_modal", toggle = "close")
    })
  })
}
