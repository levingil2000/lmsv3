#enrollment tab
enroll_students_tab_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Enroll Students",
    value = "enrollment",
    icon = icon("chalkboard"),
    br(),
    fluidRow(
      box(
        title = "Enrollment Controls", 
        status = "info", 
        solidHeader = TRUE, 
        width = 4,
        h4("Step 1: Select Quarter and Subject"),
        selectInput(ns("quarter"), "Quarter:", 
                    choices = c("Q1", "Q2", "Q3", "Q4"), 
                    selected = "Q1"),
        selectInput(ns("subject"), "Subject:", 
                    choices = c("English" = "english", 
                                "Math" = "math", 
                                "Science" = "science"), 
                    selected = "english"),
        
        hr(),
        h4("Step 2: Filter Students"),
        selectInput(ns("grade_filter"), "Filter by Grade Level:", 
                    choices = c("All Grades" = "all"),
                    selected = "all"),
        selectInput(ns("section_filter"), "Filter by Section:", 
                    choices = c("All Sections" = "all"),
                    selected = "all"),
        
        hr(),
        h4("Step 3: Enroll Students"),
        actionButton(ns("enroll_btn"), "Enroll Selected Students", 
                     class = "btn-success", style = "width: 100%;"),
        br(), br(),
        actionButton(ns("refresh_data"), "Refresh Data", 
                     class = "btn-info", style = "width: 100%;")
      ),
      
      box(
        title = "Student Selection", 
        status = "primary", 
        solidHeader = TRUE, 
        width = 8,
        p("Select students to enroll in the chosen remedial class. Use filters to narrow down students by grade level and section."),
        DT::dataTableOutput(ns("enrollment_selection_table"))
      )
    ),
    
    fluidRow(
      box(
        title = "Current Enrollment Status", 
        status = "success", 
        solidHeader = TRUE, 
        width = 12,
        h4(textOutput(ns("enrollment_title"))),
        DT::dataTableOutput(ns("current_class_enrollment"))
      )
    )
  )
}

# =============================================================================
# SERVER MODULE
# =============================================================================

enroll_students_tab_server <- function(id, con, student_data, refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    
    # Update filter choices when student data changes
    observe({
      req(student_data())
      
      # Update grade level choices
      grades <- sort(unique(student_data()$grade_level))
      grade_choices <- c("All Grades" = "all")
      for (grade in grades) {
        grade_choices[paste("Grade", grade)] <- as.character(grade)
      }
      updateSelectInput(session, "grade_filter", choices = grade_choices)
      
      # Update section choices
      sections <- sort(unique(student_data()$section))
      section_choices <- c("All Sections" = "all")
      for (section in sections) {
        section_choices[section] <- section
      }
      updateSelectInput(session, "section_filter", choices = section_choices)
    })
    
    # Filtered student data based on grade and section
    filtered_students <- reactive({
      req(student_data())
      
      data <- student_data()
      
      # Apply grade filter
      if (input$grade_filter != "all") {
        data <- data %>% filter(grade_level == as.numeric(input$grade_filter))
      }
      
      # Apply section filter
      if (input$section_filter != "all") {
        data <- data %>% filter(section == input$section_filter)
      }
      
      return(data)
    })
    
    # Selection table for enrollment - shows filtered students with full sorting
    output$enrollment_selection_table <- DT::renderDataTable({
      req(filtered_students())
      
      display_data <- filtered_students() %>%
        select(student_id, student_name, grade_level, section, Total_score_per,
               RMA_before_math_proficiency, before_reading_proficiency, reading_accuracy)
      
      DT::datatable(
        display_data,
        selection = 'multiple',
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
          order = list(list(2, 'asc'))  # Default sort by student_name
        ),
        caption = paste("Select students for", toupper(input$subject), "remedial class in", input$quarter)
      ) %>%
        DT::formatRound(columns = c("Total_score_per"), digits = 1)
    })
    
    # Handle enrollment
    observeEvent(input$enroll_btn, {
      selected_rows <- input$enrollment_selection_table_rows_selected
      
      if (length(selected_rows) > 0) {
        # Extract individual student IDs properly
        selected_student_data <- filtered_students()[selected_rows, ]
        selected_students <- selected_student_data$student_id
        
        # Set initial attendance to 0 (fixed)
        initial_attendance <- 0
        
        # Debug: Print what we're getting
        cat("Selected rows:", selected_rows, "\n")
        cat("Selected student IDs:", selected_students, "\n")
        cat("Initial attendance:", initial_attendance, "\n")
        
        result <- enroll_students(con, selected_students, input$quarter, input$subject, initial_attendance)
        
        if (result$success > 0) {
          showNotification(
            paste("Successfully enrolled", result$success, "students in", 
                  toupper(input$subject), "for", input$quarter),
            type = "message"
          )
        }
        
        if (result$duplicates > 0) {
          showNotification(
            paste(result$duplicates, "students were already enrolled in this class"),
            type = "warning"
          )
        }
        
        if (result$success == 0 && result$duplicates == 0) {
          showNotification("No students could be enrolled. Please check the data.", type = "error")
        }
        
        # Trigger refresh in parent module
        refresh_trigger(Sys.time())
      } else {
        showNotification("Please select students to enroll", type = "warning")
      }
    })
    
    # Current enrollment status
    current_enrollment <- reactive({
      refresh_trigger()  # Dependency to trigger refresh
      get_enrollment_data(con, input$quarter, input$subject)
    })
    
    output$enrollment_title <- renderText({
      enrolled_count <- nrow(current_enrollment())
      paste("Currently Enrolled in", toupper(input$subject), "-", input$quarter, ":", enrolled_count, "students")
    })
    
    output$current_class_enrollment <- DT::renderDataTable({
      enrollment_data <- current_enrollment()
      
      if (nrow(enrollment_data) > 0) {
        display_data <- enrollment_data %>%
          select(student_name, grade_level, section, enrollment_date, total_attendance, status)
        
        DT::datatable(
          display_data,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            searching = TRUE
          )
        )
      } else {
        empty_df <- data.frame(Message = "No students currently enrolled in this class")
        DT::datatable(
          empty_df,
          options = list(searching = FALSE, paging = FALSE, info = FALSE)
        )
      }
    })
    
    # Refresh data
    observeEvent(input$refresh_data, {
      refresh_trigger(Sys.time())
      showNotification("Data refreshed successfully", type = "message")
    })
    
    # Return reactive values that parent module might need
    return(list(
      quarter = reactive(input$quarter),
      subject = reactive(input$subject)
    ))
  })
}