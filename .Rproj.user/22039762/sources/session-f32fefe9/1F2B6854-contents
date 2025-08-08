# Assessment Tracker Module

assessment_tracker_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1("Assessment Dashboard", style = "color: #2c3e50; margin-bottom: 30px;"),
    
    # Filters Row
    fluidRow(
      column(4,
             selectInput(ns("filter_subject"), "Filter by Subject:",
                         choices = NULL, selected = NULL)
      ),
      column(4,
             selectInput(ns("filter_grade"), "Filter by Grade Level:",
                         choices = NULL, selected = NULL)
      ),
      column(4,
             selectInput(ns("filter_competency"), "Filter by Learning Competency:",
                         choices = NULL, selected = NULL)
      )
    ),
    
    br(),
    
    # Summary Cards
    fluidRow(
      column(3,
             div(class = "value-box",
                 h3(textOutput(ns("total_assessments"))),
                 p("Total Assessments")
             )
      ),
      column(3,
             div(class = "value-box",
                 h3(textOutput(ns("avg_pre_score"))),
                 p("Avg Pre-Score")
             )
      ),
      column(3,
             div(class = "value-box",
                 h3(textOutput(ns("avg_post_score"))),
                 p("Avg Post-Score")
             )
      ),
      column(3,
             div(class = "value-box",
                 h3(textOutput(ns("improvement_rate"))),
                 p("Improvement %")
             )
      )
    ),
    
    br(),
    
    # Main Charts Row
    fluidRow(
      column(6,
             wellPanel(
               h4("Pre-Test vs Post-Test Comparison by Subject", style = "color: #2c3e50;"),
               withSpinner(plotlyOutput(ns("pretest_posttest_comparison")))
             )
      ),
      column(6,
             wellPanel(
               h4("Score Distribution", style = "color: #2c3e50;"),
               withSpinner(plotlyOutput(ns("score_distribution")))
             )
      )
    ),
    
    br(),
    
    # Detailed Analysis Row
    fluidRow(
      column(6,
             wellPanel(
               h4("Learning Competency Performance", style = "color: #2c3e50;"),
               withSpinner(plotlyOutput(ns("competency_performance")))
             )
      ),
      column(6,
             wellPanel(
               h4("Individual Student Progress", style = "color: #2c3e50;"),
               selectInput(ns("student_select"), "Select Student:",
                           choices = NULL, selected = NULL),
               withSpinner(plotlyOutput(ns("student_progress")))
             )
      )
    ),
    
    br(),
    
    # Data Table Row
    fluidRow(
      column(12,
             wellPanel(
               h4("Assessment Details", style = "color: #2c3e50;"),
               br(),
               actionButton(ns("add_assessment"), "Add New Assessment", 
                            class = "btn-primary", style = "margin-bottom: 15px;"),
               withSpinner(DT::dataTableOutput(ns("assessments_table")))
             )
      )
    ),
    
    # Add/Edit Assessment Modal
    bsModal("assessment_modal", "Assessment Information", ns("add_assessment"), size = "medium",
            fluidRow(
              column(6,
                     textInput(ns("assessment_id"), "Assessment ID:", ""),
                     selectInput(ns("student_LRN"), "Student (LRN):",
                                 choices = NULL, selected = NULL),
                     selectInput(ns("subject"), "Subject:",
                                 choices = c("Mathematics", "English", "Science", "Filipino", "Araling Panlipunan"),
                                 selected = NULL),
                     textInput(ns("learning_competency"), "Learning Competency:", "")
              ),
              column(6,
                     numericInput(ns("grade_level"), "Grade Level:", value = 7, min = 1, max = 12),
                     numericInput(ns("preassessment_score"), "Pre-Assessment Score:", value = 0, min = 0, max = 100),
                     numericInput(ns("postassessment_score"), "Post-Assessment Score:", value = 0, min = 0, max = 100)
              )
            ),
            
            footer = tagList(
              actionButton(ns("save_assessment"), "Save", class = "btn-primary"),
              actionButton(ns("delete_assessment"), "Delete", class = "btn-danger", style = "display: none;"),
              actionButton(ns("cancel_assessment"), "Cancel", class = "btn-default")
            )
    )
  )
}

assessment_tracker_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      assessments_data = NULL,
      filtered_data = NULL,
      editing_id = NULL,
      refresh = 0
    )
    
    # Load assessments data
    observe({
      values$refresh  # Dependency for refresh
      values$assessments_data <- dbGetQuery(con, "
        SELECT at.*, sr.student_name 
        FROM assessment_table at
        LEFT JOIN students_registry sr ON at.student_LRN = sr.LRN
      ")
    })
    
    # Update filter choices and student selection
    observe({
      req(values$assessments_data)
      
      # Subjects
      subjects <- unique(values$assessments_data$subject[!is.na(values$assessments_data$subject)])
      updateSelectInput(session, "filter_subject",
                        choices = c("All" = "", subjects))
      
      # Grade levels
      grades <- sort(unique(values$assessments_data$grade_level[!is.na(values$assessments_data$grade_level)]))
      updateSelectInput(session, "filter_grade", 
                        choices = c("All" = "", grades))
      
      # Learning competencies
      competencies <- unique(values$assessments_data$learning_competency[!is.na(values$assessments_data$learning_competency)])
      updateSelectInput(session, "filter_competency",
                        choices = c("All" = "", competencies))
      
      # Students for individual progress
      students <- unique(values$assessments_data$student_name[!is.na(values$assessments_data$student_name)])
      updateSelectInput(session, "student_select",
                        choices = c("Select Student" = "", students))
      
      # Students for modal (LRN)
      student_data <- dbGetQuery(con, "SELECT LRN, student_name FROM students_registry")
      student_choices <- setNames(student_data$LRN, paste(student_data$student_name, "(", student_data$LRN, ")"))
      updateSelectInput(session, "student_LRN",
                        choices = c("Select Student" = "", student_choices))
    })
    
    # Filter assessments data
    observe({
      req(values$assessments_data)
      
      filtered <- values$assessments_data
      
      # Apply filters
      if (!is.null(input$filter_subject) && input$filter_subject != "") {
        filtered <- filtered[filtered$subject == input$filter_subject, ]
      }
      
      if (!is.null(input$filter_grade) && input$filter_grade != "") {
        filtered <- filtered[filtered$grade_level == as.numeric(input$filter_grade), ]
      }
      
      if (!is.null(input$filter_competency) && input$filter_competency != "") {
        filtered <- filtered[filtered$learning_competency == input$filter_competency, ]
      }
      
      values$filtered_data <- filtered
    })
    
    # Summary Statistics
    output$total_assessments <- renderText({
      req(values$filtered_data)
      nrow(values$filtered_data)
    })
    
    output$avg_pre_score <- renderText({
      req(values$filtered_data)
      avg_score <- mean(values$filtered_data$preassessment_score, na.rm = TRUE)
      if (is.nan(avg_score)) avg_score <- 0
      round(avg_score, 1)
    })
    
    output$avg_post_score <- renderText({
      req(values$filtered_data)
      avg_score <- mean(values$filtered_data$postassessment_score, na.rm = TRUE)
      if (is.nan(avg_score)) avg_score <- 0
      round(avg_score, 1)
    })
    
    output$improvement_rate <- renderText({
      req(values$filtered_data)
      pre_avg <- mean(values$filtered_data$preassessment_score, na.rm = TRUE)
      post_avg <- mean(values$filtered_data$postassessment_score, na.rm = TRUE)
      
      if (is.nan(pre_avg) || is.nan(post_avg) || pre_avg == 0) {
        improvement <- 0
      } else {
        improvement <- ((post_avg - pre_avg) / pre_avg) * 100
      }
      
      paste0(round(improvement, 1), "%")
    })
    
    # Pre-test vs Post-test Comparison Chart
    output$pretest_posttest_comparison <- renderPlotly({
      req(values$filtered_data)
      
      comparison_data <- values$filtered_data %>%
        filter(!is.na(preassessment_score) & !is.na(postassessment_score)) %>%
        group_by(subject) %>%
        summarise(
          avg_pre = mean(preassessment_score, na.rm = TRUE),
          avg_post = mean(postassessment_score, na.rm = TRUE),
          .groups = 'drop'
        )
      
      if (nrow(comparison_data) > 0) {
        p <- plot_ly(comparison_data) %>%
          add_trace(x = ~subject, y = ~avg_pre, type = 'bar', name = 'Pre-Assessment',
                    marker = list(color = '#e74c3c')) %>%
          add_trace(x = ~subject, y = ~avg_post, type = 'bar', name = 'Post-Assessment',
                    marker = list(color = '#27ae60')) %>%
          layout(title = "",
                 xaxis = list(title = "Subject"),
                 yaxis = list(title = "Average Score"),
                 barmode = 'group')
        p
      } else {
        plotly_empty() %>%
          layout(title = "No assessment data available")
      }
    })
    
    # Score Distribution Chart
    output$score_distribution <- renderPlotly({
      req(values$filtered_data)
      
      # Create score ranges
      filtered_data_clean <- values$filtered_data %>%
        filter(!is.na(preassessment_score) | !is.na(postassessment_score))
      
      if (nrow(filtered_data_clean) > 0) {
        pre_scores <- filtered_data_clean$preassessment_score[!is.na(filtered_data_clean$preassessment_score)]
        post_scores <- filtered_data_clean$postassessment_score[!is.na(filtered_data_clean$postassessment_score)]
        
        p <- plot_ly() %>%
          add_histogram(x = pre_scores, name = "Pre-Assessment", 
                        marker = list(color = '#e74c3c', opacity = 0.7)) %>%
          add_histogram(x = post_scores, name = "Post-Assessment",
                        marker = list(color = '#27ae60', opacity = 0.7)) %>%
          layout(title = "",
                 xaxis = list(title = "Score"),
                 yaxis = list(title = "Frequency"),
                 barmode = 'overlay')
        p
      } else {
        plotly_empty() %>%
          layout(title = "No data available")
      }
    })
    
    # Learning Competency Performance Chart
    output$competency_performance <- renderPlotly({
      req(values$filtered_data)
      
      competency_data <- values$filtered_data %>%
        filter(!is.na(learning_competency) & !is.na(preassessment_score) & !is.na(postassessment_score)) %>%
        group_by(learning_competency) %>%
        summarise(
          avg_improvement = mean(postassessment_score - preassessment_score, na.rm = TRUE),
          count = n(),
          .groups = 'drop'
        ) %>%
        arrange(desc(avg_improvement)) %>%
        head(10)
      
      if (nrow(competency_data) > 0) {
        p <- plot_ly(competency_data, 
                     x = ~reorder(learning_competency, avg_improvement), 
                     y = ~avg_improvement,
                     type = 'bar',
                     marker = list(color = '#3498db'),
                     hovertemplate = '<b>%{x}</b><br>Avg Improvement: %{y}<br>Count: %{text}<extra></extra>',
                     text = ~count) %>%
          layout(title = "",
                 xaxis = list(title = "Learning Competency"),
                 yaxis = list(title = "Average Score Improvement"))
        p
      } else {
        plotly_empty() %>%
          layout(title = "No data available")
      }
    })
    
    # Individual Student Progress Chart
    output$student_progress <- renderPlotly({
      req(input$student_select)
      
      if (input$student_select != "") {
        student_data <- values$assessments_data %>%
          filter(student_name == input$student_select & !is.na(preassessment_score) & !is.na(postassessment_score))
        
        if (nrow(student_data) > 0) {
          p <- plot_ly(student_data) %>%
            add_trace(x = ~subject, y = ~preassessment_score, type = 'bar', name = 'Pre-Assessment',
                      marker = list(color = '#e74c3c')) %>%
            add_trace(x = ~subject, y = ~postassessment_score, type = 'bar', name = 'Post-Assessment',
                      marker = list(color = '#27ae60')) %>%
            layout(title = "",
                   xaxis = list(title = "Subject"),
                   yaxis = list(title = "Score"),
                   barmode = 'group')
          p
        } else {
          plotly_empty() %>%
            layout(title = "No assessment data for selected student")
        }
      } else {
        plotly_empty() %>%
          layout(title = "Please select a student")
      }
    })
    
    # Assessments Table
    output$assessments_table <- DT::renderDataTable({
      req(values$filtered_data)
      
      display_data <- values$filtered_data %>%
        select(assessment_id, student_name, subject, learning_competency, grade_level, 
               preassessment_score, postassessment_score) %>%
        mutate(
          improvement = ifelse(!is.na(postassessment_score) & !is.na(preassessment_score),
                               round(postassessment_score - preassessment_score, 1), NA)
        )
      
      DT::datatable(display_data,
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE,
                    selection = "single") %>%
        DT::formatStyle(columns = "improvement",
                        backgroundColor = DT::styleInterval(c(-10, 0, 10), 
                                                            c("#ffebee", "#fff3e0", "#f3e5f5", "#e8f5e8")))
    })
    
    # Handle row selection for editing
    observeEvent(input$assessments_table_rows_selected, {
      if (length(input$assessments_table_rows_selected) > 0) {
        selected_row <- input$assessments_table_rows_selected
        assessment_data <- values$filtered_data[selected_row, ]
        
        values$editing_id <- assessment_data$id
        
        # Populate modal fields
        updateTextInput(session, "assessment_id", value = assessment_data$assessment_id)
        updateSelectInput(session, "student_LRN", selected = assessment_data$student_LRN)
        updateSelectInput(session, "subject", selected = assessment_data$subject)
        updateTextInput(session, "learning_competency", value = assessment_data$learning_competency)
        updateNumericInput(session, "grade_level", value = assessment_data$grade_level)
        updateNumericInput(session, "preassessment_score", value = ifelse(is.na(assessment_data$preassessment_score), 0, assessment_data$preassessment_score))
        updateNumericInput(session, "postassessment_score", value = ifelse(is.na(assessment_data$postassessment_score), 0, assessment_data$postassessment_score))
        
        # Show delete button
        shinyjs::show("delete_assessment")
        
        toggleModal(session, "assessment_modal", toggle = "open")
      }
    })
    
    # Handle add new assessment
    observeEvent(input$add_assessment, {
      values$editing_id <- NULL
      
      # Generate new assessment ID
      assessment_id <- paste0("ASS_", format(Sys.Date(), "%Y%m%d"), "_", sprintf("%04d", sample(1:9999, 1)))
      
      # Clear modal fields
      updateTextInput(session, "assessment_id", value = assessment_id)
      updateSelectInput(session, "student_LRN", selected = "")
      updateSelectInput(session, "subject", selected = "")
      updateTextInput(session, "learning_competency", value = "")
      updateNumericInput(session, "grade_level", value = 7)
      updateNumericInput(session, "preassessment_score", value = 0)
      updateNumericInput(session, "postassessment_score", value = 0)
      
      # Hide delete button
      shinyjs::hide("delete_assessment")
    })
    
    # Handle save assessment
    observeEvent(input$save_assessment, {
      req(input$assessment_id, input$student_LRN, input$subject)
      
      assessment_data <- data.frame(
        assessment_id = input$assessment_id,
        student_LRN = input$student_LRN,
        subject = input$subject,
        learning_competency = input$learning_competency,
        grade_level = input$grade_level,
        preassessment_score = input$preassessment_score,
        postassessment_score = input$postassessment_score,
        stringsAsFactors = FALSE
      )
      
      if (is.null(values$editing_id)) {
        # Insert new assessment
        result <- insert_data(con, "assessment_table", assessment_data)
      } else {
        # Update existing assessment
        result <- update_data(con, "assessment_table", assessment_data, "id", values$editing_id)
      }
      
      if (result$success) {
        showNotification(result$message, type = "message")
        values$refresh <- values$refresh + 1
        toggleModal(session, "assessment_modal", toggle = "close")
      } else {
        showNotification(result$message, type = "error")
      }
    })
    
    # Handle delete assessment
    observeEvent(input$delete_assessment, {
      req(values$editing_id)
      
      showModal(modalDialog(
        title = "Confirm Deletion",
        "Are you sure you want to delete this assessment? This action cannot be undone.",
        footer = tagList(
          actionButton(ns("confirm_delete"), "Yes, Delete", class = "btn-danger"),
          modalButton("Cancel")
        )
      ))
    })
    
    # Handle confirm delete
    observeEvent(input$confirm_delete, {
      req(values$editing_id)
      
      result <- delete_data(con, "assessment_table", "id", values$editing_id)
      
      if (result$success) {
        showNotification(result$message, type = "message")
        values$refresh <- values$refresh + 1
        removeModal()
        toggleModal(session, "assessment_modal", toggle = "close")
      } else {
        showNotification(result$message, type = "error")
      }
    })
    
    # Handle cancel
    observeEvent(input$cancel_assessment, {
      toggleModal(session, "assessment_modal", toggle = "close")
    })
  })
}