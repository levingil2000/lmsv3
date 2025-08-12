### Enrollment Dashboard
# Source all helper files
source("modules/enrollment/enrollment_dashboard/ai_helpers.R")
source("modules/enrollment/enrollment_dashboard/graph_helpers.R") 
source("modules/enrollment/enrollment_dashboard/report_document_helpers.R")
source("modules/enrollment/enrollment_dashboard/auto_report_generation.R")
source("modules/enrollment/enrollment_dashboard/graphs_tables.R")

# DASHBOARD UI MODULE
# =============================================================================

dashboard_tab_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Dashboard",
    value = "dashboard",
    icon = icon("chart-line"),
    
    # Include parallax background and animations
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/aos/2.3.4/aos.css"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/aos/2.3.4/aos.js"),
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$script(src = "scripts.js")
    ),
    
    # Parallax background
    div(class = "parallax-container",
        div(class = "parallax-bg")
    ),
    
    br(),

    
    # Value Boxes Row with animations
    fluidRow(
      column(3,
             div(class = "value-box",
                 "data-aos" = "fade-up", "data-aos-delay" = "100",
                 valueBoxOutput(ns("total_enrolled"), width = 12)
             )
      ),
      column(3,
             div(class = "value-box",
                 "data-aos" = "fade-up", "data-aos-delay" = "200",
                 valueBoxOutput(ns("active_students"), width = 12)
             )
      ),
      column(3,
             div(class = "value-box",
                 "data-aos" = "fade-up", "data-aos-delay" = "300",
                 valueBoxOutput(ns("passed_students"), width = 12)
             )
      ),
      column(3,
             div(class = "value-box",
                 "data-aos" = "fade-up", "data-aos-delay" = "400",
                 valueBoxOutput(ns("avg_attendance"), width = 12)
             )
      )
    ),
    
    br(),
    
    # Charts Row with animations
    fluidRow(
      column(4,
             div(class = "chart-container",
                 "data-aos" = "fade-right", "data-aos-delay" = "500",
                 h4("Enrollment by Grade Level"),
                 withSpinner(plotlyOutput(ns("grade_distribution")), type = 8, color = "#3498db")
             )
      ),
      column(4,
             div(class = "chart-container",
                 "data-aos" = "fade-up", "data-aos-delay" = "600",
                 h4("Enrollment by Subject"),
                 withSpinner(plotlyOutput(ns("subject_distribution")), type = 8, color = "#f39c12")
             )
      ),
      column(4,
             div(class = "chart-container",
                 "data-aos" = "fade-left", "data-aos-delay" = "700",
                 h4("Average Sessions by Subject"),
                 withSpinner(plotlyOutput(ns("avg_sessions_chart")), type = 8, color = "#2ecc71")
             )
      )
    ),
    
    br(),
    
    # Class Lists Section with animations
    div("data-aos" = "zoom-in", "data-aos-delay" = "800",
        box(
          title = "📊 Detailed Class Enrollment Lists",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          p("View organized class lists grouped by subject, quarter, and grade level. Each table shows students enrolled in that specific class configuration with attendance and status information."),
          
          fluidRow(
            column(4, selectInput(ns("filter_quarter"), "Filter by Quarter:", 
                                  choices = c("All Quarters" = "all", "Q1", "Q2", "Q3", "Q4"), 
                                  selected = "all")),
            column(4, selectInput(ns("filter_subject"), "Filter by Subject:", 
                                  choices = c("All Subjects" = "all", "English" = "english", "Math" = "math", "Science" = "science"), 
                                  selected = "all")),
            column(4, selectInput(ns("filter_grade_level"), "Filter by Grade:", 
                                  choices = c("All Grades" = "all"), selected = "all"))
          ),
          
          hr(),
          withSpinner(uiOutput(ns("class_lists_output")), type = 8, color = "#e67e22")
        )
    ),
    
    # Reports Generation Section - ENHANCED
    div("data-aos" = "fade-down", "data-aos-delay" = "50",
        box(
          title = "Reports Generator",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          
          div(
            p(" Generate comprehensive AI-powered analysis reports with detailed insights, recommendations, and professional formatting."),
            p(icon("clock"), " Analysis takes 2-3 minutes for thorough examination of all program data."),
            p(icon("file-alt"), " Produces professional Word documents ready for stakeholder presentation.")
          ),
          
          fluidRow(
            column(6,
                   actionButton(ns("generate_report"), 
                                HTML("<i class='fas fa-magic'></i> Generate AI Analysis Report"), 
                                class = "btn-primary btn-lg", 
                                style = "width: 100%; background: linear-gradient(45deg, #3498db, #2980b9);")
            ),
            column(6,
                   conditionalPanel(
                     condition = "output.report_ready == true", ns = ns,
                     downloadButton(ns("download_report"), 
                                    HTML("<i class='fas fa-download'></i> Download Report"), 
                                    class = "btn-success btn-lg",
                                    style = "width: 100%; background: linear-gradient(45deg, #27ae60, #229954);")
                   )
            )
          ),
          
          br(),
          
          div(id = ns("report_status"), 
              style = "margin-top: 15px; display: none;",
              div(class = "progress",
                  div(class = "progress-bar progress-bar-striped active",
                      style = "width: 100%; background: linear-gradient(45deg, #e74c3c, #c0392b);",
                      HTML("<i class='fas fa-cog fa-spin'></i> AI Analysis in Progress..."))
              )
          )
        )
    ),
    
    br(), br()
  )
}

# =============================================================================
# DASHBOARD SERVER MODULE
# =============================================================================

dashboard_tab_server <- function(id, con, refresh_trigger, app_title = "Tutoring Monitoring System") {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for report generation
    values <- reactiveValues(
      report_ready = FALSE,
      report_path = NULL,
      ai_analysis = NULL,
      generation_in_progress = FALSE
    )
    
    # Get dashboard data
    dashboard_data <- reactive({
      refresh_trigger()  # Dependency to trigger refresh
      
      # Get all enrollment data including passed and active
      query <- "
        SELECT 
          e.enrollment_id,
          e.student_id,
          e.quarter,
          e.subject,
          e.total_attendance,
          e.status,
          s.student_name,
          s.grade_level,
          s.section
        FROM remedial_enrollments e
        LEFT JOIN full_student_registry s ON e.student_id = s.student_id
        WHERE e.status IN ('Active', 'Passed')
        ORDER BY e.quarter, e.subject, s.grade_level, s.student_name
      "
      
      dbGetQuery(con, query)
    })
    
    # Update grade filter choices
    observe({
      req(dashboard_data())
      
      grades <- sort(unique(dashboard_data()$grade_level))
      grade_choices <- c("All Grades" = "all")
      for (grade in grades) {
        grade_choices[paste("Grade", grade)] <- as.character(grade)
      }
      updateSelectInput(session, "filter_grade_level", choices = grade_choices)
    })
    
    # Value Boxes
    output$total_enrolled <- renderValueBox({
      total <- nrow(dashboard_data())
      valueBox(
        value = total,
        subtitle = "Total Enrolled Students",
        icon = icon("users"),
        color = "blue"
      )
    })
    
    output$active_students <- renderValueBox({
      active <- sum(dashboard_data()$status == "Active", na.rm = TRUE)
      valueBox(
        value = active,
        subtitle = "Active Students",
        icon = icon("user-check"),
        color = "green"
      )
    })
    
    output$passed_students <- renderValueBox({
      passed <- sum(dashboard_data()$status == "Passed", na.rm = TRUE)
      valueBox(
        value = passed,
        subtitle = "Passed Students",
        icon = icon("graduation-cap"),
        color = "yellow"
      )
    })
    
    output$avg_attendance <- renderValueBox({
      avg_att <- round(mean(dashboard_data()$total_attendance, na.rm = TRUE), 1)
      valueBox(
        value = avg_att,
        subtitle = "Average Attendance",
        icon = icon("calendar-check"),
        color = "purple"
      )
    })
    
    # Charts using helper functions
    output$grade_distribution <- renderPlotly({
      create_grade_distribution_chart(dashboard_data())
    })
    
    output$subject_distribution <- renderPlotly({
      create_subject_distribution_chart(dashboard_data())
    })
    
    output$avg_sessions_chart <- renderPlotly({
      create_avg_sessions_chart(dashboard_data())
    })
    
    # ENHANCED Generate Report Button with Better Progress Handling
    observeEvent(input$generate_report, {
      
      # Prevent multiple simultaneous generations
      if (values$generation_in_progress) {
        showNotification("Report generation already in progress. Please wait...", 
                         type = "warning", duration = 5)
        return()
      }
      
      values$generation_in_progress <- TRUE
      values$report_ready <- FALSE
      
      # Show progress
      shinyjs::show("report_status")
      
      showNotification("🤖 Starting AI-powered analysis... This will take 2-3 minutes for comprehensive insights.", 
                       duration = 10, type = "message")
      
      # Use the report generation handler
      tryCatch({
        result <- handle_report_generation(dashboard_data(), app_title)
        
        if (result$success) {
          values$report_path <- result$file_path
          values$ai_analysis <- result$ai_analysis
          values$report_ready <- TRUE
          values$generation_in_progress <- FALSE
          
          # Hide progress
          shinyjs::hide("report_status")
          
          showNotification("✅ Report generated successfully! Your comprehensive analysis is ready for download.", 
                           duration = 15, type = "message")
        } else {
          stop(result$message)
        }
        
      }, error = function(e) {
        values$report_ready <- FALSE
        values$generation_in_progress <- FALSE
        shinyjs::hide("report_status")
        
        error_msg <- paste("❌ Report generation failed:", e$message)
        showNotification(error_msg, type = "error", duration = 20)
        
        message("Detailed error in report generation: ", toString(e))
        print(traceback())
      })
    })
    
    # Download Report
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("Tutoring_Analysis_Report_", format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        if (values$report_ready && !is.null(values$report_path) && file.exists(values$report_path)) {
          file.copy(values$report_path, file)
        } else {
          stop("Report file not available")
        }
      }
    )
    
    # Output for conditional panel
    output$report_ready <- reactive({
      return(values$report_ready)
    })
    outputOptions(output, "report_ready", suspendWhenHidden = FALSE)
    
    # CLASS LISTS FUNCTIONALITY
    
    # Create filtered data
    filtered_class_data <- reactive({
      apply_class_filters(
        dashboard_data(),
        input$filter_quarter,
        input$filter_subject,
        input$filter_grade_level
      )
    })
    
    # Generate class lists UI
    output$class_lists_output <- renderUI({
      req(filtered_class_data())
      generate_class_lists_ui(filtered_class_data(), session)
    })
    
    # Create data tables for classes
    observe({
      req(filtered_class_data())
      create_class_data_tables(filtered_class_data(), output, session)
    })
  })
}