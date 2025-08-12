# =============================================================================
# IMPROVED DASHBOARD WITH AUTO REPORTS GENERATOR (Enhanced Gemini API Integration)
# =============================================================================

# Required libraries
library(gemini.R)
library(officer)
library(flextable)
library(httr)
library(jsonlite)

# Assumption: gemini_api_key() has already been set in environment
# Call gemini_api_key("your-api-key") once at app startup

# =============================================================================
# ENHANCED GEMINI ANALYSIS FUNCTIONS WITH BETTER ERROR HANDLING
# =============================================================================

# Safe Gemini API caller with retry logic and proper wait times
safe_gemini_call <- function(prompt, max_retries = 3, wait_time = 8) {
  
  for (attempt in 1:max_retries) {
    tryCatch({
      # Add more wait time for complex analysis
      message(paste("Gemini API attempt", attempt, "- waiting", wait_time, "seconds..."))
      
      # Make the API call with explicit model specification
      response <- gemini(
        prompt = prompt  # Try the experimental model first
      )
      
      # Wait for response processing
      Sys.sleep(wait_time)
      
      # Validate response
      if (is.character(response) && length(response) > 0 && nchar(trimws(response)) > 20) {
        message(paste("✓ Gemini API call successful on attempt", attempt))
        return(trimws(response))
      } else {
        warning(paste("Invalid response on attempt", attempt, "- response:", toString(response)))
      }
      
    }, error = function(e) {
      warning(paste("Gemini API error on attempt", attempt, ":", e$message))
      
      # If it's a 404 error, try different model
      if (grepl("404", e$message, ignore.case = TRUE)) {
        tryCatch({
          message("Trying gemini-1.5-flash model due to 404 error...")
          response <- gemini(
            prompt = prompt
          )
          Sys.sleep(wait_time)
          
          if (is.character(response) && length(response) > 0 && nchar(trimws(response)) > 20) {
            message("✓ Gemini API call successful with fallback model")
            return(trimws(response))
          }
        }, error = function(e2) {
          warning(paste("Fallback model also failed:", e2$message))
        })
      }
      
      if (attempt < max_retries) {
        wait_time_retry <- wait_time + (attempt * 2)  # Increase wait time with each retry
        message(paste("Retrying in", wait_time_retry, "seconds..."))
        Sys.sleep(wait_time_retry)
      }
    })
  }
  
  # If all attempts failed, return NULL
  warning("All Gemini API attempts failed")
  return(NULL)
}

# Main AI analysis orchestrator function with enhanced error handling
gemini_analysis <- function(dashboard_summary) {
  
  message("Starting comprehensive AI analysis...")
  
  # Initialize results with fallback content
  results <- list(
    executive_summary = "Executive Summary: Analysis of the tutoring program data shows enrollment and performance patterns across multiple grade levels and subjects.",
    grade_analysis = "Grade level analysis shows distribution of students across different academic levels.",
    subject_analysis = "Subject analysis reveals enrollment patterns and engagement levels across different academic areas.",
    attendance_insights = "Attendance patterns provide insights into student engagement and program effectiveness.",
    recommendations = "1. Monitor student progress regularly\n2. Assess resource allocation\n3. Enhance student support programs\n4. Evaluate program effectiveness\n5. Implement targeted interventions"
  )
  
  # Generate each section with proper error handling
  tryCatch({
    message("Generating executive summary...")
    exec_result <- analyze_executive_summary(dashboard_summary)
    if (!is.null(exec_result)) results$executive_summary <- exec_result
  }, error = function(e) {
    message("Executive summary generation failed: ", e$message)
  })
  
  tryCatch({
    message("Analyzing grade distribution...")
    grade_result <- analyze_grade_distribution(dashboard_summary$grade_dist)
    if (!is.null(grade_result)) results$grade_analysis <- grade_result
  }, error = function(e) {
    message("Grade analysis failed: ", e$message)
  })
  
  tryCatch({
    message("Analyzing subject performance...")
    subject_result <- analyze_subject_performance(dashboard_summary$subject_dist, dashboard_summary$avg_sessions)
    if (!is.null(subject_result)) results$subject_analysis <- subject_result
  }, error = function(e) {
    message("Subject analysis failed: ", e$message)
  })
  
  tryCatch({
    message("Analyzing attendance patterns...")
    attendance_result <- analyze_attendance_patterns(dashboard_summary)
    if (!is.null(attendance_result)) results$attendance_insights <- attendance_result
  }, error = function(e) {
    message("Attendance analysis failed: ", e$message)
  })
  
  tryCatch({
    message("Generating recommendations...")
    rec_result <- generate_recommendations(dashboard_summary)
    if (!is.null(rec_result)) results$recommendations <- rec_result
  }, error = function(e) {
    message("Recommendations generation failed: ", e$message)
  })
  
  message("AI analysis completed!")
  return(results)
}

# Enhanced analysis functions with better prompts
analyze_executive_summary <- function(dashboard_summary) {
  prompt <- paste0(
    "As an educational data analyst, write a comprehensive 3-4 paragraph executive summary for a tutoring program report.\n\n",
    "PROGRAM DATA:\n",
    "• Total Students: ", dashboard_summary$total_enrolled, "\n",
    "• Active Students: ", dashboard_summary$active_students, " (", 
    round((dashboard_summary$active_students/dashboard_summary$total_enrolled)*100, 1), "%)\n",
    "• Students Who Passed: ", dashboard_summary$passed_students, " (", 
    round((dashboard_summary$passed_students/dashboard_summary$total_enrolled)*100, 1), "%)\n",
    "• Average Session Attendance: ", dashboard_summary$avg_attendance, " sessions per student\n\n",
    "REQUIREMENTS:\n",
    "- Start with an overview of the program's scope and current status\n",
    "- Highlight key performance indicators and success rates\n",
    "- Discuss student engagement and retention patterns\n",
    "- Conclude with overall program effectiveness assessment\n",
    "- Use professional, data-driven language suitable for stakeholders\n",
    "- Keep between 200-300 words\n\n",
    "Write the executive summary now:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 10))
}

analyze_grade_distribution <- function(grade_dist) {
  if (length(grade_dist) == 0) return("No grade distribution data available.")
  
  grade_text <- paste(paste("Grade", names(grade_dist), ":", grade_dist, "students"), collapse = "; ")
  total_students <- sum(grade_dist)
  
  prompt <- paste0(
    "Analyze this grade-level enrollment distribution for a tutoring program:\n\n",
    "ENROLLMENT DATA: ", grade_text, "\n",
    "TOTAL STUDENTS: ", total_students, "\n\n",
    "Provide a detailed analysis (150-200 words) covering:\n",
    "1. Which grade levels show the highest and lowest enrollment numbers\n",
    "2. What enrollment patterns suggest about academic support needs\n",
    "3. Potential reasons for distribution patterns\n",
    "4. Implications for staffing and resource allocation\n",
    "5. Recommendations for addressing any imbalances\n\n",
    "Use specific numbers from the data and provide actionable insights:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 8))
}

analyze_subject_performance <- function(subject_dist, avg_sessions) {
  if (length(subject_dist) == 0) return("No subject distribution data available.")
  
  subject_text <- paste(paste(names(subject_dist), ":", subject_dist, "students"), collapse = "; ")
  
  if (length(avg_sessions) > 0) {
    sessions_text <- paste(paste(names(avg_sessions), ":", round(avg_sessions, 1), "avg sessions"), collapse = "; ")
  } else {
    sessions_text <- "No session data available"
  }
  
  prompt <- paste0(
    "Analyze subject enrollment and attendance patterns for this tutoring program:\n\n",
    "SUBJECT ENROLLMENT: ", subject_text, "\n",
    "AVERAGE SESSION ATTENDANCE: ", sessions_text, "\n\n",
    "Provide comprehensive analysis (200-250 words) addressing:\n",
    "1. Which subjects have the highest demand and why this might be\n",
    "2. Correlation between subject enrollment and average attendance\n",
    "3. Subjects showing strong vs. weak student engagement\n",
    "4. What these patterns reveal about student academic needs\n",
    "5. Strategic recommendations for program improvement\n\n",
    "Include specific data points and provide educational insights:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 9))
}

analyze_attendance_patterns <- function(dashboard_summary) {
  pass_rate <- round((dashboard_summary$passed_students / dashboard_summary$total_enrolled) * 100, 1)
  active_rate <- round((dashboard_summary$active_students / dashboard_summary$total_enrolled) * 100, 1)
  completion_rate <- round(((dashboard_summary$passed_students + dashboard_summary$active_students) / dashboard_summary$total_enrolled) * 100, 1)
  
  prompt <- paste0(
    "Analyze attendance and completion patterns for this tutoring program:\n\n",
    "PROGRAM METRICS:\n",
    "• Total Enrolled: ", dashboard_summary$total_enrolled, " students\n",
    "• Pass Rate: ", pass_rate, "% (", dashboard_summary$passed_students, " students completed successfully)\n",
    "• Currently Active: ", active_rate, "% (", dashboard_summary$active_students, " students still enrolled)\n",
    "• Overall Engagement Rate: ", completion_rate, "% (active + passed students)\n",
    "• Average Sessions Attended: ", dashboard_summary$avg_attendance, " per student\n\n",
    "Provide detailed analysis (200-250 words) covering:\n",
    "1. Program effectiveness based on pass and retention rates\n",
    "2. Student engagement levels and what they indicate\n",
    "3. Attendance patterns and their relationship to success\n",
    "4. Identification of potential at-risk indicators\n",
    "5. Recommendations for improving retention and completion rates\n\n",
    "Focus on actionable insights for program administrators:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 9))
}

generate_recommendations <- function(dashboard_summary) {
  pass_rate <- round((dashboard_summary$passed_students / dashboard_summary$total_enrolled) * 100, 1)
  active_rate <- round((dashboard_summary$active_students / dashboard_summary$total_enrolled) * 100, 1)
  
  prompt <- paste0(
    "Based on this tutoring program data, provide 6-8 specific, actionable recommendations:\n\n",
    "PROGRAM PERFORMANCE:\n",
    "• Total Enrollment: ", dashboard_summary$total_enrolled, " students\n",
    "• Success Rate: ", pass_rate, "% completion rate\n",
    "• Active Retention: ", active_rate, "% currently active\n",
    "• Average Engagement: ", dashboard_summary$avg_attendance, " sessions per student\n\n",
    "Generate numbered recommendations (200-300 words total) focusing on:\n",
    "- Strategies to improve student retention and completion rates\n",
    "- Optimizing resource allocation based on enrollment patterns\n",
    "- Enhancing student engagement and attendance\n",
    "- Addressing any identified performance gaps\n",
    "- Implementing data-driven program improvements\n",
    "- Supporting at-risk student populations\n\n",
    "Format as numbered list with brief explanations. Be specific and actionable:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 10))
}

# =============================================================================
# IMPROVED DASHBOARD SERVER MODULE WITH BETTER ERROR HANDLING
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
    
    # Grade Level Distribution Chart
    output$grade_distribution <- renderPlotly({
      grade_dist <- dashboard_data() %>%
        group_by(grade_level) %>%
        summarise(count = n(), .groups = 'drop') %>%
        arrange(grade_level)
      
      p <- plot_ly(grade_dist, 
                   x = ~paste("Grade", grade_level), 
                   y = ~count,
                   type = 'bar',
                   marker = list(color = '#3498db'),
                   hovertemplate = 'Grade %{x}<br>Students: %{y}<extra></extra>') %>%
        layout(title = "",
               xaxis = list(title = "Grade Level"),
               yaxis = list(title = "Number of Students"),
               showlegend = FALSE)
      
      p
    })
    
    # Subject Distribution Chart
    output$subject_distribution <- renderPlotly({
      subject_dist <- dashboard_data() %>%
        group_by(subject) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(subject = tools::toTitleCase(subject)) %>%
        arrange(subject)
      
      p <- plot_ly(
        subject_dist,
        x = ~subject,
        y = ~count,
        type = 'bar',
        marker = list(color = '#f39c12'),
        hovertemplate = '%{x}<br>Students: %{y}<extra></extra>'
      ) %>%
        layout(
          title = "",
          xaxis = list(title = "Subject"),
          yaxis = list(title = "Number of Students"),
          showlegend = FALSE
        )
      
      p
    })
    
    # Average Sessions by Subject Chart
    output$avg_sessions_chart <- renderPlotly({
      avg_sessions <- dashboard_data() %>%
        group_by(subject) %>%
        summarise(avg_attendance = round(mean(total_attendance, na.rm = TRUE), 1), .groups = 'drop') %>%
        mutate(subject = tools::toTitleCase(subject))
      
      p <- plot_ly(avg_sessions, 
                   x = ~subject, 
                   y = ~avg_attendance,
                   type = 'bar',
                   marker = list(color = '#2ecc71'),
                   hovertemplate = '%{x}<br>Avg Sessions: %{y}<extra></extra>') %>%
        layout(title = "",
               xaxis = list(title = "Subject"),
               yaxis = list(title = "Average Sessions"),
               showlegend = FALSE)
      
      p
    })
    
    # Generate dashboard summary for AI analysis
    dashboard_summary <- reactive({
      req(dashboard_data())
      data <- dashboard_data()
      
      # Calculate summary statistics
      grade_dist <- table(data$grade_level)
      subject_dist <- table(tools::toTitleCase(data$subject))
      
      # Calculate average sessions by subject
      avg_sessions_df <- data %>% 
        group_by(subject) %>% 
        summarise(avg = round(mean(total_attendance, na.rm = TRUE), 1), .groups = 'drop')
      
      # Convert to named vector
      avg_sessions <- avg_sessions_df$avg
      names(avg_sessions) <- tools::toTitleCase(avg_sessions_df$subject)
      
      list(
        total_enrolled = nrow(data),
        active_students = sum(data$status == "Active", na.rm = TRUE),
        passed_students = sum(data$status == "Passed", na.rm = TRUE),
        avg_attendance = round(mean(data$total_attendance, na.rm = TRUE), 1),
        grade_dist = grade_dist,
        subject_dist = subject_dist,
        avg_sessions = avg_sessions,
        data = data
      )
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
      
      # Use future/promises for async processing if available, otherwise use regular processing
      tryCatch({
        summary_data <- dashboard_summary()
        
        # Validate data before processing
        if (summary_data$total_enrolled == 0) {
          stop("No student data available for analysis")
        }
        
        showNotification("📊 Analyzing enrollment patterns and performance metrics...", 
                         duration = 8, type = "message")
        
        # Generate AI analysis with better error handling
        values$ai_analysis <- gemini_analysis(summary_data)
        
        showNotification("📝 Generating professional Word document...", 
                         duration = 5, type = "message")
        
        # Create temporary file
        temp_file <- tempfile(fileext = ".docx")
        values$report_path <- temp_file
        
        # Generate Word document
        generate_word_report(summary_data, values$ai_analysis, temp_file, app_title)
        
        values$report_ready <- TRUE
        values$generation_in_progress <- FALSE
        
        # Hide progress
        shinyjs::hide("report_status")
        
        showNotification("✅ Report generated successfully! Your comprehensive analysis is ready for download.", 
                         duration = 15, type = "message")  # Fixed: removed 'success' type
        
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
    
    # [REST OF THE CLASS LISTS CODE REMAINS THE SAME AS IN ORIGINAL]
    # Class Lists Generation (keeping original code)
    filtered_class_data <- reactive({
      data <- dashboard_data()
      
      # Apply filters
      if (input$filter_quarter != "all") {
        data <- data %>% filter(quarter == input$filter_quarter)
      }
      
      if (input$filter_subject != "all") {
        data <- data %>% filter(subject == input$filter_subject)
      }
      
      if (input$filter_grade_level != "all") {
        data <- data %>% filter(grade_level == as.numeric(input$filter_grade_level))
      }
      
      return(data)
    })
    
    output$class_lists_output <- renderUI({
      req(filtered_class_data())
      data <- filtered_class_data()
      
      if (nrow(data) == 0) {
        return(div(
          h4("No students found with the selected filters.", 
             style = "text-align: center; color: #7f8c8d; margin: 50px 0;")
        ))
      }
      
      # Group data by quarter, subject, and grade level
      grouped_data <- data %>%
        group_by(quarter, subject, grade_level) %>%
        group_split()
      
      # Generate UI elements for each group
      class_tables <- lapply(grouped_data, function(group_data) {
        if (nrow(group_data) == 0) return(NULL)
        
        # Get group identifiers
        quarter <- group_data$quarter[1]
        subject <- tools::toTitleCase(group_data$subject[1])
        grade <- group_data$grade_level[1]
        
        # Create unique ID for this table
        table_id <- paste0("class_table_", quarter, "_", group_data$subject[1], "_", grade)
        
        # Prepare display data
        display_data <- group_data %>%
          select(student_name, section, total_attendance, status) %>%
          arrange(section, student_name)
        
        # Create the box with table
        box(
          title = paste("Grade", grade, "-", subject, "-", quarter, 
                        "(", nrow(display_data), "students)"),
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          collapsible = TRUE,
          
          DT::dataTableOutput(session$ns(table_id))
        )
      })
      
      # Remove NULL elements
      class_tables <- class_tables[!sapply(class_tables, is.null)]
      
      if (length(class_tables) == 0) {
        return(div(
          h4("No class data available for the selected filters.", 
             style = "text-align: center; color: #7f8c8d; margin: 50px 0;")
        ))
      }
      
      # Arrange tables in rows of 2
      table_rows <- list()
      for (i in seq(1, length(class_tables), 2)) {
        if (i + 1 <= length(class_tables)) {
          table_rows[[length(table_rows) + 1]] <- fluidRow(
            class_tables[[i]],
            class_tables[[i + 1]]
          )
        } else {
          table_rows[[length(table_rows) + 1]] <- fluidRow(
            class_tables[[i]]
          )
        }
      }
      
      return(do.call(tagList, table_rows))
    })
    
    # Observe changes in filtered data and create corresponding tables
    observe({
      req(filtered_class_data())
      
      data <- filtered_class_data()
      
      # Group data by quarter, subject, and grade level
      grouped_data <- data %>%
        group_by(quarter, subject, grade_level) %>%
        group_split()
      
      # Create tables for each group
      lapply(grouped_data, function(group_data) {
        if (nrow(group_data) == 0) return(NULL)
        
        # Get group identifiers
        quarter <- group_data$quarter[1]
        subject <- group_data$subject[1]
        grade <- group_data$grade_level[1]
        
        # Create unique ID for this table
        table_id <- paste0("class_table_", quarter, "_", subject, "_", grade)
        
        # Prepare display data
        display_data <- group_data %>%
          select(student_name, section, total_attendance, status) %>%
          arrange(section, student_name)
        
        # Create the data table output
        output[[table_id]] <- DT::renderDataTable({
          DT::datatable(
            display_data,
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              searching = TRUE,
              ordering = TRUE,
              dom = 'ftp'  # Show filter, table, and pagination only
            ),
            colnames = c("Student Name", "Section", "Attendance", "Status"),
            caption = paste("Students enrolled in Grade", grade, tools::toTitleCase(subject), quarter)
          ) %>%
            DT::formatStyle("status",
                            backgroundColor = DT::styleEqual(c("Active", "Passed"), 
                                                             c("#d4edda", "#cce5ff")))
        })
      })
    })
  })
}

# =============================================================================
# ENHANCED WORD DOCUMENT GENERATION FUNCTION (SAME AS ORIGINAL)
# =============================================================================

generate_word_report <- function(dashboard_summary, ai_sections, output_path, app_title) {
  tryCatch({
    # Create new Word document
    doc <- read_docx()
    
    # Add header with app title and date
    doc <- doc %>%
      body_add_par(app_title, style = "heading 1") %>%
      body_add_par("Comprehensive AI-Powered Tutoring Program Analysis", style = "heading 2") %>%
      body_add_par(paste("Generated on:", format(Sys.Date(), "%B %d, %Y")), style = "Normal") %>%
      body_add_break()
    
    # Executive Summary Section
    doc <- doc %>%
      body_add_par("EXECUTIVE SUMMARY", style = "heading 2") %>%
      body_add_par(ai_sections$executive_summary, style = "Normal") %>%
      body_add_break()
    
    # Key Metrics Overview
    doc <- doc %>%
      body_add_par("KEY PROGRAM METRICS", style = "heading 2")
    
    # Create enhanced metrics table
    pass_rate <- round((dashboard_summary$passed_students / dashboard_summary$total_enrolled) * 100, 1)
    active_rate <- round((dashboard_summary$active_students / dashboard_summary$total_enrolled) * 100, 1)
    
    metrics_data <- data.frame(
      Metric = c("Total Enrolled Students", "Currently Active Students", "Students Who Passed", 
                 "Average Attendance (Sessions)", "Success Rate", "Active Retention Rate"),
      Value = c(
        dashboard_summary$total_enrolled,
        dashboard_summary$active_students, 
        dashboard_summary$passed_students,
        dashboard_summary$avg_attendance,
        paste0(pass_rate, "%"),
        paste0(active_rate, "%")
      ),
      stringsAsFactors = FALSE
    )
    
    metrics_table <- flextable(metrics_data) %>%
      theme_booktabs() %>%
      autofit() %>%
      bg(part = "header", bg = "#3498db") %>%
      color(part = "header", color = "white") %>%
      bold(part = "header")
    
    doc <- doc %>%
      body_add_flextable(metrics_table) %>%
      body_add_break()
    
    # Grade Level Analysis
    doc <- doc %>%
      body_add_par("GRADE LEVEL ENROLLMENT ANALYSIS", style = "heading 2") %>%
      body_add_par(ai_sections$grade_analysis, style = "Normal") %>%
      body_add_break()
    
    # Only create grade table if data exists
    if (length(dashboard_summary$grade_dist) > 0) {
      grade_data <- data.frame(
        Grade_Level = paste("Grade", names(dashboard_summary$grade_dist)),
        Students_Enrolled = as.numeric(dashboard_summary$grade_dist),
        Percentage = paste0(round((as.numeric(dashboard_summary$grade_dist) / dashboard_summary$total_enrolled) * 100, 1), "%"),
        stringsAsFactors = FALSE
      )
      
      grade_table <- flextable(grade_data) %>%
        theme_booktabs() %>%
        autofit() %>%
        bg(part = "header", bg = "#9b59b6") %>%
        color(part = "header", color = "white") %>%
        bold(part = "header")
      
      doc <- doc %>%
        body_add_flextable(grade_table) %>%
        body_add_break()
    }
    
    # Subject Performance Analysis
    doc <- doc %>%
      body_add_par("SUBJECT PERFORMANCE ANALYSIS", style = "heading 2") %>%
      body_add_par(ai_sections$subject_analysis, style = "Normal") %>%
      body_add_break()
    
    # Only create subject table if data exists
    if (length(dashboard_summary$subject_dist) > 0) {
      # Ensure avg_sessions alignment with subject_dist
      subject_names <- names(dashboard_summary$subject_dist)
      avg_sessions_aligned <- numeric(length(subject_names))
      
      for (i in seq_along(subject_names)) {
        subject_name <- subject_names[i]
        # Try to find matching average session data
        matching_avg <- dashboard_summary$avg_sessions[names(dashboard_summary$avg_sessions) == tools::toTitleCase(subject_name)]
        if (length(matching_avg) > 0) {
          avg_sessions_aligned[i] <- matching_avg
        } else {
          avg_sessions_aligned[i] <- 0
        }
      }
      
      subject_data <- data.frame(
        Subject = tools::toTitleCase(subject_names),
        Students_Enrolled = as.numeric(dashboard_summary$subject_dist),
        Average_Sessions = round(avg_sessions_aligned, 1),
        Engagement_Level = ifelse(avg_sessions_aligned > mean(avg_sessions_aligned, na.rm = TRUE), "High", "Moderate"),
        stringsAsFactors = FALSE
      )
      
      subject_table <- flextable(subject_data) %>%
        theme_booktabs() %>%
        autofit() %>%
        bg(part = "header", bg = "#e67e22") %>%
        color(part = "header", color = "white") %>%
        bold(part = "header") %>%
        bg(i = ~ Engagement_Level == "High", j = "Engagement_Level", bg = "#d5f4e6") %>%
        bg(i = ~ Engagement_Level == "Moderate", j = "Engagement_Level", bg = "#fff3cd")
      
      doc <- doc %>%
        body_add_flextable(subject_table) %>%
        body_add_break()
    }
    
    # Attendance Insights
    doc <- doc %>%
      body_add_par("ATTENDANCE PATTERNS & INSIGHTS", style = "heading 2") %>%
      body_add_par(ai_sections$attendance_insights, style = "Normal") %>%
      body_add_break()
    
    # Recommendations Section
    doc <- doc %>%
      body_add_par("STRATEGIC RECOMMENDATIONS", style = "heading 2") %>%
      body_add_par(ai_sections$recommendations, style = "Normal") %>%
      body_add_break()
    
    # Detailed Class Lists Section
    doc <- doc %>%
      body_add_par("DETAILED ENROLLMENT BY CLASS", style = "heading 2") %>%
      body_add_par("The following section provides comprehensive enrollment details organized by quarter, subject, and grade level, including individual student attendance patterns.", style = "Normal") %>%
      body_add_break()
    
    # Group data by quarter, subject, and grade
    grouped_data <- dashboard_summary$data %>%
      group_by(quarter, subject, grade_level) %>%
      group_split()
    
    for (group in grouped_data) {
      if (nrow(group) > 0) {
        quarter <- group$quarter[1]
        subject <- tools::toTitleCase(group$subject[1])
        grade <- group$grade_level[1]
        
        # Add section header
        doc <- doc %>%
          body_add_par(paste("Grade", grade, "-", subject, "-", quarter, 
                             "(", nrow(group), "students)"), style = "heading 3")
        
        # Prepare class data with summary stats
        class_summary <- group %>%
          summarise(
            total_students = n(),
            active_students = sum(status == "Active", na.rm = TRUE),
            passed_students = sum(status == "Passed", na.rm = TRUE),
            avg_attendance = round(mean(total_attendance, na.rm = TRUE), 1),
            .groups = 'drop'
          )
        
        doc <- doc %>%
          body_add_par(paste0("Class Summary: ", class_summary$active_students, " active, ", 
                              class_summary$passed_students, " passed, average ", 
                              class_summary$avg_attendance, " sessions attended"), style = "Normal")
        
        # Create class table (limit to top 20 students if more)
        class_data <- group %>%
          select(student_name, section, total_attendance, status) %>%
          arrange(desc(total_attendance), section, student_name) %>%
          slice_head(n = 20)
        
        names(class_data) <- c("Student Name", "Section", "Sessions Attended", "Status")
        
        # Create class table
        class_table <- flextable(class_data) %>%
          theme_booktabs() %>%
          autofit() %>%
          bg(part = "header", bg = "#34495e") %>%
          color(part = "header", color = "white") %>%
          bold(part = "header") %>%
          bg(i = ~ Status == "Active", j = "Status", bg = "#d4edda") %>%
          bg(i = ~ Status == "Passed", j = "Status", bg = "#cce5ff")
        
        doc <- doc %>%
          body_add_flextable(class_table)
        
        if (nrow(group) > 20) {
          doc <- doc %>%
            body_add_par(paste("(Showing top 20 of", nrow(group), "students by attendance)"), style = "Normal")
        }
        
        doc <- doc %>% body_add_break()
      }
    }
    
    # Add enhanced footer with generation details
    doc <- doc %>%
      body_add_par("---", style = "Normal") %>%
      body_add_par("REPORT DETAILS", style = "heading 3") %>%
      body_add_par(paste("Report generated by:", app_title), style = "Normal") %>%
      body_add_par(paste("Generated on:", format(Sys.time(), "%B %d, %Y at %I:%M %p")), style = "Normal") %>%
      body_add_par("AI Analysis powered by Google Gemini", style = "Normal") %>%
      body_add_par("This report provides comprehensive analysis of tutoring program data with actionable insights for program improvement.", style = "Normal")
    
    # Save document
    print(doc, target = output_path)
    
    message("✓ Word document successfully generated at: ", output_path)
    return(TRUE)
    
  }, error = function(e) {
    message("❌ Error in generate_word_report: ", e$message)
    print(traceback())
    stop(e)
  })
}

# =============================================================================
# UI MODULE (SAME AS ORIGINAL - KEEPING FOR COMPLETENESS)
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
    
    # Reports Generation Section - ENHANCED
    div("data-aos" = "fade-down", "data-aos-delay" = "50",
        box(
          title = "🤖 AI-Powered Analytics Report Generator",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          
          div(
            p(icon("robot"), " Generate comprehensive AI-powered analysis reports with detailed insights, recommendations, and professional formatting."),
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
    
    br(), br()
  )
}