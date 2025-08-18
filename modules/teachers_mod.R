# Enhanced Teachers Module with AI Report Generation

# Source the AI report generation functions
source("modules/enrollment/enrollment_dashboard/ai_helpers.R")
source("modules/enrollment/enrollment_dashboard/report_document_helpers.R")
source("modules/teacher/teacher_report_helpers.R") 

teachers_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1("Teachers Management", style = "color: #2c3e50; margin-bottom: 30px;"),
    
    tabsetPanel(
      # Teacher Registry Tab
      tabPanel("Teacher Registry",
               br(),
               fluidRow(
                 column(12,
                        actionButton(ns("add_teacher"), "Add New Teacher", 
                                     class = "btn-primary", style = "margin-bottom: 15px;")
                 )
               ),
               
               fluidRow(
                 column(12,
                        withSpinner(DT::dataTableOutput(ns("teachers_table")))
                 )
               )
      ),
      
      # Teacher Analytics Tab
      tabPanel("Teacher Analytics",
               br(),
               
               # Filters
               fluidRow(
                 column(4,
                        selectInput(ns("filter_grade"), "Grade Level:",
                                    choices = NULL, selected = NULL)
                 ),
                 column(4,
                        selectInput(ns("filter_subject"), "Subject:",
                                    choices = NULL, selected = NULL)
                 ),
                 column(4,
                        selectInput(ns("filter_competency"), "Learning Competency:",
                                    choices = NULL, selected = NULL)
                 )
               ),
               
               # First row of charts
               fluidRow(
                 column(6,
                        wellPanel(
                          h4("Tutoring Hours Distribution per Teacher", style = "color: #2c3e50;"),
                          withSpinner(plotlyOutput(ns("hours_distribution_chart")))
                        )
                 ),
                 column(6,
                        wellPanel(
                          h4("Learning Competency Distribution", style = "color: #2c3e50;"),
                          withSpinner(plotlyOutput(ns("competency_distribution_chart")))
                        )
                 )
               ),
               
               # Sentiment Analysis Section
               fluidRow(
                 column(6,
                        wellPanel(
                          h4("Sentiment Analysis", style = "color: #2c3e50;"),
                          br(),
                          fluidRow(
                            column(6,
                                   h5("Remarks & Observations", style = "color: #34495e;"),
                                   withSpinner(plotOutput(ns("remarks_sentiment_meter"), height = "150px"))
                            ),
                            column(6,
                                   h5("Assessment Reflection", style = "color: #34495e;"),
                                   withSpinner(plotOutput(ns("reflection_sentiment_meter"), height = "150px"))
                            )
                          )
                        )
                 ),
                 column(6,
                        wellPanel(
                          h4("Common Phrases Analysis", style = "color: #2c3e50;"),
                          br(),
                          fluidRow(
                            column(6,
                                   h5("Top Remarks Phrases", style = "color: #34495e;"),
                                   withSpinner(verbatimTextOutput(ns("top_remarks_phrases")))
                            ),
                            column(6,
                                   h5("Top Reflection Phrases", style = "color: #34495e;"),
                                   withSpinner(verbatimTextOutput(ns("top_reflection_phrases")))
                            )
                          )
                        )
                 )
               ),
               
               br(),
               
               fluidRow(
                 column(12,
                        wellPanel(
                          h4("Teacher Performance Summary", style = "color: #2c3e50;"),
                          withSpinner(DT::dataTableOutput(ns("teacher_performance_table")))
                        )
                 )
               )
      ),
      
      # AI Report Analysis Tab
      tabPanel("AI Report Analysis",
               br(),
               fluidRow(
                 column(12,
                        wellPanel(
                          h4("AI-Generated Teacher Performance Analysis", style = "color: #2c3e50;"),
                          p("Generate comprehensive AI-powered reports analyzing teacher performance, workload distribution, competency coverage, and strategic recommendations for faculty development."),
                          br(),
                          actionButton(ns("generate_full_report"), "Generate Complete AI Analysis Report", 
                                       class = "btn-success btn-lg", 
                                       icon = icon("robot"),
                                       style = "width: 100%; margin-bottom: 20px;"),
                          
                          conditionalPanel(
                            condition = paste0("output['", ns("show_analysis"), "']"),
                            wellPanel(
                              h5("AI Analysis Preview:", style = "color: #34495e;"),
                              div(id = ns("analysis_content"),
                                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; max-height: 400px; overflow-y: auto;",
                                  htmlOutput(ns("ai_analysis_preview"))
                              )
                            )
                          ),
                          
                          conditionalPanel(
                            condition = paste0("output['", ns("show_download"), "']"),
                            br(),
                            downloadButton(ns("download_report"), "Download Full Report (.docx)", 
                                           class = "btn-primary btn-lg",
                                           style = "width: 100%;")
                          )
                        )
                 )
               )
      )
    ),
    
    # Add/Edit Teacher Modal
    bsModal("teacher_modal", "Teacher Information", ns("add_teacher"), size = "medium",
            fluidRow(
              column(6,
                     textInput(ns("teacher_name"), "Teacher Name:", ""),
                     textInput(ns("teacherID"), "Teacher ID:", ""),
                     textInput(ns("subject_handled"), "Subject Handled:", "")
              ),
              column(6,
                     numericInput(ns("year_level"), "Year Level:", value = 7, min = 1, max = 12),
                     selectInput(ns("teacher_status"), "Status:",
                                 choices = c("Active", "Inactive", "On Leave"),
                                 selected = "Active")
              )
            ),
            
            footer = tagList(
              actionButton(ns("save_teacher"), "Save", class = "btn-primary"),
              actionButton(ns("cancel_teacher"), "Cancel", class = "btn-default")
            )
    )
  )
}

teachers_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      teachers_data = NULL,
      editing_id = NULL,
      refresh = 0,
      report_result = NULL,
      ai_analysis = NULL
    )
    
    # Helper function to create sentiment meter
    create_sentiment_meter <- function(sentiment_score, title) {
      # Normalize sentiment score to 0-10 scale
      normalized_score <- pmax(0, pmin(10, (sentiment_score + 1) * 5))
      
      # Determine color based on score
      if (normalized_score <= 3.3) {
        color <- "#e74c3c"  # Red
      } else if (normalized_score <= 6.7) {
        color <- "#f39c12"  # Yellow/Orange
      } else {
        color <- "#27ae60"  # Green
      }
      
      # Create gauge-like visualization
      par(mar = c(2, 2, 2, 2), bg = "white")
      plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 1), 
           axes = FALSE, xlab = "", ylab = "", main = title)
      
      # Background bar
      rect(0, 0.3, 10, 0.7, col = "#ecf0f1", border = NA)
      
      # Filled bar based on sentiment
      rect(0, 0.3, normalized_score, 0.7, col = color, border = NA)
      
      # Add scale markers
      for (i in 0:10) {
        lines(c(i, i), c(0.2, 0.8), col = "black", lwd = 0.5)
      }
      
      # Add score text
      text(5, 0.1, paste("Score:", round(normalized_score, 1)), 
           cex = 1.2, font = 2)
      
      # Add scale labels
      text(0, 0.9, "Negative", cex = 0.8)
      text(5, 0.9, "Neutral", cex = 0.8)
      text(10, 0.9, "Positive", cex = 0.8)
    }
    
    # Helper function to extract common phrases
    extract_common_phrases <- function(text_data, n_phrases = 5) {
      if (is.null(text_data) || length(text_data) == 0 || all(is.na(text_data))) {
        return("No data available")
      }
      
      # Clean and prepare text
      clean_text <- text_data[!is.na(text_data) & text_data != ""]
      
      if (length(clean_text) == 0) {
        return("No data available")
      }
      
      # Create corpus
      corpus <- corpus(clean_text)
      
      # Create tokens and remove stopwords, punctuation
      tokens <- tokens(corpus, remove_punct = TRUE, remove_symbols = TRUE) %>%
        tokens_remove(stopwords("english")) %>%
        tokens_tolower()
      
      # Create n-grams (2-3 word phrases)
      bigrams <- tokens_ngrams(tokens, n = 2:3)
      
      # Create document-feature matrix
      dfm_phrases <- dfm(bigrams)
      
      # Get top features
      top_features <- textstat_frequency(dfm_phrases, n = n_phrases)
      
      if (nrow(top_features) == 0) {
        return("No common phrases found")
      }
      
      # Format output
      phrases <- paste(1:min(n_phrases, nrow(top_features)), ". ", 
                       top_features$feature[1:min(n_phrases, nrow(top_features))], 
                       " (", top_features$frequency[1:min(n_phrases, nrow(top_features))], ")",
                       sep = "", collapse = "\n")
      
      return(phrases)
    }
    
    # Load teachers data
    observe({
      values$refresh  # Dependency for refresh
      values$teachers_data <- get_table_data(con, "teacher_registry")
    })
    
    # Update filter choices
    observe({
      # Grade levels from teacher registry
      if (!is.null(values$teachers_data) && nrow(values$teachers_data) > 0) {
        grades <- sort(unique(values$teachers_data$year_level))
        updateSelectInput(session, "filter_grade", 
                          choices = c("All" = "", grades))
      }
      
      # Subjects from teacher sessions
      subjects <- dbGetQuery(con, "SELECT DISTINCT subject FROM teacher_sessions WHERE subject IS NOT NULL")$subject
      updateSelectInput(session, "filter_subject",
                        choices = c("All" = "", subjects))
      
      # Learning competencies
      competencies <- dbGetQuery(con, "SELECT DISTINCT learning_competency FROM teacher_sessions WHERE learning_competency IS NOT NULL")$learning_competency
      updateSelectInput(session, "filter_competency",
                        choices = c("All" = "", competencies))
    })
    
    # Teachers table
    output$teachers_table <- DT::renderDataTable({
      req(values$teachers_data)
      
      display_data <- values$teachers_data %>%
        select(name, teacherID, subject_handled, year_level, teacher_status)
      
      DT::datatable(display_data,
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE,
                    selection = "single") %>%
        DT::formatStyle(columns = "teacher_status",
                        backgroundColor = DT::styleEqual(c("Active", "Inactive", "On Leave"), 
                                                         c("#d4edda", "#f8d7da", "#fff3cd")))
    })
    
    # Handle row selection for editing
    observeEvent(input$teachers_table_rows_selected, {
      if (length(input$teachers_table_rows_selected) > 0) {
        selected_row <- input$teachers_table_rows_selected
        teacher_data <- values$teachers_data[selected_row, ]
        
        values$editing_id <- teacher_data$id
        
        # Populate modal fields
        updateTextInput(session, "teacher_name", value = teacher_data$name)
        updateTextInput(session, "teacherID", value = teacher_data$teacherID)
        updateTextInput(session, "subject_handled", value = teacher_data$subject_handled)
        updateNumericInput(session, "year_level", value = teacher_data$year_level)
        updateSelectInput(session, "teacher_status", selected = teacher_data$teacher_status)
        
        toggleModal(session, "teacher_modal", toggle = "open")
      }
    })
    
    # Handle add new teacher
    observeEvent(input$add_teacher, {
      values$editing_id <- NULL
      
      # Clear modal fields
      updateTextInput(session, "teacher_name", value = "")
      updateTextInput(session, "teacherID", value = "")
      updateTextInput(session, "subject_handled", value = "")
      updateNumericInput(session, "year_level", value = 7)
      updateSelectInput(session, "teacher_status", selected = "Active")
    })
    
    # Handle save teacher
    observeEvent(input$save_teacher, {
      req(input$teacher_name, input$teacherID)
      
      teacher_data <- data.frame(
        name = input$teacher_name,
        teacherID = input$teacherID,
        subject_handled = input$subject_handled,
        year_level = input$year_level,
        teacher_status = input$teacher_status,
        stringsAsFactors = FALSE
      )
      
      if (is.null(values$editing_id)) {
        # Insert new teacher
        result <- insert_data(con, "teacher_registry", teacher_data)
      } else {
        # Update existing teacher
        result <- update_data(con, "teacher_registry", teacher_data, "id", values$editing_id)
      }
      
      if (result$success) {
        showNotification(result$message, type = "message")
        values$refresh <- values$refresh + 1
        toggleModal(session, "teacher_modal", toggle = "close")
      } else {
        showNotification(result$message, type = "error")
      }
    })
    
    # Handle cancel
    observeEvent(input$cancel_teacher, {
      toggleModal(session, "teacher_modal", toggle = "close")
    })
    
    # Helper function to build filter conditions for analytics
    build_analytics_filter <- function() {
      conditions <- c()
      
      if (!is.null(input$filter_grade) && input$filter_grade != "") {
        conditions <- c(conditions, paste0("ts.grade_level = ", input$filter_grade))
      }
      
      if (!is.null(input$filter_subject) && input$filter_subject != "") {
        conditions <- c(conditions, paste0("ts.subject = '", input$filter_subject, "'"))
      }
      
      if (!is.null(input$filter_competency) && input$filter_competency != "") {
        conditions <- c(conditions, paste0("ts.learning_competency = '", input$filter_competency, "'"))
      }
      
      if (length(conditions) > 0) {
        return(paste0("WHERE ", paste(conditions, collapse = " AND ")))
      } else {
        return("")
      }
    }
    
    # Teaching Hours Distribution Chart (Changed to Horizontal Bar)
    output$hours_distribution_chart <- renderPlotly({
      filter_conditions <- build_analytics_filter()
      
      query <- paste0("
        SELECT tr.name, SUM(ts.duration) as total_hours
        FROM teacher_registry tr
        LEFT JOIN teacher_sessions ts ON tr.teacherID = ts.teacher_id
        ", gsub("WHERE", "WHERE tr.teacher_status = 'Active' AND", filter_conditions, fixed = TRUE),
                      if (filter_conditions == "") "WHERE tr.teacher_status = 'Active'" else "",
                      "
        GROUP BY tr.name
        HAVING total_hours > 0
        ORDER BY total_hours DESC
        LIMIT 10
      ")
      
      hours_data <- dbGetQuery(con, query)
      
      if (nrow(hours_data) > 0) {
        p <- plot_ly(hours_data, 
                     y = ~reorder(name, total_hours), 
                     x = ~total_hours,
                     type = 'bar',
                     orientation = 'h',
                     marker = list(color = '#3498db'),
                     hovertemplate = '<b>%{y}</b><br>Hours: %{x}<extra></extra>') %>%
          layout(title = "",
                 yaxis = list(title = "Teacher"),
                 xaxis = list(title = "Total Hours"),
                 margin = list(l = 120))
        p
      } else {
        plotly_empty() %>%
          layout(title = "No data available")
      }
    })
    
    # Learning Competency Distribution Chart (New Horizontal Bar Chart)
    output$competency_distribution_chart <- renderPlotly({
      filter_conditions <- build_analytics_filter()
      
      query <- paste0("
        SELECT learning_competency, COUNT(*) as session_count
        FROM teacher_sessions ts
        JOIN teacher_registry tr ON tr.teacherID = ts.teacher_id
        ", gsub("WHERE", "WHERE tr.teacher_status = 'Active' AND", filter_conditions, fixed = TRUE),
                      if (filter_conditions == "") "WHERE tr.teacher_status = 'Active'" else "",
                      " AND learning_competency IS NOT NULL AND learning_competency != ''
        GROUP BY learning_competency
        ORDER BY session_count DESC
        LIMIT 10
      ")
      
      competency_data <- dbGetQuery(con, query)
      
      if (nrow(competency_data) > 0) {
        # Truncate long competency names for better display
        competency_data$display_name <- sapply(competency_data$learning_competency, function(x) {
          if (nchar(x) > 40) {
            paste0(substr(x, 1, 37), "...")
          } else {
            x
          }
        })
        
        p <- plot_ly(competency_data, 
                     y = ~reorder(display_name, session_count), 
                     x = ~session_count,
                     type = 'bar',
                     orientation = 'h',
                     marker = list(color = '#e74c3c'),
                     hovertemplate = '<b>%{customdata}</b><br>Sessions: %{x}<extra></extra>',
                     customdata = ~learning_competency) %>%
          layout(title = "",
                 yaxis = list(title = "Learning Competency"),
                 xaxis = list(title = "Number of Sessions"),
                 margin = list(l = 150))
        p
      } else {
        plotly_empty() %>%
          layout(title = "No data available")
      }
    })
    
    # Sentiment Analysis for Remarks using sentimentr
    output$remarks_sentiment_meter <- renderPlot({
      filter_conditions <- build_analytics_filter()
      
      query <- paste0("
        SELECT remarks_observation
        FROM teacher_sessions ts
        JOIN teacher_registry tr ON tr.teacherID = ts.teacher_id
        ", gsub("WHERE", "WHERE tr.teacher_status = 'Active' AND", filter_conditions, fixed = TRUE),
                      if (filter_conditions == "") "WHERE tr.teacher_status = 'Active'" else "",
                      " AND remarks_observation IS NOT NULL AND remarks_observation != ''
      ")
      
      remarks_data <- dbGetQuery(con, query)
      
      if (nrow(remarks_data) > 0 && !all(is.na(remarks_data$remarks_observation))) {
        # Clean and prepare text data
        clean_remarks <- remarks_data$remarks_observation[!is.na(remarks_data$remarks_observation) & 
                                                            remarks_data$remarks_observation != ""]
        
        if (length(clean_remarks) > 0) {
          # Perform sentiment analysis using sentimentr
          tryCatch({
            sentiment_results <- sentiment(clean_remarks)
            avg_sentiment <- mean(sentiment_results$sentiment, na.rm = TRUE)
            
            # sentimentr returns scores typically between -1 and 1, but can exceed this range
            # Normalize to ensure it fits our expected range
            avg_sentiment <- pmax(-1, pmin(1, avg_sentiment))
          }, error = function(e) {
            avg_sentiment <- 0
          })
        } else {
          avg_sentiment <- 0
        }
      } else {
        avg_sentiment <- 0
      }
      
      create_sentiment_meter(avg_sentiment, "")
    })
    
    # Sentiment Analysis for Assessment Reflection using sentimentr
    output$reflection_sentiment_meter <- renderPlot({
      filter_conditions <- build_analytics_filter()
      
      query <- paste0("
        SELECT assessment_reflection
        FROM teacher_sessions ts
        JOIN teacher_registry tr ON tr.teacherID = ts.teacher_id
        ", gsub("WHERE", "WHERE tr.teacher_status = 'Active' AND", filter_conditions, fixed = TRUE),
                      if (filter_conditions == "") "WHERE tr.teacher_status = 'Active'" else "",
                      " AND assessment_reflection IS NOT NULL AND assessment_reflection != ''
      ")
      
      reflection_data <- dbGetQuery(con, query)
      
      if (nrow(reflection_data) > 0 && !all(is.na(reflection_data$assessment_reflection))) {
        # Clean and prepare text data
        clean_reflections <- reflection_data$assessment_reflection[!is.na(reflection_data$assessment_reflection) & 
                                                                     reflection_data$assessment_reflection != ""]
        
        if (length(clean_reflections) > 0) {
          # Perform sentiment analysis using sentimentr
          tryCatch({
            sentiment_results <- sentiment(clean_reflections)
            avg_sentiment <- mean(sentiment_results$sentiment, na.rm = TRUE)
            
            # sentimentr returns scores typically between -1 and 1, but can exceed this range
            # Normalize to ensure it fits our expected range
            avg_sentiment <- pmax(-1, pmin(1, avg_sentiment))
          }, error = function(e) {
            avg_sentiment <- 0
          })
        } else {
          avg_sentiment <- 0
        }
      } else {
        avg_sentiment <- 0
      }
      
      create_sentiment_meter(avg_sentiment, "")
    })
    
    # Top Remarks Phrases
    output$top_remarks_phrases <- renderText({
      filter_conditions <- build_analytics_filter()
      
      query <- paste0("
        SELECT remarks_observation
        FROM teacher_sessions ts
        JOIN teacher_registry tr ON tr.teacherID = ts.teacher_id
        ", gsub("WHERE", "WHERE tr.teacher_status = 'Active' AND", filter_conditions, fixed = TRUE),
                      if (filter_conditions == "") "WHERE tr.teacher_status = 'Active'" else "",
                      " AND remarks_observation IS NOT NULL AND remarks_observation != ''
      ")
      
      remarks_data <- dbGetQuery(con, query)
      
      if (nrow(remarks_data) > 0) {
        extract_common_phrases(remarks_data$remarks_observation, 5)
      } else {
        "No remarks data available"
      }
    })
    
    # Top Reflection Phrases
    output$top_reflection_phrases <- renderText({
      filter_conditions <- build_analytics_filter()
      
      query <- paste0("
        SELECT assessment_reflection
        FROM teacher_sessions ts
        JOIN teacher_registry tr ON tr.teacherID = ts.teacher_id
        ", gsub("WHERE", "WHERE tr.teacher_status = 'Active' AND", filter_conditions, fixed = TRUE),
                      if (filter_conditions == "") "WHERE tr.teacher_status = 'Active'" else "",
                      " AND assessment_reflection IS NOT NULL AND assessment_reflection != ''
      ")
      
      reflection_data <- dbGetQuery(con, query)
      
      if (nrow(reflection_data) > 0) {
        extract_common_phrases(reflection_data$assessment_reflection, 5)
      } else {
        "No reflection data available"
      }
    })
    
    # Teacher Performance Table
    output$teacher_performance_table <- DT::renderDataTable({
      filter_conditions <- build_analytics_filter()
      
      query <- paste0("
        SELECT tr.name as 'Teacher Name',
               tr.subject_handled as 'Subject',
               COUNT(ts.id) as 'Total Sessions',
               ROUND(SUM(ts.duration), 2) as 'Total Hours',
               ROUND(AVG(ts.duration), 2) as 'Avg Session Duration'
        FROM teacher_registry tr
        LEFT JOIN teacher_sessions ts ON tr.teacherID = ts.teacher_id
        ", gsub("WHERE", "WHERE tr.teacher_status = 'Active' AND", filter_conditions, fixed = TRUE),
                      if (filter_conditions == "") "WHERE tr.teacher_status = 'Active'" else "",
                      "
        GROUP BY tr.name, tr.subject_handled
        ORDER BY `Total Hours` DESC
      ")
      
      performance_data <- dbGetQuery(con, query)
      
      DT::datatable(performance_data,
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    })
    
    # =============================================================================
    # AI REPORT GENERATION FUNCTIONALITY
    # =============================================================================
    
    # Handle full report generation (AI Report Analysis tab)
    observeEvent(input$generate_full_report, {
      req(values$teachers_data)
      
      if (nrow(values$teachers_data) == 0) {
        showNotification("No teacher data available for analysis.", type = "warning")
        return()
      }
      
      withProgress(message = 'Generating AI Analysis...', value = 0, {
        incProgress(0.1, detail = "Preparing teacher data...")
        
        tryCatch({
          # Get sessions data for comprehensive analysis
          sessions_data <- tryCatch({
            dbGetQuery(con, "SELECT * FROM teacher_sessions")
          }, error = function(e) {
            message("Sessions data not available: ", e$message)
            NULL
          })
          
          incProgress(0.3, detail = "Running AI analysis...")
          
          result <- handle_teachers_report_generation(
            teachers_data = values$teachers_data, 
            sessions_data = sessions_data,
            app_title = "Project ARAL Management System"
          )
          
          incProgress(0.8, detail = "Finalizing report...")
          
          if (result$success) {
            values$report_result <- result
            values$ai_analysis <- result$ai_analysis
            
            incProgress(1.0, detail = "Complete!")
            
            showNotification("AI analysis completed successfully!", type = "success")
          } else {
            showNotification(paste("Analysis failed:", result$message), type = "error")
          }
          
        }, error = function(e) {
          showNotification(paste("Error generating analysis:", e$message), type = "error")
        })
      })
    })
    
    # Show analysis content conditionally
    output$show_analysis <- reactive({
      !is.null(values$ai_analysis)
    })
    outputOptions(output, "show_analysis", suspendWhenHidden = FALSE)
    
    # Show download button conditionally
    output$show_download <- reactive({
      !is.null(values$report_result) && values$report_result$success
    })
    outputOptions(output, "show_download", suspendWhenHidden = FALSE)
    
    # Render AI analysis preview
    output$ai_analysis_preview <- renderText({
      req(values$ai_analysis)
      
      preview_html <- paste0(
        "<h5 style='color: #2c3e50; margin-bottom: 15px;'>Executive Summary</h5>",
        "<p style='text-align: justify; margin-bottom: 20px;'>", values$ai_analysis$executive_summary, "</p>",
        
        "<h5 style='color: #2c3e50; margin-bottom: 15px;'>Performance Analysis</h5>",
        "<p style='text-align: justify; margin-bottom: 20px;'>", 
        substr(values$ai_analysis$performance_analysis, 1, 300), "...</p>",
        
        "<h5 style='color: #2c3e50; margin-bottom: 15px;'>Workload Distribution</h5>",
        "<p style='text-align: justify; margin-bottom: 20px;'>", 
        substr(values$ai_analysis$workload_distribution, 1, 300), "...</p>",
        
        "<h5 style='color: #2c3e50; margin-bottom: 15px;'>Learning Competency Coverage</h5>",
        "<p style='text-align: justify; margin-bottom: 20px;'>", 
        substr(values$ai_analysis$competency_analysis, 1, 300), "...</p>",
        
        "<h5 style='color: #2c3e50; margin-bottom: 15px;'>Strategic Recommendations</h5>",
        "<p style='text-align: justify;'>", 
        substr(values$ai_analysis$recommendations, 1, 400), "...</p>",
        
        "<div style='margin-top: 20px; padding: 10px; background-color: #e8f6f3; border-left: 4px solid #2ecc71;'>",
        "<strong>👨‍🏫 Full detailed analysis including engagement insights, faculty directory, and comprehensive recommendations is available in the downloadable Word document.</strong>",
        "</div>"
      )
      
      preview_html
    })
    
    # Download handler for the generated report
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("Teachers_Performance_Analysis_Report_", format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        req(values$report_result, values$report_result$success)
        
        # Copy the temporary file to the download location
        file.copy(values$report_result$file_path, file)
      },
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    )
    
    # Clean up temporary files when session ends
    session$onSessionEnded(function() {
      if (!is.null(values$report_result) && values$report_result$success) {
        if (file.exists(values$report_result$file_path)) {
          file.remove(values$report_result$file_path)
        }
      }
    })
  })
}