# =============================================================================
# SESSION THEMES ANALYSIS - SIMPLE GEMINI-POWERED THEME EXTRACTION
# =============================================================================

# UI Module
session_themes_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1("Session Themes Analysis", style = "color: #2c3e50; margin-bottom: 30px;"),
    
    fluidRow(
      column(4,
             wellPanel(
               h4("Analysis Settings"),
               selectInput(ns("analysis_type"), "Analyze:",
                           choices = list(
                             "Remarks (Class Observations)" = "remarks",
                             "Assessments (Student Outcomes)" = "assessments"
                           ),
                           selected = "remarks"),
               
               dateRangeInput(ns("date_range"), "Date Range:",
                              start = Sys.Date() - 60,
                              end = Sys.Date()),
               
               selectInput(ns("filter_teacher"), "Teacher:",
                           choices = NULL, selected = NULL),
               
               selectInput(ns("filter_subject"), "Subject:",
                           choices = NULL, selected = NULL),
               
               br(),
               actionButton(ns("analyze_themes"), "Extract Themes", 
                            class = "btn-primary btn-block")
             ),
             
             # Data summary
             wellPanel(
               h5("Data Summary"),
               withSpinner(htmlOutput(ns("data_summary")))
             )
      ),
      
      column(8,
             conditionalPanel(
               condition = paste0("output['", ns("show_results"), "']"),
               div(class = "panel panel-success",
                   div(class = "panel-heading",
                       h4("🔍 Common Themes Identified", style = "color: white; margin: 0;")
                   ),
                   div(class = "panel-body",
                       withSpinner(htmlOutput(ns("themes_display")))
                   )
               )
             )
      )
    )
  )
}

# Server Module
session_themes_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    values <- reactiveValues(
      sessions_data = NULL,
      filtered_data = NULL,
      themes_result = NULL
    )
    
    # Load data and update filters
    observe({
      values$sessions_data <- get_table_data(con, "teacher_sessions")
      
      if (!is.null(values$sessions_data) && nrow(values$sessions_data) > 0) {
        teachers <- dbGetQuery(con, "SELECT DISTINCT name, teacherID FROM teacher_registry WHERE teacher_status = 'Active'")
        teacher_choices <- setNames(teachers$teacherID, teachers$name)
        
        updateSelectInput(session, "filter_teacher", 
                          choices = c("All Teachers" = "", teacher_choices))
        
        subjects <- unique(values$sessions_data$subject[!is.na(values$sessions_data$subject) & values$sessions_data$subject != ""])
        updateSelectInput(session, "filter_subject", 
                          choices = c("All Subjects" = "", subjects))
      }
    })
    
    # Filter data
    observe({
      req(values$sessions_data)
      
      filtered <- values$sessions_data
      
      if (!is.null(input$date_range)) {
        filtered <- filtered[as.Date(filtered$date) >= input$date_range[1] & 
                               as.Date(filtered$date) <= input$date_range[2], ]
      }
      
      if (!is.null(input$filter_teacher) && input$filter_teacher != "") {
        filtered <- filtered[filtered$teacher_id == input$filter_teacher, ]
      }
      
      if (!is.null(input$filter_subject) && input$filter_subject != "") {
        filtered <- filtered[filtered$subject == input$filter_subject, ]
      }
      
      values$filtered_data <- filtered
    })
    
    # Data summary
    output$data_summary <- renderUI({
      req(values$filtered_data)
      
      total_sessions <- nrow(values$filtered_data)
      
      if (input$analysis_type == "remarks") {
        valid_entries <- sum(!is.na(values$filtered_data$remarks_observation) & 
                               values$filtered_data$remarks_observation != "")
        field_name <- "Remarks"
      } else {
        valid_entries <- sum(!is.na(values$filtered_data$assessment_reflection) & 
                               values$filtered_data$assessment_reflection != "")
        field_name <- "Assessments"
      }
      
      div(
        p(strong("Total Sessions: "), total_sessions),
        p(strong(paste("Valid", field_name, ": ")), valid_entries),
        p(strong("Coverage: "), paste0(round((valid_entries/total_sessions)*100, 1), "%"))
      )
    })
    
    # Show results condition
    output$show_results <- reactive({
      !is.null(values$themes_result)
    })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)
    
    # Analyze themes
    observeEvent(input$analyze_themes, {
      req(values$filtered_data)
      
      # Get text data based on analysis type
      if (input$analysis_type == "remarks") {
        text_data <- values$filtered_data$remarks_observation[
          !is.na(values$filtered_data$remarks_observation) & 
            values$filtered_data$remarks_observation != ""
        ]
      } else {
        text_data <- values$filtered_data$assessment_reflection[
          !is.na(values$filtered_data$assessment_reflection) & 
            values$filtered_data$assessment_reflection != ""
        ]
      }
      
      # Validate minimum data requirements
      if (length(text_data) < 3) {
        showNotification("Need at least 3 entries for theme analysis", type = "warning")
        return()
      }
      
      if (length(text_data) < 10) {
        showNotification(paste("Limited data available (", length(text_data), " entries). Results may be less comprehensive."), 
                         type = "warning", duration = 8)
      }
      
      showNotification("🤖 Analyzing themes...", type = "message", duration = 5)
      
      tryCatch({
        result <- extract_session_themes(text_data, input$analysis_type)
        values$themes_result <- result
        
        if (result$success) {
          showNotification("✅ Themes extracted successfully!", type = "message")
        } else {
          showNotification(paste("❌ Error:", result$message), type = "error")
        }
        
      }, error = function(e) {
        showNotification(paste("❌ Analysis failed:", e$message), type = "error")
      })
    })
    
    # Display themes
    output$themes_display <- renderUI({
      req(values$themes_result)
      
      if (values$themes_result$success) {
        HTML(values$themes_result$html_output)
      } else {
        div(class = "alert alert-danger",
            p(paste("Analysis failed:", values$themes_result$message)))
      }
    })
  })
}

# =============================================================================
# GEMINI THEME EXTRACTION FUNCTION
# =============================================================================

extract_session_themes <- function(text_data, analysis_type) {
  tryCatch({
    library(gemini.R)
    
    # Robust sampling with validation
    total_entries <- length(text_data)
    
    # Determine sample size and adjust expectations
    if (total_entries < 3) {
      stop("Insufficient data: Need at least 3 entries for analysis")
    }
    
    # Smart sampling based on available data
    if (total_entries <= 10) {
      sample_texts <- text_data  # Use all data
      sample_size <- total_entries
      theme_count <- "3-5"  # Expect fewer themes
    } else if (total_entries <= 25) {
      sample_size <- total_entries  # Use all data
      sample_texts <- text_data
      theme_count <- "5-7"
    } else if (total_entries <= 50) {
      sample_size <- total_entries  # Use all data
      sample_texts <- text_data
      theme_count <- "6-8"
    } else {
      sample_size <- 50  # Sample for API efficiency
      sample_texts <- sample(text_data, sample_size)
      theme_count <- "8-10"
    }
    
    # Create focused prompt with adaptive expectations
    field_description <- if (analysis_type == "remarks") {
      "classroom observations and teaching remarks describing how lessons were conducted"
    } else {
      "assessment reflections describing student outcomes and what teaching methods worked"
    }
    
    prompt <- paste0(
      "Analyze the following ", field_description, " and identify the most common themes.\n\n",
      "DATA (", sample_size, " entries from ", total_entries, " total):\n",
      paste(sample_texts, collapse = "\n---\n"),
      "\n\nBased on this dataset size, extract ", theme_count, " common themes and rank them by frequency. ",
      if (total_entries < 15) {
        "Since the dataset is small, focus on the most prominent patterns even if they appear infrequently. "
      } else {
        ""
      },
      "For each theme, provide:\n",
      "1. Theme name (2-4 words)\n",
      "2. Frequency level (Most Common, Very Common, Common, Moderate, Less Common)\n",
      "3. Brief description (1 sentence)\n\n",
      "Format as:\n",
      "THEME: [name] | FREQUENCY: [level] | DESCRIPTION: [description]\n\n",
      "Focus on actionable, specific themes that teachers can understand and act upon.",
      if (total_entries < 15) {
        "\n\nNote: Adjust frequency labels appropriately for this smaller dataset size."
      } else {
        ""
      }
    )
    
    # Call Gemini
    response <- gemini(prompt)
    
    # Generate implications and recommendations
    implications_response <- analyze_theme_implications(response, analysis_type, total_entries)
    
    # Create HTML output
    html_output <- format_themes_html(response, implications_response, analysis_type, total_entries, sample_size)
    
    return(list(
      success = TRUE,
      html_output = html_output,
      raw_response = response,
      sample_info = list(
        total_entries = total_entries,
        sample_size = sample_size,
        coverage = round((sample_size/total_entries)*100, 1)
      )
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Theme extraction error:", e$message),
      html_output = ""
    ))
  })
}

# =============================================================================
# THEME IMPLICATIONS ANALYSIS
# =============================================================================

analyze_theme_implications <- function(themes_response, analysis_type, total_entries) {
  tryCatch({
    field_focus <- if (analysis_type == "remarks") {
      "classroom observations and teaching practices"
    } else {
      "student assessment outcomes and learning effectiveness"
    }
    
    # Adjust analysis depth based on data size
    analysis_depth <- if (total_entries < 15) {
      "Keep the analysis focused and practical given the limited dataset size. "
    } else if (total_entries < 30) {
      "Provide moderate depth analysis appropriate for this dataset size. "
    } else {
      "Provide comprehensive analysis with detailed insights. "
    }
    
    prompt <- paste0(
      "Based on the following identified themes from ", field_focus, " (", total_entries, " total entries), provide:\n\n",
      "IDENTIFIED THEMES:\n",
      themes_response,
      "\n\nAnalyze these themes and provide:\n",
      "1. KEY IMPLICATIONS: What do these themes reveal about current teaching/learning conditions?\n",
      "2. PRIORITY AREAS: Which themes need immediate attention?\n",
      "3. ACTIONABLE RECOMMENDATIONS: Specific steps to address the most critical themes\n\n",
      analysis_depth,
      "Focus on practical implications that school administrators and teachers can act upon."
    )
    
    implications <- gemini(prompt)
    return(implications)
    
  }, error = function(e) {
    return("Unable to generate implications analysis due to API error.")
  })
}

# =============================================================================
# HTML FORMATTING FUNCTION
# =============================================================================

format_themes_html <- function(gemini_response, implications_response, analysis_type, total_entries, sample_size = total_entries) {
  
  # Create header
  analysis_title <- if (analysis_type == "remarks") {
    "Classroom Observation Themes"
  } else {
    "Student Assessment Themes"
  }
  
  # Create sampling info badge
  sampling_info <- if (sample_size < total_entries) {
    paste0('<span class="badge badge-info" style="font-size: 0.8em;">Analyzed ', sample_size, ' of ', total_entries, ' entries (', round((sample_size/total_entries)*100, 1), '% coverage)</span>')
  } else if (total_entries < 15) {
    paste0('<span class="badge badge-warning" style="font-size: 0.8em;">Limited dataset: ', total_entries, ' entries</span>')
  } else {
    paste0('<span class="badge badge-success" style="font-size: 0.8em;">Full analysis: ', total_entries, ' entries</span>')
  }
  
  # Format the themes response
  formatted_themes <- gsub("\n", "<br>", gemini_response)
  formatted_themes <- gsub("THEME:", "<strong>THEME:</strong>", formatted_themes)
  formatted_themes <- gsub("FREQUENCY:", "<span class='frequency-badge'>FREQUENCY:</span>", formatted_themes)
  formatted_themes <- gsub("DESCRIPTION:", "<em>DESCRIPTION:</em>", formatted_themes)
  
  # Add frequency color coding
  formatted_themes <- gsub("Most Common", "<span class='freq-most'>Most Common</span>", formatted_themes)
  formatted_themes <- gsub("Very Common", "<span class='freq-very'>Very Common</span>", formatted_themes)
  formatted_themes <- gsub("Common", "<span class='freq-common'>Common</span>", formatted_themes)
  formatted_themes <- gsub("Moderate", "<span class='freq-moderate'>Moderate</span>", formatted_themes)
  formatted_themes <- gsub("Less Common", "<span class='freq-less'>Less Common</span>", formatted_themes)
  
  # Format implications response
  formatted_implications <- gsub("\n", "<br>", implications_response)
  formatted_implications <- gsub("KEY IMPLICATIONS:", "<h4>🔍 Key Implications</h4>", formatted_implications)
  formatted_implications <- gsub("PRIORITY AREAS:", "<h4>⚠️ Priority Areas</h4>", formatted_implications)
  formatted_implications <- gsub("ACTIONABLE RECOMMENDATIONS:", "<h4>💡 Actionable Recommendations</h4>", formatted_implications)
  formatted_implications <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", formatted_implications)
  
  # Data quality notice for small datasets
  quality_notice <- if (total_entries < 10) {
    '<div class="alert alert-warning" style="margin-bottom: 15px; padding: 10px; border-radius: 5px;">
    <strong>⚠️ Limited Data Notice:</strong> This analysis is based on a small dataset. Results should be interpreted with caution and may not represent comprehensive patterns.
    </div>'
  } else if (total_entries < 25) {
    '<div class="alert alert-info" style="margin-bottom: 15px; padding: 8px; border-radius: 5px;">
    <strong>ℹ️ Dataset Notice:</strong> Analysis based on moderate dataset size. Consider collecting more data for more robust insights.
    </div>'
  } else {
    ""
  }
  
  html_output <- paste0(
    '<div class="themes-analysis">',
    '<div class="themes-header" style="background: linear-gradient(135deg, #4CAF50, #45a049); color: white; padding: 15px; border-radius: 8px; margin-bottom: 20px;">',
    '<h3 style="margin: 0; color: white;">', analysis_title, '</h3>',
    '<p style="margin: 5px 0 0 0; opacity: 0.9;">Based on ', total_entries, ' session entries</p>',
    '<div style="margin-top: 8px;">', sampling_info, '</div>',
    '</div>',
    
    quality_notice,
    
    '<div class="themes-content" style="background: white; padding: 20px; border-radius: 8px; border: 1px solid #ddd; margin-bottom: 20px;">',
    '<h4>📊 Common Themes Identified</h4>',
    formatted_themes,
    '</div>',
    
    '<div class="implications-content" style="background: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 4px solid #007bff;">',
    '<h3 style="color: #007bff; margin-top: 0;">📋 Analysis & Recommendations</h3>',
    formatted_implications,
    '</div>',
    
    '<div class="themes-note" style="margin-top: 15px; padding: 10px; background: #e9ecef; border-radius: 5px; font-size: 0.9em;">',
    '<strong>Note:</strong> This AI analysis identifies patterns in session data and provides actionable insights for improving teaching practices and student outcomes.',
    if (sample_size < total_entries) {
      paste0(' Analysis used a representative sample of ', round((sample_size/total_entries)*100, 1), '% of available data.')
    } else {
      ""
    },
    '</div>',
    '</div>',
    
    # CSS Styles
    '<style>',
    '.frequency-badge { color: #007bff; font-weight: bold; }',
    '.freq-most { background: #d4edda; color: #155724; padding: 2px 8px; border-radius: 12px; font-size: 0.85em; font-weight: bold; }',
    '.freq-very { background: #d1ecf1; color: #0c5460; padding: 2px 8px; border-radius: 12px; font-size: 0.85em; font-weight: bold; }',
    '.freq-common { background: #fff3cd; color: #856404; padding: 2px 8px; border-radius: 12px; font-size: 0.85em; font-weight: bold; }',
    '.freq-moderate { background: #f8d7da; color: #721c24; padding: 2px 8px; border-radius: 12px; font-size: 0.85em; font-weight: bold; }',
    '.freq-less { background: #e2e3e5; color: #383d41; padding: 2px 8px; border-radius: 12px; font-size: 0.85em; font-weight: bold; }',
    '.themes-content strong { color: #2c3e50; }',
    '.themes-content em { color: #6c757d; }',
    '.implications-content h4 { color: #495057; margin-top: 20px; margin-bottom: 10px; }',
    '.implications-content strong { color: #2c3e50; }',
    '.badge { padding: 4px 8px; border-radius: 12px; font-weight: bold; }',
    '.badge-info { background-color: #17a2b8; color: white; }',
    '.badge-warning { background-color: #ffc107; color: #212529; }',
    '.badge-success { background-color: #28a745; color: white; }',
    '.alert { border: 1px solid transparent; border-radius: 4px; }',
    '.alert-warning { color: #856404; background-color: #fff3cd; border-color: #ffeaa7; }',
    '.alert-info { color: #0c5460; background-color: #d1ecf1; border-color: #b8daff; }',
    '</style>'
  )
  
  return(html_output)
}