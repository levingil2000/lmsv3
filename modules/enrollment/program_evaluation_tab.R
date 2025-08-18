# =============================================================================
# PROGRAM EVALUATION TAB MODULE (REVISED)
# =============================================================================

program_evaluation_tab_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Program Evaluation",
    value = "program_eval",
    icon = icon("chart-bar"),
    
    # Include parallax and animations
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
    
    # Title & Grade Filter
    div("data-aos" = "fade-down", "data-aos-delay" = "100",
        box(
          title = "Program Evaluation Dashboard",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          p("Explore student proficiency growth in Mathematics and Reading across BOSY and EOSY."),
          
          fluidRow(
            column(4, offset = 4,
                   selectInput(ns("grade_filter"), 
                               "Filter by Grade Level:",
                               choices = c("All Grades" = "all"),
                               selected = "all",
                               width = "100%")
            )
          )
        )
    ),
    
    br(),
    
    # Mathematics Section
    fluidRow(
      column(12,
             div("data-aos" = "fade-right", "data-aos-delay" = "200",
                 h3("Mathematics Program Evaluation", class = "text-primary"),
                 hr()
             )
      )
    ),
    
    fluidRow(
      column(4,
             div(class = "chart-container",
                 "data-aos" = "fade-up", "data-aos-delay" = "300",
                 h4("BOSY Math Proficiency", class = "text-center"),
                 withSpinner(plotlyOutput(ns("bosy_math_plot"), height = "300px"), type = 8, color = "#3498db")
             )
      ),
      column(4,
             div(class = "chart-container",
                 "data-aos" = "fade-up", "data-aos-delay" = "400",
                 h4("EOSY Math Proficiency", class = "text-center"),
                 withSpinner(plotlyOutput(ns("eosy_math_plot"), height = "300px"), type = 8, color = "#9b59b6")
             )
      ),
      column(4,
             div(class = "chart-container",
                 "data-aos" = "fade-up", "data-aos-delay" = "500",
                 h4("Math Score Comparison", class = "text-center"),
                 withSpinner(plotlyOutput(ns("math_score_comparison"), height = "300px"), type = 8, color = "#2ecc71")
             )
      )
    ),
    
    br(),
    
    # Reading Section
    fluidRow(
      column(12,
             div("data-aos" = "fade-left", "data-aos-delay" = "600",
                 h3("📖 Reading Program Evaluation", class = "text-primary"),
                 hr()
             )
      )
    ),
    
    fluidRow(
      column(4,
             div(class = "chart-container",
                 "data-aos" = "fade-up", "data-aos-delay" = "700",
                 h4("BOSY Reading Proficiency", class = "text-center"),
                 withSpinner(plotlyOutput(ns("bosy_reading_plot"), height = "300px"), type = 8, color = "#f39c12")
             )
      ),
      column(4,
             div(class = "chart-container",
                 "data-aos" = "fade-up", "data-aos-delay" = "800",
                 h4("EOSY Reading Proficiency", class = "text-center"),
                 withSpinner(plotlyOutput(ns("eosy_reading_plot"), height = "300px"), type = 8, color = "#e74c3c")
             )
      ),
      column(4,
             div(class = "chart-container",
                 "data-aos" = "fade-up", "data-aos-delay" = "900",
                 h4("Reading Accuracy Comparison", class = "text-center"),
                 withSpinner(plotlyOutput(ns("reading_accuracy_comparison"), height = "300px"), type = 8, color = "#16a085")
             )
      )
    ),
    
    br(),
    
    # Legend Section
    div("data-aos" = "zoom-in", "data-aos-delay" = "1000",
        box(
          title = "Legend for Proficiency Levels",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          p(HTML("<b>EN:</b> Emerging (Not Proficient) &nbsp;&nbsp; 
                 <b>EL:</b> Emerging (Low Proficient) &nbsp;&nbsp; 
                 <b>D:</b> Developing (Nearly Proficient) &nbsp;&nbsp; 
                 <b>T:</b> Transitioning (Proficient) &nbsp;&nbsp; 
                 <b>G:</b> At Grade Level (Highly Proficient)"))
        )
    ),
    
    br(), br()
  )
}

# Server Module
program_evaluation_tab_server <- function(id, con, refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Define proficiency levels, abbreviations, and a mapping for consistent ordering and labeling
    proficiency_levels <- c(
      "Emerging (Not Proficient)",
      "Emerging (Low Proficient)", 
      "Developing (Nearly Proficient)",
      "Transitioning (Proficient)",
      "At Grade Level (Highly Proficient)"
    )
    
    proficiency_abbreviations <- c("EN", "EL", "D", "T", "G")
    
    proficiency_map <- setNames(proficiency_abbreviations, proficiency_levels)
    
    # Get available grade levels for filter
    observe({
      req(con)
      
      grades_query <- "
        SELECT DISTINCT s.grade_level
        FROM full_student_registry s
        INNER JOIN remedial_enrollments e ON s.student_id = e.student_id
        WHERE e.status IN ('Active', 'Passed')
        ORDER BY s.grade_level
      "
      
      grades <- dbGetQuery(con, grades_query)
      
      choices <- c("All Grades" = "all")
      if (nrow(grades) > 0) {
        grade_choices <- setNames(grades$grade_level, paste("Grade", grades$grade_level))
        choices <- c(choices, grade_choices)
      }
      
      updateSelectInput(session, "grade_filter", choices = choices)
    })
    
    # Reactive data for math enrollments
    math_data <- reactive({
      req(con)
      
      base_query <- "
        SELECT s.student_id, s.student_name, s.grade_level, s.section,
               s.RMA_before_math_proficiency, s.RMA_before_math_proficiency_end,
               s.Total_score_per, s.Total_score_per_end
        FROM full_student_registry s
        INNER JOIN remedial_enrollments e ON s.student_id = e.student_id
        WHERE (e.subject IN ('Math', 'Mathematics', 'math', 'mathematics')) 
          AND e.status IN ('Active', 'Passed')
      "
      
      params <- list()
      if (!is.null(input$grade_filter) && input$grade_filter != "all") {
        base_query <- paste(base_query, "AND s.grade_level = ?")
        params <- list(as.numeric(input$grade_filter))
      }
      
      if (length(params) > 0) {
        dbGetQuery(con, base_query, params)
      } else {
        dbGetQuery(con, base_query)
      }
    })
    
    # Reactive data for reading enrollments
    reading_data <- reactive({
      req(con)
      
      base_query <- "
        SELECT s.student_id, s.student_name, s.grade_level, s.section,
               s.before_reading_proficiency, s.after_reading_proficiency,
               s.reading_accuracy, s.reading_accuracy_after
        FROM full_student_registry s
        INNER JOIN remedial_enrollments e ON s.student_id = e.student_id
        WHERE (e.subject IN ('English', 'Reading', 'english', 'reading')) 
          AND e.status IN ('Active', 'Passed')
      "
      
      params <- list()
      if (!is.null(input$grade_filter) && input$grade_filter != "all") {
        base_query <- paste(base_query, "AND s.grade_level = ?")
        params <- list(as.numeric(input$grade_filter))
      }
      
      if (length(params) > 0) {
        dbGetQuery(con, base_query, params)
      } else {
        dbGetQuery(con, base_query)
      }
    })
    
    # Helper function to create an empty plot with a message
    create_empty_plot <- function(message) {
      plot_ly() %>%
        add_annotations(
          text = message,
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16, color = "gray")
        ) %>%
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        ) %>%
        config(displayModeBar = FALSE)
    }
    
    # BOSY Math Plot
    output$bosy_math_plot <- renderPlotly({
      data <- math_data()
      if (is.null(data) || nrow(data) == 0) {
        return(create_empty_plot("No math enrollment data"))
      }
      
      valid_prof <- data$RMA_before_math_proficiency[!is.na(data$RMA_before_math_proficiency) & data$RMA_before_math_proficiency != ""]
      if (length(valid_prof) == 0) {
        return(create_empty_plot("No proficiency data"))
      }
      
      prof_dist <- table(factor(data$RMA_before_math_proficiency, levels = proficiency_levels))
      
      plot_data <- data.frame(
        Proficiency = names(prof_dist),
        Frequency = as.numeric(prop.table(prof_dist))
      )
      
      # Add abbreviations for plotting
      plot_data$Abbreviation <- proficiency_map[plot_data$Proficiency]
      
      p <- plot_ly(
        data = plot_data,
        x = ~factor(Abbreviation, levels = proficiency_abbreviations),
        y = ~Frequency,
        type = "bar",
        text = ~Proficiency, # Use full name for hover
        marker = list(color = "#3498db", opacity = 0.8),
        hovertemplate = "<b>%{text}</b><br>Frequency: %{y:.2%}<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "Proficiency Level"),
          yaxis = list(title = "Relative Frequency", range = c(0, 1), tickformat = ".1%"),
          margin = list(b = 50, l = 60, r = 20, t = 20),
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE)
      
      return(p)
    })
    
    # EOSY Math Plot
    output$eosy_math_plot <- renderPlotly({
      data <- math_data()
      if (is.null(data) || nrow(data) == 0) {
        return(create_empty_plot("No math enrollment data"))
      }
      
      valid_prof <- data$RMA_before_math_proficiency_end[!is.na(data$RMA_before_math_proficiency_end) & data$RMA_before_math_proficiency_end != ""]
      if (length(valid_prof) == 0) {
        return(create_empty_plot("No end-of-year data"))
      }
      
      prof_dist <- table(factor(data$RMA_before_math_proficiency_end, levels = proficiency_levels))
      
      plot_data <- data.frame(
        Proficiency = names(prof_dist),
        Frequency = as.numeric(prop.table(prof_dist))
      )
      
      plot_data$Abbreviation <- proficiency_map[plot_data$Proficiency]
      
      p <- plot_ly(
        data = plot_data,
        x = ~factor(Abbreviation, levels = proficiency_abbreviations),
        y = ~Frequency,
        type = "bar",
        text = ~Proficiency,
        marker = list(color = "#27ae60", opacity = 0.8),
        hovertemplate = "<b>%{text}</b><br>Frequency: %{y:.2%}<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "Proficiency Level"),
          yaxis = list(title = "Relative Frequency", range = c(0, 1), tickformat = ".1%"),
          margin = list(b = 50, l = 60, r = 20, t = 20),
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE)
      
      return(p)
    })
    
    # Math Score Comparison
    output$math_score_comparison <- renderPlotly({
      data <- math_data()
      req(nrow(data) > 0)
      
      before_avg <- mean(data$Total_score_per, na.rm = TRUE)
      after_avg <- mean(data$Total_score_per_end, na.rm = TRUE)
      
      # Fallback if 'after' data is missing
      if (is.nan(before_avg)) before_avg <- 0
      if (is.nan(after_avg)) after_avg <- before_avg
      
      comp_data <- data.frame(
        Period = c("Before", "After"),
        Score = c(before_avg, after_avg)
      )
      
      # Ensure correct "Before" -> "After" ordering
      comp_data$Period <- factor(comp_data$Period, levels = c("Before", "After"))
      
      colors <- if (after_avg > before_avg) c("#e74c3c", "#27ae60") else c("#f39c12", "#e67e22")
      
      plot_ly(
        data = comp_data,
        x = ~Period,
        y = ~Score,
        type = "bar",
        marker = list(color = colors, opacity = 0.8),
        text = ~paste0(round(Score, 1), "%"),
        textposition = "outside",
        textfont = list(size = 14, color = "black"),
        hovertemplate = "<b>%{x}</b><br>Average Score: %{y:.1f}%<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "", tickfont = list(size = 12, color = "black")),
          yaxis = list(title = "Average Score (%)", range = c(0, max(comp_data$Score, 100) * 1.2)),
          margin = list(b = 50, l = 60, r = 20, t = 50),
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # BOSY Reading Plot
    output$bosy_reading_plot <- renderPlotly({
      data <- reading_data()
      if (is.null(data) || nrow(data) == 0) {
        return(create_empty_plot("No reading enrollment data"))
      }
      
      valid_prof <- data$before_reading_proficiency[!is.na(data$before_reading_proficiency) & data$before_reading_proficiency != ""]
      if (length(valid_prof) == 0) {
        return(create_empty_plot("No proficiency data"))
      }
      
      prof_dist <- table(factor(data$before_reading_proficiency, levels = proficiency_levels))
      
      plot_data <- data.frame(
        Proficiency = names(prof_dist),
        Frequency = as.numeric(prop.table(prof_dist))
      )
      
      plot_data$Abbreviation <- proficiency_map[plot_data$Proficiency]
      
      p <- plot_ly(
        data = plot_data,
        x = ~factor(Abbreviation, levels = proficiency_abbreviations),
        y = ~Frequency,
        type = "bar",
        text = ~Proficiency,
        marker = list(color = "#9b59b6", opacity = 0.8),
        hovertemplate = "<b>%{text}</b><br>Frequency: %{y:.2%}<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "Proficiency Level"),
          yaxis = list(title = "Relative Frequency", range = c(0, 1), tickformat = ".1%"),
          margin = list(b = 50, l = 60, r = 20, t = 20),
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE)
      
      return(p)
    })
    
    # EOSY Reading Plot
    output$eosy_reading_plot <- renderPlotly({
      data <- reading_data()
      if (is.null(data) || nrow(data) == 0) {
        return(create_empty_plot("No reading enrollment data"))
      }
      
      valid_prof <- data$after_reading_proficiency[!is.na(data$after_reading_proficiency) & data$after_reading_proficiency != ""]
      if (length(valid_prof) == 0) {
        return(create_empty_plot("No end-of-year data"))
      }
      
      prof_dist <- table(factor(data$after_reading_proficiency, levels = proficiency_levels))
      
      plot_data <- data.frame(
        Proficiency = names(prof_dist),
        Frequency = as.numeric(prop.table(prof_dist))
      )
      
      plot_data$Abbreviation <- proficiency_map[plot_data$Proficiency]
      
      p <- plot_ly(
        data = plot_data,
        x = ~factor(Abbreviation, levels = proficiency_abbreviations),
        y = ~Frequency,
        type = "bar",
        text = ~Proficiency,
        marker = list(color = "#1abc9c", opacity = 0.8),
        hovertemplate = "<b>%{text}</b><br>Frequency: %{y:.2%}<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "Proficiency Level"),
          yaxis = list(title = "Relative Frequency", range = c(0, 1), tickformat = ".1%"),
          margin = list(b = 50, l = 60, r = 20, t = 20),
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE)
      
      return(p)
    })
    
    # Reading Accuracy Comparison
    output$reading_accuracy_comparison <- renderPlotly({
      data <- reading_data()
      req(nrow(data) > 0)
      
      before_avg <- mean(data$reading_accuracy, na.rm = TRUE)
      after_avg <- mean(data$reading_accuracy_after, na.rm = TRUE)
      
      if (is.nan(before_avg)) before_avg <- 0
      if (is.nan(after_avg)) after_avg <- before_avg
      
      comp_data <- data.frame(
        Period = c("Before", "After"),
        Accuracy = c(before_avg, after_avg)
      )
      
      # Ensure correct "Before" -> "After" ordering
      comp_data$Period <- factor(comp_data$Period, levels = c("Before", "After"))
      
      colors <- if (after_avg > before_avg) c("#e74c3c", "#27ae60") else c("#f39c12", "#e67e22")
      
      plot_ly(
        data = comp_data,
        x = ~Period,
        y = ~Accuracy,
        type = "bar",
        marker = list(color = colors, opacity = 0.8),
        text = ~paste0(round(Accuracy, 1), "%"),
        textposition = "outside",
        textfont = list(size = 14, color = "black"),
        hovertemplate = "<b>%{x}</b><br>Average Accuracy: %{y:.1f}%<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "", tickfont = list(size = 12, color = "black")),
          yaxis = list(title = "Average Reading Accuracy (%)", range = c(0, max(comp_data$Accuracy, 100) * 1.2)),
          margin = list(b = 50, l = 60, r = 20, t = 50),
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # Update plots when refresh trigger changes
    observeEvent(refresh_trigger(), {
      math_data()
      reading_data()
    })
  })
}