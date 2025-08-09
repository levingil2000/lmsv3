dashboard_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    tags$head(
      # External CSS & JS
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/aos/2.3.4/aos.css"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/aos/2.3.4/aos.js"),
      tags$link(rel = "stylesheet", href = "styles.css"),   # custom CSS
      tags$script(src = "scripts.js")                       # custom JS
    ),
    
    # Parallax background
    div(class = "parallax-container",
        div(class = "parallax-bg")
    ),
    
    # Summary Statistics Row with staggered animations
    fluidRow(
      column(2,
             div(class = "value-box",
                 "data-aos" = "fade-up",
                 "data-aos-delay" = "100",
                 h3(textOutput(ns("total_students"))),
                 p("Total Students")
             )
      ),
      column(2,
             div(class = "value-box",
                 "data-aos" = "fade-up", 
                 "data-aos-delay" = "200",
                 h3(textOutput(ns("total_teachers"))),
                 p("Total Teachers")
             )
      ),
      column(2,
             div(class = "value-box",
                 "data-aos" = "fade-up",
                 "data-aos-delay" = "300",
                 h3(textOutput(ns("total_hours"))),
                 p("Tutoring Hours")
             )
      ),
      column(2,
             div(class = "value-box",
                 "data-aos" = "fade-up",
                 "data-aos-delay" = "400",
                 h3(textOutput(ns("total_graduates"))),
                 p("Graduates")
             )
      ),
      column(2,
             div(class = "value-box",
                 "data-aos" = "fade-up",
                 "data-aos-delay" = "500",
                 h3(textOutput(ns("total_amount"))),
                 p("Partner Amount")
             )
      ),
      column(2,
             div(class = "value-box",
                 "data-aos" = "fade-up",
                 "data-aos-delay" = "600",
                 h3(textOutput(ns("total_facilities"))),
                 p("Facilities")
             )
      )
    ),
    
    br(),
    
    # Charts Row with slide-in animations
    fluidRow(
      column(6,
             div(class = "chart-container",
                 "data-aos" = "fade-right",
                 "data-aos-delay" = "700",
                 h4("Facilities Distribution"),
                 withSpinner(plotlyOutput(ns("facilities_chart")), 
                             type = 8, color = "#667eea")
             )
      ),
      column(6,
             div(class = "chart-container",
                 "data-aos" = "fade-left",
                 "data-aos-delay" = "800",
                 h4("Partner Contributions"),
                 withSpinner(plotlyOutput(ns("partners_chart")),
                             type = 8, color = "#764ba2")
             )
      )
    ),
    
    br(),
    
    # Detailed Analytics Section
    div("data-aos" = "zoom-in",
        "data-aos-delay" = "900",
        h3("Detailed Analytics", 
           style = "color: white; margin-bottom: 30px; text-align: center; 
               text-shadow: 2px 2px 4px rgba(0,0,0,0.3); font-weight: 600;")
    ),
    
    # First row: Top Teachers & Assessment Comparison
    fluidRow(
      class = "equal-height-row",
      column(6,
             div(class = "chart-container",
                 "data-aos" = "flip-left",
                 "data-aos-delay" = "1000",
                 h4("Top 5 Teachers (Most Tutor Time)"),
                 withSpinner(DT::dataTableOutput(ns("top_teachers_table")),
                             type = 8, color = "#667eea")
             )
      ),
      column(6,
             div(class = "chart-container",
                 "data-aos" = "flip-right",
                 "data-aos-delay" = "1200",
                 h4("Assessment Score Comparison"),
                 withSpinner(plotlyOutput(ns("assessment_comparison_chart")),
                             type = 8, color = "#27ae60")
             )
      )
    ),
    
    br(),
    
    # Second row: Poor Performing Students full width
    fluidRow(
      column(12,
             div(class = "chart-container",
                 "data-aos" = "flip-up",
                 "data-aos-delay" = "1100",
                 h4("Poor Performing Students"),
                 selectInput(ns("competency_filter"), "Learning Competency Filter:",
                             choices = NULL, selected = NULL),
                 withSpinner(DT::dataTableOutput(ns("poor_students_table")),
                             type = 8, color = "#e74c3c")
             )
      )
    ),
    
    # Add some breathing space
    br(), br()
  )
}

dashboard_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive data
    dashboard_data <- reactive({
      get_dashboard_stats(con)
    })
    
    # Update competency filter choices
    observe({
      competencies <- dbGetQuery(con, "SELECT DISTINCT learning_competency FROM assessment_table WHERE learning_competency IS NOT NULL")$learning_competency
      updateSelectInput(session, "competency_filter", 
                        choices = c("All" = "", competencies))
    })
    
    # Summary Statistics Outputs
    output$total_students <- renderText({
      dashboard_data()$total_students
    })
    
    output$total_teachers <- renderText({
      dashboard_data()$total_teachers
    })
    
    output$total_hours <- renderText({
      round(dashboard_data()$total_hours, 1)
    })
    
    output$total_graduates <- renderText({
      dashboard_data()$total_graduates
    })
    
    output$total_amount <- renderText({
      paste("₱", format(dashboard_data()$total_partner_amount, big.mark = ",", nsmall = 2))
    })
    
    output$total_facilities <- renderText({
      dbGetQuery(con, "SELECT COUNT(*) as count FROM facilities_table")$count
    })
    
    # Enhanced Facilities Distribution Chart
    output$facilities_chart <- renderPlotly({
      facilities_data <- dbGetQuery(con, "
        SELECT type, COUNT(*) as count 
        FROM facilities_table 
        WHERE type IS NOT NULL 
        GROUP BY type
      ")
      
      if (nrow(facilities_data) > 0) {
        colors <- c('#667eea', '#764ba2', '#f093fb', '#f5576c', '#4facfe', '#00f2fe')
        
        p <- plot_ly(facilities_data, 
                     x = ~type, 
                     y = ~count, 
                     type = 'bar',
                     marker = list(
                       color = colors[1:nrow(facilities_data)],
                       line = list(color = 'rgba(255, 255, 255, 0.6)', width = 2)
                     ),
                     hovertemplate = '<b>%{x}</b><br>Count: %{y}<extra></extra>') %>%
          layout(title = "",
                 xaxis = list(title = "Facility Type", 
                              titlefont = list(size = 14, color = '#2c3e50')),
                 yaxis = list(title = "Count",
                              titlefont = list(size = 14, color = '#2c3e50')),
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 paper_bgcolor = 'rgba(0,0,0,0)')
        p
      } else {
        plotly_empty() %>%
          layout(title = "No data available")
      }
    })
    
    # Enhanced Partners Chart
    output$partners_chart <- renderPlotly({
      partners_data <- dbGetQuery(con, "
        SELECT name, amount 
        FROM partners_table 
        WHERE amount > 0 
        ORDER BY amount DESC 
        LIMIT 10
      ")
      
      if (nrow(partners_data) > 0) {
        p <- plot_ly(partners_data, 
                     x = ~reorder(name, amount), 
                     y = ~amount, 
                     type = 'bar',
                     marker = list(
                       color = ~amount,
                       colorscale = list(c(0, '#f093fb'), c(1, '#f5576c')),
                       line = list(color = 'rgba(255, 255, 255, 0.6)', width = 2)
                     ),
                     hovertemplate = '<b>%{x}</b><br>Amount: ₱%{y}<extra></extra>') %>%
          layout(title = "",
                 xaxis = list(title = "Partners",
                              titlefont = list(size = 14, color = '#2c3e50')),
                 yaxis = list(title = "Amount (₱)",
                              titlefont = list(size = 14, color = '#2c3e50')),
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 paper_bgcolor = 'rgba(0,0,0,0)')
        p
      } else {
        plotly_empty() %>%
          layout(title = "No data available")
      }
    })
    
    # Top Teachers Table
    output$top_teachers_table <- DT::renderDataTable({
      top_teachers <- get_top_teachers(con)
      
      if (nrow(top_teachers) > 0) {
        top_teachers$total_hours <- round(top_teachers$total_hours, 2)
        colnames(top_teachers) <- c("Teacher Name", "Total Hours")
      }
      
      DT::datatable(top_teachers, 
                    options = list(
                      pageLength = 5, 
                      dom = 't',
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#34495e', 'color': '#fff'});",
                        "}"
                      )
                    ),
                    rownames = FALSE)
    })
    
    # Poor Performing Students Table
    output$poor_students_table <- DT::renderDataTable({
      poor_students <- get_poor_performing_students(con, input$competency_filter)
      
      if (nrow(poor_students) > 0) {
        poor_students$preassessment_score <- round(poor_students$preassessment_score, 2)
        poor_students$postassessment_score <- round(poor_students$postassessment_score, 2)
        colnames(poor_students) <- c("Student", "Subject", "Competency", "Pre-Score", "Post-Score")
      }
      
      DT::datatable(poor_students, 
                    options = list(
                      pageLength = 5, 
                      dom = 't',
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#34495e', 'color': '#fff'});",
                        "}"
                      )
                    ),
                    rownames = FALSE)
    })
    
    # Enhanced Assessment Comparison Chart
    output$assessment_comparison_chart <- renderPlotly({
      assessment_data <- dbGetQuery(con, "
        SELECT subject, 
               AVG(preassessment_score) as avg_pre, 
               AVG(postassessment_score) as avg_post
        FROM assessment_table 
        WHERE preassessment_score IS NOT NULL AND postassessment_score IS NOT NULL
        GROUP BY subject
      ")
      
      if (nrow(assessment_data) > 0) {
        p <- plot_ly(assessment_data) %>%
          add_trace(x = ~subject, y = ~avg_pre, type = 'bar', name = 'Pre-Assessment',
                    marker = list(color = '#e74c3c', 
                                  line = list(color = 'rgba(255, 255, 255, 0.6)', width = 2))) %>%
          add_trace(x = ~subject, y = ~avg_post, type = 'bar', name = 'Post-Assessment',
                    marker = list(color = '#27ae60',
                                  line = list(color = 'rgba(255, 255, 255, 0.6)', width = 2))) %>%
          layout(title = "",
                 xaxis = list(title = "Subject",
                              titlefont = list(size = 14, color = '#2c3e50')),
                 yaxis = list(title = "Average Score",
                              titlefont = list(size = 14, color = '#2c3e50')),
                 barmode = 'group',
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 paper_bgcolor = 'rgba(0,0,0,0)',
                 legend = list(
                   bgcolor = 'rgba(255, 255, 255, 0.8)',
                   bordercolor = 'rgba(255, 255, 255, 0.8)',
                   borderwidth = 2
                 ))
        p
      } else {
        plotly_empty() %>%
          layout(title = "No assessment data available")
      }
    })
  })
}