# Dashboard Module (Updated with animation hooks)

dashboard_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # 1. Add this div for the parallax background effect.
    #    Its appearance and behavior are controlled by style.css and custom-animations.js
    div(id = "parallax-background"),
    
    # 2. The inline <style> tag has been removed. 
    #    All styles are now in the external style.css file for better organization.
    
    h1("Dashboard Overview", style = "color: #2c3e50; margin-bottom: 30px;"),
    
    # 3. Each major section is wrapped in a div with the 'fade-in-element' class.
    #    This class marks it for animation by our JavaScript.
    #    An inline style is used to add a delay for a pleasant, staggered effect.
    div(
      class = "fade-in-element",
      fluidRow(
        column(2, div(class = "value-box", h3(textOutput(ns("total_students"))), p("Total Students"))),
        column(2, div(class = "value-box", h3(textOutput(ns("total_teachers"))), p("Total Teachers"))),
        column(2, div(class = "value-box", h3(textOutput(ns("total_hours"))), p("Tutoring Hours"))),
        column(2, div(class = "value-box", h3(textOutput(ns("total_graduates"))), p("Graduates"))),
        column(2, div(class = "value-box", h3(textOutput(ns("total_amount"))), p("Partner Amount"))),
        column(2, div(class = "value-box", h3(textOutput(ns("total_facilities"))), p("Facilities")))
      )
    ),
    
    br(),
    
    div(
      class = "fade-in-element",
      style = "transition-delay: 200ms;", # This row will animate 200ms after the one above
      fluidRow(
        column(6, wellPanel(h4("Facilities Distribution", style = "color: #2c3e50;"), withSpinner(plotlyOutput(ns("facilities_chart"))))),
        column(6, wellPanel(h4("Partner Contributions", style = "color: #2c3e50;"), withSpinner(plotlyOutput(ns("partners_chart")))))
      )
    ),
    
    br(),
    
    h3("Detailed Analytics", style = "color: #2c3e50; margin-bottom: 20px;"),
    
    div(
      class = "fade-in-element",
      style = "transition-delay: 400ms;", # This row will animate 400ms after the first one
      fluidRow(
        column(4, wellPanel(h4("Top 5 Teachers (Most Tutor Time)", style = "color: #2c3e50;"), withSpinner(DT::dataTableOutput(ns("top_teachers_table"))))),
        column(4, wellPanel(h4("Poor Performing Students", style = "color: #2c3e50;"), selectInput(ns("competency_filter"), "Learning Competency Filter:", choices = NULL, selected = NULL), withSpinner(DT::dataTableOutput(ns("poor_students_table"))))),
        column(4, wellPanel(h4("Assessment Score Comparison", style = "color: #2c3e50;"), withSpinner(plotlyOutput(ns("assessment_comparison_chart")))))
      )
    )
  )
}

# IMPORTANT: The dashboard_server function does not need any changes.
# You can keep your existing server logic exactly as it is.
dashboard_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    
    # ... (Your existing server code remains unchanged here)
    
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
    output$total_students <- renderText({ dashboard_data()$total_students })
    output$total_teachers <- renderText({ dashboard_data()$total_teachers })
    output$total_hours <- renderText({ round(dashboard_data()$total_hours, 1) })
    output$total_graduates <- renderText({ dashboard_data()$total_graduates })
    output$total_amount <- renderText({ paste("₱", format(dashboard_data()$total_partner_amount, big.mark = ",", nsmall = 2)) })
    output$total_facilities <- renderText({ dbGetQuery(con, "SELECT COUNT(*) as count FROM facilities_table")$count })
    
    # Facilities Distribution Chart
    output$facilities_chart <- renderPlotly({
      facilities_data <- dbGetQuery(con, "
        SELECT type, COUNT(*) as count 
        FROM facilities_table 
        WHERE type IS NOT NULL 
        GROUP BY type
      ")
      
      if (nrow(facilities_data) > 0) {
        p <- plot_ly(facilities_data, 
                     x = ~type, 
                     y = ~count, 
                     type = 'bar',
                     marker = list(color = '#3498db'),
                     hovertemplate = '<b>%{x}</b><br>Count: %{y}<extra></extra>') %>%
          layout(title = "",
                 xaxis = list(title = "Facility Type"),
                 yaxis = list(title = "Count"))
        p
      } else {
        plotly_empty() %>%
          layout(title = "No data available")
      }
    })
    
    # Partners Chart
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
                     marker = list(color = '#e74c3c'),
                     hovertemplate = '<b>%{x}</b><br>Amount: ₱%{y}<extra></extra>') %>%
          layout(title = "",
                 xaxis = list(title = "Partners"),
                 yaxis = list(title = "Amount (₱)"))
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
                    options = list(pageLength = 5, dom = 't'),
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
                    options = list(pageLength = 5, dom = 't'),
                    rownames = FALSE)
    })
    
    # Assessment Comparison Chart
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
  })
}