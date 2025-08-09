students_analytics_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    
    # Filters
    create_analytics_filters(ns),
    
    # Summary Cards and Charts
    create_analytics_dashboard(ns)
  )
}

students_analytics_server <- function(id, con, shared_values) {
  moduleServer(id, function(input, output, session) {
    # Update filter choices
    observe({
      update_filter_choices(input, output, session, con, shared_values$students_data)
    })
    
    # Analytics outputs
    output$total_sessions <- renderText({
      calculate_total_sessions(con, input)
    })
    
    output$sessions_by_subject_chart <- renderPlotly({
      create_sessions_by_subject_chart(con, input)
    })
    
    output$sessions_by_competency_chart <- renderPlotly({
      create_sessions_by_competency_chart(con, input)
    })
    
    output$student_assessment_chart <- renderPlotly({
      create_student_assessment_chart(con, input)
    })
  })
}