#Helper functions for dashboard generation

# Grade Level Distribution Chart
create_grade_distribution_chart <- function(dashboard_data) {
  grade_dist <- dashboard_data %>%
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
  
  return(p)
}

# Subject Distribution Chart
create_subject_distribution_chart <- function(dashboard_data) {
  subject_dist <- dashboard_data %>%
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
  
  return(p)
}

# Average Sessions by Subject Chart
create_avg_sessions_chart <- function(dashboard_data) {
  avg_sessions <- dashboard_data %>%
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
  
  return(p)
}