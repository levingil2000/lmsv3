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
          title = "Class Lists by Subject, Quarter, and Grade",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          p("View organized class lists grouped by subject, quarter, and grade level. Each table shows students enrolled in that specific class configuration."),
          
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
# =============================================================================
# SERVER MODULE
# =============================================================================

dashboard_tab_server <- function(id, con, refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    
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
    
    # Subject Distribution Chart (now a bar chart)
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
    
    # Class Lists Generation
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