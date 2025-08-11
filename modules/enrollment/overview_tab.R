#Class overview
class_overview_tab_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Class Overview",
    value = "view",
    icon = icon("eye"),
    br(),
    fluidRow(
      box(
        title = "All Remedial Class Enrollments", 
        status = "primary", 
        solidHeader = TRUE, 
        width = 12,
        
        fluidRow(
          column(6,
                 checkboxInput(ns("show_inactive"), "Show inactive enrollments", value = FALSE)
          ),
          column(6,
                 downloadButton(ns("download_enrollments"), "Download All Data", 
                                class = "btn-success", style = "float: right;")
          )
        ),
        
        hr(),
        DT::dataTableOutput(ns("all_enrollments_overview"))
      )
    )
  )
}

# =============================================================================
# SERVER MODULE
# =============================================================================

class_overview_tab_server <- function(id, con, refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    
    # Overview table for all enrollments
    output$all_enrollments_overview <- DT::renderDataTable({
      refresh_trigger()  # Dependency to trigger refresh
      
      all_enrollments <- if (input$show_inactive) {
        dbGetQuery(con, "
          SELECT 
            e.enrollment_id,
            e.quarter,
            e.subject,
            s.student_name,
            s.grade_level,
            s.section,
            e.enrollment_date,
            e.total_attendance,
            e.status
          FROM remedial_enrollments e
          LEFT JOIN full_student_registry s ON e.student_id = s.student_id
          ORDER BY e.quarter, e.subject, s.grade_level, s.student_name
        ")
      } else {
        get_enrollment_data(con)
      }
      
      if (nrow(all_enrollments) > 0) {
        DT::datatable(
          all_enrollments %>% select(-enrollment_id),
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            searching = TRUE
          ),
          caption = "All Remedial Class Enrollments"
        ) %>%
          DT::formatStyle("status",
                          backgroundColor = DT::styleEqual(c("Active", "Passed", "Inactive"), 
                                                           c("#d4edda", "#cce5ff", "#f8d7da")))
      } else {
        empty_df <- data.frame(Message = "No enrollment data found")
        DT::datatable(
          empty_df,
          options = list(searching = FALSE, paging = FALSE, info = FALSE)
        )
      }
    })
    
    # Download handler
    output$download_enrollments <- downloadHandler(
      filename = function() {
        paste("remedial_enrollments_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        all_data <- dbGetQuery(con, "
          SELECT 
            e.quarter,
            e.subject,
            s.student_name,
            s.grade_level,
            s.section,
            e.enrollment_date,
            e.total_attendance,
            e.status,
            s.Total_score_per,
            s.RMA_before_math_proficiency,
            s.before_reading_proficiency
          FROM remedial_enrollments e
          LEFT JOIN full_student_registry s ON e.student_id = s.student_id
          ORDER BY e.quarter, e.subject, s.grade_level, s.student_name
        ")
        write.csv(all_data, file, row.names = FALSE)
      }
    )
  })
}