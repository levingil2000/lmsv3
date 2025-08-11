#Student registry
student_registry_tab_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Student Registry",
    value = "registry",
    icon = icon("users"),
    br(),
    fluidRow(
      box(
        title = "Student Registry", 
        status = "primary", 
        solidHeader = TRUE, 
        width = 12,
        p("Complete student registry with test scores and proficiency levels."),
        DT::dataTableOutput(ns("remedial_student_registry"))
      )
    )
  )
}

# =============================================================================
# SERVER MODULE
# =============================================================================

student_registry_tab_server <- function(id, student_data) {
  moduleServer(id, function(input, output, session) {
    
    # Student Registry Table
    output$remedial_student_registry <- DT::renderDataTable({
      req(student_data())
      
      display_data <- student_data() %>%
        select(student_id, student_name, grade_level, section, Total_score_per,
               RMA_before_math_proficiency, before_reading_proficiency,
               starts_with("testq"))
      
      DT::datatable(
        display_data,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE
        ),
        caption = "Complete Student Registry"
      ) %>%
        DT::formatRound(columns = c("Total_score_per"), digits = 1)
    })
  })
}