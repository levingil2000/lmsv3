source("modules/enrollment/enrollment_database_helpers.R")
source("modules/enrollment/enrollment_tab.R")
source("modules/enrollment/manage_class_tab.R")
source("modules/enrollment/overview_tab.R")
source("modules/enrollment/student_registry_tab.R")
source("modules/enrollment/enrollment_dashboard.R")

# =============================================================================
# MAIN UI MODULE
# =============================================================================
student_enrollment_ui <- function(id) {
  ns <- NS(id)
  
  tabsetPanel(
    id = ns("main_tabs"),
    type = "tabs",
    
    # Dashboard Tab
    dashboard_tab_ui(ns("dashboard")),
    
    # Student Registry Tab
    student_registry_tab_ui(ns("registry")),
    
    # Class Enrollment Tab
    enroll_students_tab_ui(ns("enrollment")),
    
    # Manage Enrollments Tab
    manage_classes_tab_ui(ns("management")),
    
    # View All Classes Tab
    class_overview_tab_ui(ns("overview"))
  )
}

# =============================================================================
# MAIN SERVER MODULE
# =============================================================================

student_enrollment_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize database
    create_enrollment_table(con)
    
    # Reactive values for managing state
    values <- reactiveValues(
      student_data = NULL,
      last_refresh = Sys.time()
    )
    
    # Load initial data
    observe({
      values$student_data <- get_student_registry(con)
    })
    
    # Create reactive for student data
    student_data <- reactive({
      values$student_data
    })
    
    # Create reactive function for triggering refreshes
    refresh_trigger <- reactiveVal(Sys.time())
    
    # Observe refresh trigger and update student data
    observeEvent(refresh_trigger(), {
      values$student_data <- get_student_registry(con)
      values$last_refresh <- Sys.time()
    })
    
    # Initialize all tab servers
    dashboard_tab_server("dashboard", con, refresh_trigger)
    
    student_registry_tab_server("registry", student_data, con)
    
    enroll_students_tab_server("enrollment", con, student_data, refresh_trigger)
    
    manage_classes_tab_server("management", con, refresh_trigger)
    
    class_overview_tab_server("overview", con, refresh_trigger)
  })
}