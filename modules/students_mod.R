library(readr)
library(readxl)
source("modules/students/students_registry.R")
source("modules/students/students_analytics.R")
source("modules/students/students_helpers.R")

students_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1("Students Management", style = "color: #2c3e50; margin-bottom: 30px;"),
    
    tabsetPanel(
      # Student Registry Tab
      tabPanel("Student Registry", students_registry_ui(ns("registry"))),
      
      # Student Analytics Tab  
      tabPanel("Student Analytics", students_analytics_ui(ns("analytics")))
    )
  )
}

students_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    # Shared reactive values
    shared_values <- reactiveValues(
      refresh_trigger = 0,
      students_data = NULL
    )
    
    # Load students data whenever refresh is triggered
    observe({
      shared_values$refresh_trigger
      shared_values$students_data <- get_table_data(con, "students_registry")
    })
    
    # Initialize sub-modules
    students_registry_server("registry", con, shared_values)
    students_analytics_server("analytics", con, shared_values)
  })
}