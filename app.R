# 1. Load libraries and source modules/helpers
library(shiny)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)
library(shinydashboard)
library(plotly)
library(shinycssloaders)
library(shinyWidgets)
library(shinyBS)  # for modals
library(shinyjs)  # for show/hide functionality

source("modules/dashboard_mod.R")
source("modules/students_mod.R")
source("modules/teachers_mod.R")
source("modules/facilities_mod.R")
source("modules/partners_mod.R")
source("modules/sessions_mod.R")
source("modules/enrollment_mod.R")
source("modules/timeline_mod.R")
source("R/session_student_teacher_stitch.R") #helper functions to stitch sessions, teachers and students
source("R/db_helpers.R")
source("R/report_helpers.R")


# 2. UI: Define the navigation layout
ui <- navbarPage(
  title = tags$div(style = "display: flex; align-items: center;",
                   tags$img(src = "logo.png", height = "50px", style = "margin-right: 10px;"),
                   tags$div(
                     "Project ARAL Management System",
                     tags$br(),
                     tags$sub("Macabud National High School")
                   )
  ),
  theme = "styles.css",
  tabPanel("Dashboard", dashboard_ui("dashboard")),
  tabPanel("Students", students_ui("students")),
  tabPanel("Teachers", teachers_ui("teachers")),
  tabPanel("Facilities", facilities_ui("facilities")),
  tabPanel("Partners", partners_ui("partners")),
  tabPanel("Sessions", sessions_ui("sessions")),
  tabPanel("Student Enrollment", student_enrollment_ui("student_enrollment") ),
  tabPanel("Events", timeline_ui("timeline"))
)

# 3. Server: Call the server logic for each module
server <- function(input, output, session) {
  # Establish a single database connection for the app session
  con <- db_connect("lms_database.sqlite")
  
  # Close connection when the app stops
  onStop(function() {
    dbDisconnect(con)
  })
  
  dashboard_server("dashboard", con)
  students_server("students", con)
  teachers_server("teachers", con)
  facilities_server("facilities", con)
  partners_server("partners", con)
  sessions_server("sessions", con)
  student_enrollment_server("student_enrollment", con)
  timeline_server("timeline", con)
  
}

# 4. Run the app
shinyApp(ui, server)