#Graphs aand tables generated

# Filter data based on user selections
apply_class_filters <- function(data, filter_quarter = "all", filter_subject = "all", filter_grade_level = "all") {
  
  # Apply filters
  if (filter_quarter != "all") {
    data <- data %>% filter(quarter == filter_quarter)
  }
  
  if (filter_subject != "all") {
    data <- data %>% filter(subject == filter_subject)
  }
  
  if (filter_grade_level != "all") {
    data <- data %>% filter(grade_level == as.numeric(filter_grade_level))
  }
  
  return(data)
}

# Generate class list UI elements
generate_class_lists_ui <- function(filtered_data, session) {
  
  if (nrow(filtered_data) == 0) {
    return(div(
      h4("No students found with the selected filters.", 
         style = "text-align: center; color: #7f8c8d; margin: 50px 0;")
    ))
  }
  
  # Group data by quarter, subject, and grade level
  grouped_data <- filtered_data %>%
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
}

# Create data tables for class groups
create_class_data_tables <- function(filtered_data, output, session) {
  
  # Group data by quarter, subject, and grade level
  grouped_data <- filtered_data %>%
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
}