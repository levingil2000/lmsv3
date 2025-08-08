# Initialize sample data for testing the tutorial program management system

initialize_sample_data <- function(con) {
  
  # Sample Students
  sample_students <- data.frame(
    student_name = c("Juan Dela Cruz", "Maria Santos", "Jose Rizal", "Ana Garcia", "Pedro Gonzales"),
    grade_level = c(7, 8, 9, 7, 8),
    section = c("Mabini", "Bonifacio", "Rizal", "Mabini", "Bonifacio"),
    LRN = c("123456789012", "123456789013", "123456789014", "123456789015", "123456789016"),
    address = c("Brgy. 1, Macabud", "Brgy. 2, Macabud", "Brgy. 3, Macabud", "Brgy. 1, Macabud", "Brgy. 2, Macabud"),
    narrative = c("Needs support in Math", "Excels in English", "Good in Science", "Struggles with reading", "Active participant"),
    parent_name = c("Roberto Dela Cruz", "Carmen Santos", "Antonio Rizal", "Elena Garcia", "Miguel Gonzales"),
    parent_contact = c("09171234567", "09181234567", "09191234567", "09201234567", "09211234567"),
    consent_given = c(TRUE, TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  
  # Sample Teachers
  sample_teachers <- data.frame(
    name = c("Ms. Rodriguez", "Mr. Fernandez", "Mrs. Villanueva", "Mr. Mendoza"),
    teacherID = c("TCHR001", "TCHR002", "TCHR003", "TCHR004"),
    subject_handled = c("Mathematics", "English", "Science", "Filipino"),
    year_level = c(7, 8, 9, 7),
    teacher_status = c("Active", "Active", "Active", "Active"),
    stringsAsFactors = FALSE
  )
  
  # Sample Enrollments
  sample_enrollments <- data.frame(
    student_name = c("Juan Dela Cruz", "Maria Santos", "Jose Rizal", "Ana Garcia", "Pedro Gonzales"),
    LRN = c("123456789012", "123456789013", "123456789014", "123456789015", "123456789016"),
    subject = c("Mathematics", "English", "Science", "Mathematics", "English"),
    preassessment = c(65, 78, 82, 55, 70),
    date_start = c("2024-01-15", "2024-01-15", "2024-01-16", "2024-01-16", "2024-01-17"),
    status = c("Active", "Active", "Active", "Active", "Active"),
    stringsAsFactors = FALSE
  )
  
  # Sample Teacher Sessions
  sample_sessions <- data.frame(
    session_id = c("SES_20240115_001", "SES_20240116_001", "SES_20240117_001", "SES_20240118_001"),
    teacher_id = c("TCHR001", "TCHR002", "TCHR003", "TCHR001"),
    date = c("2024-01-15", "2024-01-16", "2024-01-17", "2024-01-18"),
    duration = c(2.0, 1.5, 2.5, 1.0),
    learning_competency = c("Basic Algebra", "Reading Comprehension", "Scientific Method", "Geometry Basics"),
    subject = c("Mathematics", "English", "Science", "Mathematics"),
    grade_level = c(7, 8, 9, 7),
    list_of_students = c("Juan Dela Cruz, Ana Garcia", "Maria Santos, Pedro Gonzales", "Jose Rizal", "Juan Dela Cruz"),
    stringsAsFactors = FALSE
  )
  
  # Sample Assessments
  sample_assessments <- data.frame(
    assessment_id = c("ASS_20240115_001", "ASS_20240115_002", "ASS_20240115_003", "ASS_20240115_004", "ASS_20240115_005"),
    student_LRN = c("123456789012", "123456789013", "123456789014", "123456789015", "123456789016"),
    subject = c("Mathematics", "English", "Science", "Mathematics", "English"),
    learning_competency = c("Basic Algebra", "Reading Comprehension", "Scientific Method", "Geometry Basics", "Grammar Rules"),
    grade_level = c(7, 8, 9, 7, 8),
    preassessment_score = c(65, 78, 82, 55, 70),
    postassessment_score = c(85, 88, 90, 75, 82),
    stringsAsFactors = FALSE
  )
  
  # Sample Facilities
  sample_facilities <- data.frame(
    FR_ID = c("FAC_001", "FAC_002", "FAC_003", "FAC_004", "FAC_005"),
    name = c("Computer Lab A", "Projector Unit 1", "Whiteboard Set", "Audio System", "Learning Materials Set"),
    description = c("20-unit computer laboratory", "LCD projector for presentations", "Magnetic whiteboard with markers", "Sound system for audio lessons", "Books and worksheets"),
    status = c("Available", "In Use", "Available", "Under Maintenance", "Available"),
    type = c("Equipment", "Equipment", "Furniture", "Equipment", "Materials"),
    amount = c(250000, 35000, 15000, 25000, 10000),
    stringsAsFactors = FALSE
  )
  
  # Sample Partners
  sample_partners <- data.frame(
    partner_ID = c("PTN_001", "PTN_002", "PTN_003", "PTN_004"),
    name = c("Macabud Barangay Council", "Local Business Association", "Parent-Teacher Association", "Alumni Association"),
    amount = c(50000, 75000, 25000, 100000),
    service_cash_fixture_provided = c("Cash donation for equipment", "Computer units and cash", "Learning materials", "Scholarship fund"),
    date_given = c("2024-01-10", "2024-01-15", "2024-01-20", "2024-01-25"),
    contact_person_partner = c("Brgy. Captain Santos", "Mr. Dela Rosa", "Mrs. Hernandez", "Ms. Aquino"),
    contact_no_partner = c("09171111111", "09182222222", "09193333333", "09204444444"),
    stringsAsFactors = FALSE
  )
  
  # Insert sample data
  tryCatch({
    # Insert students
    dbWriteTable(con, "students_registry", sample_students, append = TRUE, row.names = FALSE)
    
    # Insert teachers  
    dbWriteTable(con, "teacher_registry", sample_teachers, append = TRUE, row.names = FALSE)
    
    # Insert enrollments
    dbWriteTable(con, "enrollment_table", sample_enrollments, append = TRUE, row.names = FALSE)
    
    # Insert sessions
    dbWriteTable(con, "teacher_sessions", sample_sessions, append = TRUE, row.names = FALSE)
    
    # Insert assessments
    dbWriteTable(con, "assessment_table", sample_assessments, append = TRUE, row.names = FALSE)
    
    # Insert facilities
    dbWriteTable(con, "facilities_table", sample_facilities, append = TRUE, row.names = FALSE)
    
    # Insert partners
    dbWriteTable(con, "partners_table", sample_partners, append = TRUE, row.names = FALSE)
    
    cat("Sample data initialized successfully!\n")
    
  }, error = function(e) {
    cat("Error initializing sample data:", e$message, "\n")
  })
}

# Function to check if tables are empty and initialize if needed
check_and_initialize_data <- function(con) {
  students_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM students_registry")$count
  
  if (students_count == 0) {
    cat("No existing data found. Initializing sample data...\n")
    initialize_sample_data(con)
  } else {
    cat("Existing data found. Sample data initialization skipped.\n")
  }
}