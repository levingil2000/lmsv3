
library(DBI)
library(RSQLite)
library(googlesheets4)
library(googledrive)

# Connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), "lms_database.sqlite")

# Query all the required metrics
TotStudents <- dbGetQuery(con, "SELECT COUNT(*) as count FROM remedial_enrollments")$count
TotTeachers <- dbGetQuery(con, "SELECT COUNT(*) as count FROM teacher_registry")$count
TotSessions <- dbGetQuery(con, "SELECT COUNT(*) as count FROM teacher_sessions")$count
TotHours <- dbGetQuery(con, "SELECT SUM(duration) as total FROM teacher_sessions")$total
TotGrad <- dbGetQuery(con, "SELECT COUNT(*) as count FROM remedial_enrollments WHERE status = 'Passed'")$count
ActFacility <- dbGetQuery(con, "SELECT COUNT(*) as count FROM facilities_table")$count
TotPartner <- dbGetQuery(con, "SELECT SUM(amount) as total FROM partners_table")$total

# Close database connection
dbDisconnect(con)

# Create the summary dataframe
summary_df <- data.frame(
  Variable = c("TotStudents", "TotTeachers", "TotSessions", "TotHours", "TotGrad", "ActFacility", "TotPartner"),
  Value = c(TotStudents, TotTeachers, TotSessions, TotHours, TotGrad, ActFacility, TotPartner)
)

# Create frequency distribution for RMA_before_math_proficiency
freq_before <- dbGetQuery(con, "
  SELECT grade_level, RMA_before_math_proficiency, COUNT(*) as frequency
  FROM full_student_registry
  GROUP BY grade_level, RMA_before_math_proficiency
  ORDER BY grade_level, RMA_before_math_proficiency
")

# Create frequency distribution for RMA_before_math_proficiency_end
freq_after <- dbGetQuery(con, "
  SELECT grade_level, RMA_before_math_proficiency_end, COUNT(*) as frequency
  FROM full_student_registry
  GROUP BY grade_level, RMA_before_math_proficiency_end
  ORDER BY grade_level, RMA_before_math_proficiency_end
")

# Create before/after math scores comparison
before_avg <- dbGetQuery(con, "SELECT AVG(Total_score_per) as avg_score FROM full_student_registry")$avg_score
after_avg <- dbGetQuery(con, "SELECT AVG(Total_score_per_end) as avg_score FROM full_student_registry")$avg_score

math_comparison <- data.frame(
  variable = c("Before", "After"),
  math_score = c(before_avg, after_avg)
)

# Print the dataframes to verify
print("Summary:")
print(summary_df)
print("Frequency Before:")
print(freq_before)
print("Frequency After:")
print(freq_after)
print("Math Comparison:")
print(math_comparison)

# Authenticate with Google (this will prompt for authentication if not already done)
gs4_auth()

# Create a new Google Sheet with multiple sheets
sheet_id <- gs4_create("LMS_Analysis")

# Write each table to a separate sheet
write_sheet(summary_df, ss = sheet_id, sheet = "Summary")
write_sheet(freq_before, ss = sheet_id, sheet = "Frequency_Before")
write_sheet(freq_after, ss = sheet_id, sheet = "Frequency_After")
write_sheet(math_comparison, ss = sheet_id, sheet = "Math_Comparison")

cat("All data uploaded to Google Sheets successfully!")