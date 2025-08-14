# =============================================================================
# FIXED PARTNERS HELPERS - ERROR RESOLUTION
# =============================================================================

# The main issue is likely in the grepl() calls where NA values or empty vectors
# are being passed. Here's the corrected version with proper error handling:

# =============================================================================
# 1. PARTNERS DASHBOARD SUMMARY CREATOR (FIXED)
# =============================================================================

create_partners_dashboard_summary <- function(partners_data) {
  data <- partners_data
  
  # Basic statistics with better NA handling
  total_partners <- nrow(data)
  total_amount <- sum(data$amount, na.rm = TRUE)
  avg_contribution <- if(total_partners > 0) round(mean(data$amount, na.rm = TRUE), 2) else 0
  
  # Safer categorization with explicit NA handling
  service_column <- data$service_cash_fixture_provided
  service_column[is.na(service_column)] <- ""  # Replace NA with empty string
  
  cash_donations <- sum(grepl("cash|money|₱|peso", tolower(service_column)), na.rm = TRUE)
  service_donations <- sum(!grepl("cash|money|₱|peso", tolower(service_column)) & 
                             service_column != "", na.rm = TRUE)
  
  # Monthly contribution trends with better date handling
  monthly_trends <- data %>%
    filter(!is.na(date_given) & !is.na(amount) & amount > 0) %>%
    mutate(
      date_given = as.Date(date_given),
      year_month = format(date_given, "%Y-%m")
    ) %>%
    filter(!is.na(year_month)) %>%  # Additional filter for valid dates
    group_by(year_month) %>%
    summarise(
      total_amount = sum(amount, na.rm = TRUE),
      count = n(),
      .groups = 'drop'
    ) %>%
    arrange(year_month)
  
  # Top contributors with better filtering
  top_partners <- data %>%
    filter(!is.na(amount) & amount > 0) %>%
    arrange(desc(amount)) %>%
    head(10)
  
  # Donation type distribution with safer string operations
  donation_types <- data %>%
    filter(!is.na(service_cash_fixture_provided) & service_cash_fixture_provided != "") %>%
    mutate(
      service_clean = tolower(ifelse(is.na(service_cash_fixture_provided), "", service_cash_fixture_provided)),
      type = case_when(
        grepl("cash|money|₱|peso", service_clean) ~ "Cash",
        grepl("equipment|computer|laptop|projector", service_clean) ~ "Equipment",
        grepl("service|training|consultation", service_clean) ~ "Service",
        grepl("book|material|supply", service_clean) ~ "Materials",
        TRUE ~ "Other"
      )
    ) %>%
    count(type)
  
  # Recent activity with safer date operations
  recent_cutoff <- Sys.Date() - 180
  recent_data <- data %>%
    filter(!is.na(date_given)) %>%
    mutate(date_given = as.Date(date_given)) %>%
    filter(!is.na(date_given) & date_given >= recent_cutoff)
  
  recent_partners <- nrow(recent_data)
  recent_amount <- sum(recent_data$amount, na.rm = TRUE)
  
  list(
    module_type = "partners",
    total_partners = total_partners,
    total_amount = total_amount,
    avg_contribution = avg_contribution,
    cash_donations = cash_donations,
    service_donations = service_donations,
    monthly_trends = monthly_trends,
    top_partners = top_partners,
    donation_types = donation_types,
    recent_partners = recent_partners,
    recent_amount = recent_amount,
    data = data
  )
}

# =============================================================================
# 2. FIXED WORD DOCUMENT GENERATION HELPERS
# =============================================================================

# Helper: Key Partnership Metrics Section (FIXED)
add_partnership_metrics_section <- function(doc, partners_summary) {
  doc <- doc %>%
    body_add_par("KEY PARTNERSHIP METRICS", style = "heading 2")
  
  # Calculate additional metrics with safety checks
  total_partners <- partners_summary$total_partners
  cash_donations <- partners_summary$cash_donations
  service_donations <- partners_summary$service_donations
  recent_partners <- partners_summary$recent_partners
  
  cash_percentage <- if(total_partners > 0) round((cash_donations / total_partners) * 100, 1) else 0
  service_percentage <- if(total_partners > 0) round((service_donations / total_partners) * 100, 1) else 0
  recent_activity_rate <- if(total_partners > 0) round((recent_partners / total_partners) * 100, 1) else 0
  
  metrics_data <- data.frame(
    Metric = c("Total Registered Partners", "Total Contributions Received", "Average Contribution per Partner", 
               "Cash Donations", "Service/Equipment Donations", "Recent Activity Rate (6 months)",
               "Partnership Engagement Score"),
    Value = c(
      as.character(partners_summary$total_partners),
      paste0("₱", format(partners_summary$total_amount, big.mark = ",")),
      paste0("₱", format(partners_summary$avg_contribution, big.mark = ",")),
      paste0(cash_donations, " (", cash_percentage, "%)"),
      paste0(service_donations, " (", service_percentage, "%)"),
      paste0(recent_partners, " (", recent_activity_rate, "%)"),
      ifelse(recent_activity_rate > 60, "Excellent", 
             ifelse(recent_activity_rate > 40, "Good", "Needs Improvement"))
    ),
    stringsAsFactors = FALSE
  )
  
  metrics_table <- flextable(metrics_data) %>%
    theme_booktabs() %>%
    autofit() %>%
    bg(part = "header", bg = "#2ecc71") %>%
    color(part = "header", color = "white") %>%
    bold(part = "header")
  
  doc <- doc %>%
    body_add_flextable(metrics_table) %>%
    body_add_par("", style = "Normal")
  
  return(doc)
}

# Helper: Detailed Partner Directory (FIXED)
add_detailed_partner_directory <- function(doc, partners_summary) {
  doc <- doc %>%
    body_add_par("DETAILED PARTNER DIRECTORY", style = "heading 2") %>%
    body_add_par("Complete listing of all registered partners with contribution details and contact information.", style = "Normal") %>%
    body_add_par("", style = "Normal")
  
  # Group partners by contribution type with better error handling
  if (nrow(partners_summary$data) > 0) {
    # Prepare service column safely
    data_clean <- partners_summary$data %>%
      mutate(
        service_clean = ifelse(is.na(service_cash_fixture_provided), "", 
                               tolower(service_cash_fixture_provided))
      )
    
    # Cash contributors with safer filtering
    cash_partners <- data_clean %>%
      filter(grepl("cash|money|₱|peso", service_clean)) %>%
      filter(!is.na(amount) & amount > 0) %>%
      arrange(desc(amount))
    
    if (nrow(cash_partners) > 0) {
      doc <- doc %>%
        body_add_par("Cash Contributors", style = "heading 3")
      
      cash_data <- cash_partners %>%
        head(20) %>%  # Limit to top 20 to keep document manageable
        select(
          Partner_Name = name,
          Partner_ID = partner_ID,
          Amount = amount,
          Date_Given = date_given,
          Contact_Person = contact_person_partner
        ) %>%
        mutate(
          Partner_Name = ifelse(is.na(Partner_Name), "N/A", Partner_Name),
          Partner_ID = ifelse(is.na(Partner_ID), "N/A", Partner_ID),
          Amount = paste0("₱", format(ifelse(is.na(Amount), 0, Amount), big.mark = ",")),
          Date_Given = ifelse(is.na(Date_Given), "N/A", as.character(Date_Given)),
          Contact_Person = ifelse(is.na(Contact_Person) | Contact_Person == "", "N/A", Contact_Person)
        )
      
      cash_table <- flextable(cash_data) %>%
        theme_booktabs() %>%
        autofit() %>%
        bg(part = "header", bg = "#27ae60") %>%
        color(part = "header", color = "white") %>%
        bold(part = "header")
      
      doc <- doc %>%
        body_add_flextable(cash_table) %>%
        body_add_par("", style = "Normal")
    }
    
    # Service/Equipment contributors with safer filtering
    service_partners <- data_clean %>%
      filter(!grepl("cash|money|₱|peso", service_clean),
             service_clean != "") %>%
      arrange(desc(ifelse(is.na(amount), 0, amount)))
    
    if (nrow(service_partners) > 0) {
      doc <- doc %>%
        body_add_par("Service & Equipment Contributors", style = "heading 3")
      
      service_data <- service_partners %>%
        head(20) %>%  # Limit to top 20
        select(
          Partner_Name = name,
          Partner_ID = partner_ID,
          Service_Provided = service_cash_fixture_provided,
          Date_Given = date_given,
          Contact_Person = contact_person_partner
        ) %>%
        mutate(
          Partner_Name = ifelse(is.na(Partner_Name), "N/A", Partner_Name),
          Partner_ID = ifelse(is.na(Partner_ID), "N/A", Partner_ID),
          Service_Provided = ifelse(is.na(Service_Provided) | Service_Provided == "", "N/A", Service_Provided),
          Date_Given = ifelse(is.na(Date_Given), "N/A", as.character(Date_Given)),
          Contact_Person = ifelse(is.na(Contact_Person) | Contact_Person == "", "N/A", Contact_Person)
        )
      
      service_table <- flextable(service_data) %>%
        theme_booktabs() %>%
        autofit() %>%
        bg(part = "header", bg = "#f39c12") %>%
        color(part = "header", color = "white") %>%
        bold(part = "header")
      
      doc <- doc %>%
        body_add_flextable(service_table) %>%
        body_add_par("", style = "Normal")
    }
  }
  
  return(doc)
}

# =============================================================================
# 3. MAIN PARTNERS REPORT GENERATION HANDLER (ENHANCED ERROR HANDLING)
# =============================================================================

handle_partners_report_generation <- function(partners_data, app_title = "Project ARAL Management System") {
  
  tryCatch({
    # Validate input data first
    if (is.null(partners_data) || !is.data.frame(partners_data)) {
      stop("Invalid partners data: must be a data frame")
    }
    
    if (nrow(partners_data) == 0) {
      stop("No partner data available for analysis")
    }
    
    # Clean the data before processing
    partners_data_clean <- partners_data %>%
      mutate(
        # Ensure required columns exist and are properly formatted
        partner_ID = ifelse(is.na(partner_ID), "", as.character(partner_ID)),
        name = ifelse(is.na(name), "", as.character(name)),
        amount = ifelse(is.na(amount), 0, as.numeric(amount)),
        service_cash_fixture_provided = ifelse(is.na(service_cash_fixture_provided), "", 
                                               as.character(service_cash_fixture_provided)),
        date_given = as.Date(date_given),
        contact_person_partner = ifelse(is.na(contact_person_partner), "", 
                                        as.character(contact_person_partner)),
        contact_no_partner = ifelse(is.na(contact_no_partner), "", 
                                    as.character(contact_no_partner))
      )
    
    message("Creating partners dashboard summary...")
    summary_data <- create_partners_dashboard_summary(partners_data_clean)
    
    # Validate summary data
    if (summary_data$total_partners == 0) {
      stop("No valid partner data found after cleaning")
    }
    
    message("Starting AI analysis...")
    # Generate AI analysis with better error handling
    ai_analysis <- partners_gemini_analysis(summary_data)
    
    # Create temporary file
    temp_file <- tempfile(fileext = ".docx")
    
    message("Generating Word document...")
    # Generate Word document
    generate_partners_word_report(summary_data, ai_analysis, temp_file, app_title)
    
    return(list(
      success = TRUE,
      file_path = temp_file,
      ai_analysis = ai_analysis,
      message = "Partners report generated successfully!"
    ))
    
  }, error = function(e) {
    message("Error in handle_partners_report_generation: ", e$message)
    return(list(
      success = FALSE,
      error = e$message,
      message = paste("Partners report generation failed:", e$message)
    ))
  })
}