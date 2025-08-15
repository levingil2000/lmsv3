# =============================================================================
# FACILITIES AI REPORT GENERATION HELPERS
# =============================================================================

# =============================================================================
# 1. FACILITIES DASHBOARD SUMMARY CREATOR
# =============================================================================

create_facilities_dashboard_summary <- function(facilities_data) {
  data <- facilities_data
  
  # Basic statistics with better NA handling
  total_facilities <- nrow(data)
  total_value <- sum(data$amount, na.rm = TRUE)
  avg_value <- if(total_facilities > 0) round(mean(data$amount, na.rm = TRUE), 2) else 0
  
  # Status distribution with safer operations
  status_column <- data$status
  status_column[is.na(status_column)] <- "Unknown"
  
  available_count <- sum(status_column == "Available", na.rm = TRUE)
  in_use_count <- sum(status_column == "In Use", na.rm = TRUE)
  maintenance_count <- sum(status_column == "Under Maintenance", na.rm = TRUE)
  damaged_count <- sum(status_column == "Damaged", na.rm = TRUE)
  
  # Type distribution
  type_distribution <- data %>%
    filter(!is.na(type) & type != "") %>%
    count(type, sort = TRUE)
  
  # Value distribution by status
  value_by_status <- data %>%
    filter(!is.na(status) & !is.na(amount)) %>%
    group_by(status) %>%
    summarise(
      count = n(),
      total_value = sum(amount, na.rm = TRUE),
      avg_value = round(mean(amount, na.rm = TRUE), 2),
      .groups = 'drop'
    )
  
  # High-value facilities (top 80th percentile)
  high_value_threshold <- quantile(data$amount, 0.8, na.rm = TRUE)
  high_value_facilities <- data %>%
    filter(!is.na(amount) & amount >= high_value_threshold) %>%
    arrange(desc(amount)) %>%
    head(10)
  
  # Maintenance needs analysis
  maintenance_facilities <- data %>%
    filter(status %in% c("Under Maintenance", "Damaged"))
  
  maintenance_value <- sum(maintenance_facilities$amount, na.rm = TRUE)
  
  # Utilization metrics
  utilization_rate <- if(total_facilities > 0) round((in_use_count / total_facilities) * 100, 1) else 0
  availability_rate <- if(total_facilities > 0) round((available_count / total_facilities) * 100, 1) else 0
  
  list(
    module_type = "facilities",
    total_facilities = total_facilities,
    total_value = total_value,
    avg_value = avg_value,
    available_count = available_count,
    in_use_count = in_use_count,
    maintenance_count = maintenance_count,
    damaged_count = damaged_count,
    type_distribution = type_distribution,
    value_by_status = value_by_status,
    high_value_facilities = high_value_facilities,
    maintenance_facilities = maintenance_facilities,
    maintenance_value = maintenance_value,
    utilization_rate = utilization_rate,
    availability_rate = availability_rate,
    data = data
  )
}

# =============================================================================
# 2. FACILITIES AI ANALYSIS FUNCTIONS
# =============================================================================

facilities_gemini_analysis <- function(facilities_summary) {
  
  message("Starting comprehensive Facilities AI analysis...")
  
  # Initialize results with fallback content
  results <- list(
    executive_summary = "Executive Summary: Facilities analysis reveals asset utilization patterns, maintenance requirements, and infrastructure optimization opportunities across the facility inventory.",
    utilization_analysis = "Utilization analysis shows facility usage patterns, availability rates, and operational efficiency metrics.",
    asset_management = "Asset management insights indicate value distribution, condition assessment, and resource allocation patterns.",
    maintenance_insights = "Maintenance insights reveal facility condition trends and preventive maintenance opportunities.",
    recommendations = "1. Optimize facility utilization rates\n2. Implement preventive maintenance programs\n3. Assess high-value asset security\n4. Improve facility availability tracking\n5. Develop asset replacement planning"
  )
  
  # Generate each section with proper error handling
  tryCatch({
    message("Generating executive summary...")
    exec_result <- analyze_facilities_executive_summary(facilities_summary)
    if (!is.null(exec_result)) results$executive_summary <- exec_result
  }, error = function(e) {
    message("Executive summary generation failed: ", e$message)
  })
  
  tryCatch({
    message("Analyzing utilization patterns...")
    utilization_result <- analyze_facility_utilization(facilities_summary)
    if (!is.null(utilization_result)) results$utilization_analysis <- utilization_result
  }, error = function(e) {
    message("Utilization analysis failed: ", e$message)
  })
  
  tryCatch({
    message("Analyzing asset management...")
    asset_result <- analyze_asset_management(facilities_summary)
    if (!is.null(asset_result)) results$asset_management <- asset_result
  }, error = function(e) {
    message("Asset management analysis failed: ", e$message)
  })
  
  tryCatch({
    message("Generating maintenance insights...")
    maintenance_result <- analyze_maintenance_insights(facilities_summary)
    if (!is.null(maintenance_result)) results$maintenance_insights <- maintenance_result
  }, error = function(e) {
    message("Maintenance insights analysis failed: ", e$message)
  })
  
  tryCatch({
    message("Generating strategic recommendations...")
    rec_result <- generate_facilities_recommendations(facilities_summary)
    if (!is.null(rec_result)) results$recommendations <- rec_result
  }, error = function(e) {
    message("Recommendations generation failed: ", e$message)
  })
  
  message("Facilities AI analysis completed!")
  return(results)
}

# Enhanced analysis functions for facilities data
analyze_facilities_executive_summary <- function(facilities_summary) {
  prompt <- paste0(
    "As a facilities management specialist, write a comprehensive 3-4 paragraph executive summary for a facility management and asset utilization report.\n\n",
    "FACILITIES DATA:\n",
    "• Total Facilities: ", facilities_summary$total_facilities, "\n",
    "• Total Asset Value: ₱", format(facilities_summary$total_value, big.mark = ","), "\n",
    "• Average Facility Value: ₱", format(facilities_summary$avg_value, big.mark = ","), "\n",
    "• Available Facilities: ", facilities_summary$available_count, " (", facilities_summary$availability_rate, "%)\n",
    "• In Use Facilities: ", facilities_summary$in_use_count, " (", facilities_summary$utilization_rate, "%)\n",
    "• Under Maintenance: ", facilities_summary$maintenance_count, "\n",
    "• Damaged Facilities: ", facilities_summary$damaged_count, "\n",
    "• Maintenance-Related Value: ₱", format(facilities_summary$maintenance_value, big.mark = ","), "\n\n",
    "REQUIREMENTS:\n",
    "- Start with an overview of the facility portfolio's scope and asset value\n",
    "- Highlight key utilization metrics and operational efficiency indicators\n",
    "- Discuss facility condition status and maintenance requirements\n",
    "- Conclude with overall facility management effectiveness and strategic priorities\n",
    "- Use professional, stakeholder-appropriate language\n",
    "- Keep between 250-350 words\n\n",
    "Write the executive summary now:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 10))
}

analyze_facility_utilization <- function(facilities_summary) {
  if (facilities_summary$total_facilities == 0) return("No facility data available for analysis.")
  
  utilization_text <- paste0(
    "Utilization Rate: ", facilities_summary$utilization_rate, "%, ",
    "Availability Rate: ", facilities_summary$availability_rate, "%, ",
    "In Use: ", facilities_summary$in_use_count, "/", facilities_summary$total_facilities, " facilities"
  )
  
  prompt <- paste0(
    "Analyze facility utilization patterns and operational efficiency:\n\n",
    "UTILIZATION METRICS:\n",
    "• ", utilization_text, "\n",
    "• Available for Use: ", facilities_summary$available_count, " facilities\n",
    "• Facilities Needing Attention: ", facilities_summary$maintenance_count + facilities_summary$damaged_count, 
    " (maintenance + damaged)\n",
    "• Total Facility Types: ", nrow(facilities_summary$type_distribution), " different categories\n\n",
    "Provide comprehensive analysis (200-250 words) addressing:\n",
    "1. Current utilization efficiency and capacity optimization opportunities\n",
    "2. Facility availability patterns and resource allocation effectiveness\n",
    "3. Operational bottlenecks and underutilized assets\n",
    "4. Usage patterns that indicate high-demand vs low-demand facilities\n",
    "5. Strategic recommendations for improving facility utilization rates\n\n",
    "Include specific data points and provide actionable insights for facility operations:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 9))
}

analyze_asset_management <- function(facilities_summary) {
  # Calculate asset distribution metrics
  high_value_count <- nrow(facilities_summary$high_value_facilities)
  total_facilities <- facilities_summary$total_facilities
  
  asset_concentration <- if(total_facilities > 0) round((high_value_count / total_facilities) * 100, 1) else 0
  
  prompt <- paste0(
    "Analyze asset management and value distribution patterns:\n\n",
    "ASSET METRICS:\n",
    "• Total Portfolio Value: ₱", format(facilities_summary$total_value, big.mark = ","), "\n",
    "• Average Asset Value: ₱", format(facilities_summary$avg_value, big.mark = ","), "\n",
    "• High-Value Assets (top 20%): ", high_value_count, " facilities (", asset_concentration, "%)\n",
    "• Assets Under Maintenance/Repair: ₱", format(facilities_summary$maintenance_value, big.mark = ","), " value\n",
    "• Facility Categories: ", nrow(facilities_summary$type_distribution), " different types\n\n",
    "Provide detailed analysis (200-250 words) covering:\n",
    "1. Asset value distribution and portfolio composition insights\n",
    "2. High-value asset concentration and security considerations\n",
    "3. Asset condition assessment and depreciation patterns\n",
    "4. Resource allocation efficiency across facility types\n",
    "5. Risk management implications for valuable assets\n",
    "6. Strategic asset development and replacement planning needs\n\n",
    "Focus on asset optimization and financial stewardship insights:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 9))
}

analyze_maintenance_insights <- function(facilities_summary) {
  maintenance_total <- facilities_summary$maintenance_count + facilities_summary$damaged_count
  maintenance_percentage <- if(facilities_summary$total_facilities > 0) {
    round((maintenance_total / facilities_summary$total_facilities) * 100, 1)
  } else 0
  
  prompt <- paste0(
    "Analyze facility maintenance patterns and condition management:\n\n",
    "MAINTENANCE METRICS:\n",
    "• Facilities Under Maintenance: ", facilities_summary$maintenance_count, "\n",
    "• Damaged Facilities: ", facilities_summary$damaged_count, "\n",
    "• Total Needing Attention: ", maintenance_total, " (", maintenance_percentage, "% of portfolio)\n",
    "• Value of Assets Needing Maintenance: ₱", format(facilities_summary$maintenance_value, big.mark = ","), "\n",
    "• Operational Facilities: ", facilities_summary$available_count + facilities_summary$in_use_count, 
    " (available + in use)\n\n",
    "Provide comprehensive analysis (200-250 words) covering:\n",
    "1. Current maintenance workload and resource requirements\n",
    "2. Facility condition trends and deterioration patterns\n",
    "3. Preventive vs reactive maintenance opportunities\n",
    "4. Impact of maintenance issues on operational capacity\n",
    "5. Cost implications of deferred maintenance\n",
    "6. Recommendations for maintenance priority scheduling\n\n",
    "Focus on maintenance strategy and operational continuity insights:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 9))
}

generate_facilities_recommendations <- function(facilities_summary) {
  # Calculate key metrics for recommendations
  operational_rate <- round(((facilities_summary$available_count + facilities_summary$in_use_count) / 
                               facilities_summary$total_facilities) * 100, 1)
  
  prompt <- paste0(
    "Based on this facility management analysis, provide 6-8 specific, actionable recommendations:\n\n",
    "FACILITY PERFORMANCE INDICATORS:\n",
    "• Total Facility Portfolio: ", facilities_summary$total_facilities, " facilities\n",
    "• Total Asset Value: ₱", format(facilities_summary$total_value, big.mark = ","), "\n",
    "• Utilization Rate: ", facilities_summary$utilization_rate, "% actively in use\n",
    "• Operational Rate: ", operational_rate, "% (available + in use)\n",
    "• Facilities Needing Maintenance: ", facilities_summary$maintenance_count + facilities_summary$damaged_count, "\n",
    "• High-Value Assets: ", nrow(facilities_summary$high_value_facilities), " facilities\n\n",
    "Generate numbered recommendations (250-350 words total) focusing on:\n",
    "- Strategies to improve facility utilization and operational efficiency\n",
    "- Preventive maintenance planning and condition management\n",
    "- Asset protection and security for high-value facilities\n",
    "- Resource allocation optimization across facility types\n",
    "- Technology integration for better facility management\n",
    "- Long-term strategic planning for facility development\n",
    "- Cost optimization and budget planning improvements\n\n",
    "Format as numbered list with brief explanations. Be specific and actionable for facility management:"
  )
  
  return(safe_gemini_call(prompt, max_retries = 3, wait_time = 10))
}

# =============================================================================
# 3. FACILITIES WORD DOCUMENT GENERATION
# =============================================================================

generate_facilities_word_report <- function(facilities_summary, ai_sections, output_path, app_title = "Project ARAL Management System") {
  tryCatch({
    # Load required library
    if (!requireNamespace("officer", quietly = TRUE)) {
      stop("officer package is required for Word document generation")
    }
    if (!requireNamespace("flextable", quietly = TRUE)) {
      stop("flextable package is required for table formatting")
    }
    
    library(officer)
    library(flextable)
    
    doc <- read_docx()
    
    # Header
    doc <- doc %>%
      body_add_par("Facilities Management & Asset Utilization Report", style = "heading 1") %>%
      body_add_par("Comprehensive Analysis of Facility Portfolio and Operational Efficiency", style = "heading 2") %>%
      body_add_par(paste("Generated on:", format(Sys.Date(), "%B %d, %Y")), style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # Executive Summary
    doc <- doc %>%
      body_add_par("EXECUTIVE SUMMARY", style = "heading 2") %>%
      body_add_par(ai_sections$executive_summary, style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # Key Facility Metrics
    doc <- add_facility_metrics_section(doc, facilities_summary)
    
    # Utilization Analysis
    doc <- doc %>%
      body_add_par("FACILITY UTILIZATION ANALYSIS", style = "heading 2") %>%
      body_add_par(ai_sections$utilization_analysis, style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # Add utilization breakdown table
    doc <- add_utilization_breakdown_section(doc, facilities_summary)
    
    # Asset Management
    doc <- doc %>%
      body_add_par("ASSET MANAGEMENT & VALUE ANALYSIS", style = "heading 2") %>%
      body_add_par(ai_sections$asset_management, style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # High-value facilities table
    doc <- add_high_value_facilities_section(doc, facilities_summary)
    
    # Maintenance Insights
    doc <- doc %>%
      body_add_par("MAINTENANCE INSIGHTS & CONDITION MANAGEMENT", style = "heading 2") %>%
      body_add_par(ai_sections$maintenance_insights, style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # Maintenance analysis table
    doc <- add_maintenance_analysis_section(doc, facilities_summary)
    
    # Strategic Recommendations
    doc <- doc %>%
      body_add_par("STRATEGIC RECOMMENDATIONS", style = "heading 2") %>%
      body_add_par(ai_sections$recommendations, style = "Normal") %>%
      body_add_par("", style = "Normal")
    
    # Detailed Facility Directory
    doc <- add_detailed_facility_directory(doc, facilities_summary)
    
    # Footer
    doc <- add_facility_report_footer(doc, app_title)
    
    # Save
    print(doc, target = output_path)
    message("✓ Facilities Word document successfully generated at: ", output_path)
    return(TRUE)
    
  }, error = function(e) {
    message("❌ Error in generate_facilities_word_report: ", e$message)
    print(traceback())
    stop(e)
  })
}

# Helper: Key Facility Metrics Section
add_facility_metrics_section <- function(doc, facilities_summary) {
  doc <- doc %>%
    body_add_par("KEY FACILITY METRICS", style = "heading 2")
  
  metrics_data <- data.frame(
    Metric = c("Total Facilities", "Total Asset Value", "Average Facility Value", 
               "Utilization Rate", "Availability Rate", "Facilities in Maintenance",
               "Damaged Facilities", "Operational Efficiency Score"),
    Value = c(
      as.character(facilities_summary$total_facilities),
      paste0("₱", format(facilities_summary$total_value, big.mark = ",")),
      paste0("₱", format(facilities_summary$avg_value, big.mark = ",")),
      paste0(facilities_summary$utilization_rate, "% (", facilities_summary$in_use_count, " facilities)"),
      paste0(facilities_summary$availability_rate, "% (", facilities_summary$available_count, " facilities)"),
      paste0(facilities_summary$maintenance_count, " facilities"),
      paste0(facilities_summary$damaged_count, " facilities"),
      ifelse(facilities_summary$utilization_rate > 70, "Excellent", 
             ifelse(facilities_summary$utilization_rate > 50, "Good", "Needs Improvement"))
    ),
    stringsAsFactors = FALSE
  )
  
  metrics_table <- flextable(metrics_data) %>%
    theme_booktabs() %>%
    autofit() %>%
    bg(part = "header", bg = "#3498db") %>%
    color(part = "header", color = "white") %>%
    bold(part = "header")
  
  doc <- doc %>%
    body_add_flextable(metrics_table) %>%
    body_add_par("", style = "Normal")
  
  return(doc)
}

# Helper: Utilization Breakdown Section
add_utilization_breakdown_section <- function(doc, facilities_summary) {
  if (nrow(facilities_summary$value_by_status) > 0) {
    doc <- doc %>%
      body_add_par("Facility Status Distribution", style = "heading 3")
    
    breakdown_data <- facilities_summary$value_by_status %>%
      mutate(
        Status = status,
        Count = count,
        Total_Value = paste0("₱", format(total_value, big.mark = ",")),
        Average_Value = paste0("₱", format(avg_value, big.mark = ","))
      ) %>%
      select(Status, Count, Total_Value, Average_Value)
    
    breakdown_table <- flextable(breakdown_data) %>%
      theme_booktabs() %>%
      autofit() %>%
      bg(part = "header", bg = "#2ecc71") %>%
      color(part = "header", color = "white") %>%
      bold(part = "header") %>%
      bg(i = ~ Status == "Available", j = "Status", bg = "#d4edda") %>%
      bg(i = ~ Status == "In Use", j = "Status", bg = "#fff3cd") %>%
      bg(i = ~ Status == "Under Maintenance", j = "Status", bg = "#cce5ff") %>%
      bg(i = ~ Status == "Damaged", j = "Status", bg = "#f8d7da")
    
    doc <- doc %>%
      body_add_flextable(breakdown_table) %>%
      body_add_par("", style = "Normal")
  }
  
  return(doc)
}

# Helper: High-Value Facilities Section
add_high_value_facilities_section <- function(doc, facilities_summary) {
  if (nrow(facilities_summary$high_value_facilities) > 0) {
    doc <- doc %>%
      body_add_par("High-Value Facility Assets", style = "heading 3")
    
    high_value_data <- facilities_summary$high_value_facilities %>%
      head(10) %>%
      mutate(
        Facility_Name = name,
        Facility_ID = FR_ID,
        Type = type,
        Status = status,
        Value = paste0("₱", format(amount, big.mark = ","))
      ) %>%
      select(Facility_Name, Facility_ID, Type, Status, Value)
    
    high_value_table <- flextable(high_value_data) %>%
      theme_booktabs() %>%
      autofit() %>%
      bg(part = "header", bg = "#e67e22") %>%
      color(part = "header", color = "white") %>%
      bold(part = "header")
    
    doc <- doc %>%
      body_add_flextable(high_value_table) %>%
      body_add_par("", style = "Normal")
  }
  
  return(doc)
}

# Helper: Maintenance Analysis Section
add_maintenance_analysis_section <- function(doc, facilities_summary) {
  if (nrow(facilities_summary$maintenance_facilities) > 0) {
    doc <- doc %>%
      body_add_par("Facilities Requiring Maintenance Attention", style = "heading 3")
    
    maintenance_data <- facilities_summary$maintenance_facilities %>%
      head(15) %>%
      mutate(
        Facility_Name = name,
        Facility_ID = FR_ID,
        Type = type,
        Status = status,
        Value = paste0("₱", format(ifelse(is.na(amount), 0, amount), big.mark = ","))
      ) %>%
      select(Facility_Name, Facility_ID, Type, Status, Value)
    
    maintenance_table <- flextable(maintenance_data) %>%
      theme_booktabs() %>%
      autofit() %>%
      bg(part = "header", bg = "#e74c3c") %>%
      color(part = "header", color = "white") %>%
      bold(part = "header")
    
    doc <- doc %>%
      body_add_flextable(maintenance_table) %>%
      body_add_par("", style = "Normal")
  }
  
  return(doc)
}

# Helper: Detailed Facility Directory
add_detailed_facility_directory <- function(doc, facilities_summary) {
  doc <- doc %>%
    body_add_par("DETAILED FACILITY DIRECTORY", style = "heading 2") %>%
    body_add_par("Complete listing of all facilities organized by status and type.", style = "Normal") %>%
    body_add_par("", style = "Normal")
  
  if (nrow(facilities_summary$data) > 0) {
    # Group by status
    statuses <- c("Available", "In Use", "Under Maintenance", "Damaged")
    
    for (status in statuses) {
      status_facilities <- facilities_summary$data %>%
        filter(status == !!status) %>%
        arrange(desc(ifelse(is.na(amount), 0, amount)))
      
      if (nrow(status_facilities) > 0) {
        doc <- doc %>%
          body_add_par(paste(status, "Facilities"), style = "heading 3")
        
        status_data <- status_facilities %>%
          head(20) %>%
          select(
            Facility_Name = name,
            Facility_ID = FR_ID,
            Type = type,
            Description = description,
            Value = amount
          ) %>%
          mutate(
            Facility_Name = ifelse(is.na(Facility_Name), "N/A", Facility_Name),
            Facility_ID = ifelse(is.na(Facility_ID), "N/A", Facility_ID),
            Type = ifelse(is.na(Type) | Type == "", "N/A", Type),
            Description = ifelse(is.na(Description) | Description == "", "N/A", Description),
            Value = paste0("₱", format(ifelse(is.na(Value), 0, Value), big.mark = ","))
          )
        
        status_table <- flextable(status_data) %>%
          theme_booktabs() %>%
          autofit() %>%
          bg(part = "header", bg = "#34495e") %>%
          color(part = "header", color = "white") %>%
          bold(part = "header")
        
        doc <- doc %>%
          body_add_flextable(status_table) %>%
          body_add_par("", style = "Normal")
      }
    }
  }
  
  return(doc)
}

# Helper: Facility Report Footer
add_facility_report_footer <- function(doc, app_title) {
  doc <- doc %>%
    body_add_par("---", style = "Normal") %>%
    body_add_par("REPORT DETAILS", style = "heading 3") %>%
    body_add_par(paste("Report generated by:", app_title), style = "Normal") %>%
    body_add_par(paste("Generated on:", format(Sys.time(), "%B %d, %Y at %I:%M %p")), style = "Normal") %>%
    body_add_par("AI Analysis powered by Google Gemini", style = "Normal") %>%
    body_add_par("This facility management report provides comprehensive analysis of asset utilization, maintenance requirements, and strategic recommendations for optimal facility operations.", style = "Normal")
  
  return(doc)
}

# =============================================================================
# 4. MAIN FACILITIES REPORT GENERATION HANDLER
# =============================================================================

handle_facilities_report_generation <- function(facilities_data, app_title = "Project ARAL Management System") {
  
  tryCatch({
    # Validate input data first
    if (is.null(facilities_data) || !is.data.frame(facilities_data)) {
      stop("Invalid facilities data: must be a data frame")
    }
    
    if (nrow(facilities_data) == 0) {
      stop("No facility data available for analysis")
    }
    
    # Clean the data before processing
    facilities_data_clean <- facilities_data %>%
      mutate(
        # Ensure required columns exist and are properly formatted
        FR_ID = ifelse(is.na(FR_ID), "", as.character(FR_ID)),
        name = ifelse(is.na(name), "", as.character(name)),
        description = ifelse(is.na(description), "", as.character(description)),
        status = ifelse(is.na(status), "Unknown", as.character(status)),
        type = ifelse(is.na(type), "", as.character(type)),
        amount = ifelse(is.na(amount), 0, as.numeric(amount))
      )
    
    message("Creating facilities dashboard summary...")
    summary_data <- create_facilities_dashboard_summary(facilities_data_clean)
    
    # Validate summary data
    if (summary_data$total_facilities == 0) {
      stop("No valid facility data found after cleaning")
    }
    
    message("Starting AI analysis...")
    # Generate AI analysis with better error handling
    ai_analysis <- facilities_gemini_analysis(summary_data)
    
    # Create temporary file
    temp_file <- tempfile(fileext = ".docx")
    
    message("Generating Word document...")
    # Generate Word document
    generate_facilities_word_report(summary_data, ai_analysis, temp_file, app_title)
    
    return(list(
      success = TRUE,
      file_path = temp_file,
      ai_analysis = ai_analysis,
      message = "Facilities report generated successfully!"
    ))
    
  }, error = function(e) {
    message("Error in handle_facilities_report_generation: ", e$message)
    return(list(
      success = FALSE,
      error = e$message,
      message = paste("Facilities report generation failed:", e$message)
    ))
  })
}