# Partners Module with AI Report Generation

# Source the AI report generation functions
source("modules/enrollment/enrollment_dashboard/ai_helpers.R")
source("modules/enrollment/enrollment_dashboard/report_document_helpers.R")
source("modules/partners/partners_helpers.R") 
partners_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h1("Partners Management", style = "color: #2c3e50; margin-bottom: 30px;"),
    
    tabsetPanel(
      # Partners Registry Tab - CLEANED (removed report button)
      tabPanel("Partners Registry",
               br(),
               fluidRow(
                 column(12,
                        actionButton(ns("add_partner"), "Add New Partner", 
                                     class = "btn-primary", style = "margin-bottom: 15px;")
                 )
               ),
               
               fluidRow(
                 column(12,
                        withSpinner(DT::dataTableOutput(ns("partners_table")))
                 )
               )
      ),
      
      # Partners Dashboard Tab
      tabPanel("Partners Dashboard",
               br(),
               
               # Summary Cards
               fluidRow(
                 column(3,
                        div(class = "value-box",
                            h3(textOutput(ns("total_partners"))),
                            p("Total Partners")
                        )
                 ),
                 column(3,
                        div(class = "value-box",
                            h3(textOutput(ns("total_amount_received"))),
                            p("Total Amount")
                        )
                 ),
                 column(3,
                        div(class = "value-box",
                            h3(textOutput(ns("cash_donations"))),
                            p("Cash Donations")
                        )
                 ),
                 column(3,
                        div(class = "value-box",
                            h3(textOutput(ns("service_donations"))),
                            p("Service/Fixture")
                        )
                 )
               ),
               
               br(),
               
               # Charts Row
               fluidRow(
                 column(6,
                        wellPanel(
                          h4("Top Partners by Contribution", style = "color: #2c3e50;"),
                          withSpinner(plotlyOutput(ns("top_partners_chart")))
                        )
                 ),
                 column(6,
                        wellPanel(
                          h4("Distribution by Donation Type", style = "color: #2c3e50;"),
                          withSpinner(plotlyOutput(ns("donation_type_chart")))
                        )
                 )
               ),
               
               br(),
               
               fluidRow(
                 column(12,
                        wellPanel(
                          h4("Monthly Donation Trends", style = "color: #2c3e50;"),
                          withSpinner(plotlyOutput(ns("monthly_trends_chart")))
                        )
                 )
               )
      ),
      
      # AI Report Analysis Tab
      tabPanel("AI Report Analysis",
               br(),
               fluidRow(
                 column(12,
                        wellPanel(
                          h4("AI-Generated Partnership Analysis", style = "color: #2c3e50;"),
                          p("Generate comprehensive AI-powered reports analyzing partnership patterns, contribution trends, and strategic recommendations."),
                          br(),
                          actionButton(ns("generate_full_report"), "Generate Complete AI Analysis Report", 
                                       class = "btn-success btn-lg", 
                                       icon = icon("robot"),
                                       style = "width: 100%; margin-bottom: 20px;"),
                          
                          conditionalPanel(
                            condition = paste0("output['", ns("show_analysis"), "']"),
                            wellPanel(
                              h5("AI Analysis Preview:", style = "color: #34495e;"),
                              div(id = ns("analysis_content"),
                                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; max-height: 400px; overflow-y: auto;",
                                  htmlOutput(ns("ai_analysis_preview"))
                              )
                            )
                          ),
                          
                          conditionalPanel(
                            condition = paste0("output['", ns("show_download"), "']"),
                            br(),
                            downloadButton(ns("download_report"), "Download Full Report (.docx)", 
                                           class = "btn-primary btn-lg",
                                           style = "width: 100%;")
                          )
                        )
                 )
               )
      )
    ),
    
    # Add/Edit Partner Modal
    bsModal("partner_modal", "Partner Information", ns("add_partner"), size = "large",
            fluidRow(
              column(6,
                     textInput(ns("partner_ID"), "Partner ID:", ""),
                     textInput(ns("partner_name"), "Partner Name:", ""),
                     numericInput(ns("amount"), "Amount:", value = 0, min = 0),
                     textInput(ns("service_cash_fixture_provided"), "Service/Cash/Fixture Provided:", "")
              ),
              column(6,
                     dateInput(ns("date_given"), "Date Given:", value = Sys.Date()),
                     textInput(ns("contact_person_partner"), "Contact Person:", ""),
                     textInput(ns("contact_no_partner"), "Contact Number:", "")
              )
            ),
            
            footer = tagList(
              actionButton(ns("save_partner"), "Save", class = "btn-primary"),
              actionButton(ns("delete_partner"), "Delete", class = "btn-danger", style = "display: none;"),
              actionButton(ns("cancel_partner"), "Cancel", class = "btn-default")
            )
    )
    
    # NOTE: Removed the report generation modal since we removed the duplicate button
  )
}

##SErver
partners_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      partners_data = NULL,
      editing_id = NULL,
      refresh = 0,
      report_result = NULL,
      ai_analysis = NULL
    )
    
    # Load partners data
    observe({
      values$refresh  # Dependency for refresh
      values$partners_data <- get_table_data(con, "partners_table")
    })
    
    # Partners table
    output$partners_table <- DT::renderDataTable({
      req(values$partners_data)
      
      display_data <- values$partners_data %>%
        select(partner_ID, name, amount, service_cash_fixture_provided, date_given, 
               contact_person_partner, contact_no_partner) %>%
        mutate(amount = ifelse(is.na(amount), 0, amount))
      
      DT::datatable(display_data,
                    options = list(
                      pageLength = 10, 
                      scrollX = TRUE,
                      columnDefs = list(
                        list(targets = c(2), render = DT::JS(
                          "function(data, type, row, meta) {",
                          "return '₱' + parseFloat(data).toLocaleString();",
                          "}"
                        ))
                      )
                    ),
                    rownames = FALSE,
                    selection = "single")
    })
    
    # Handle row selection for editing
    observeEvent(input$partners_table_rows_selected, {
      if (length(input$partners_table_rows_selected) > 0) {
        selected_row <- input$partners_table_rows_selected
        partner_data <- values$partners_data[selected_row, ]
        
        values$editing_id <- partner_data$id
        
        # Populate modal fields
        updateTextInput(session, "partner_ID", value = partner_data$partner_ID)
        updateTextInput(session, "partner_name", value = partner_data$name)
        updateNumericInput(session, "amount", value = ifelse(is.na(partner_data$amount), 0, partner_data$amount))
        updateTextInput(session, "service_cash_fixture_provided", value = partner_data$service_cash_fixture_provided)
        updateDateInput(session, "date_given", value = as.Date(partner_data$date_given))
        updateTextInput(session, "contact_person_partner", value = partner_data$contact_person_partner)
        updateTextInput(session, "contact_no_partner", value = partner_data$contact_no_partner)
        
        # Show delete button
        shinyjs::show("delete_partner")
        
        toggleModal(session, "partner_modal", toggle = "open")
      }
    })
    
    # Handle add new partner
    observeEvent(input$add_partner, {
      values$editing_id <- NULL
      
      # Generate new partner ID
      partner_id <- paste0("PTN_", format(Sys.Date(), "%Y%m%d"), "_", sprintf("%04d", sample(1:9999, 1)))
      
      # Clear modal fields
      updateTextInput(session, "partner_ID", value = partner_id)
      updateTextInput(session, "partner_name", value = "")
      updateNumericInput(session, "amount", value = 0)
      updateTextInput(session, "service_cash_fixture_provided", value = "")
      updateDateInput(session, "date_given", value = Sys.Date())
      updateTextInput(session, "contact_person_partner", value = "")
      updateTextInput(session, "contact_no_partner", value = "")
      
      # Hide delete button
      shinyjs::hide("delete_partner")
    })
    
    # Handle save partner
    observeEvent(input$save_partner, {
      req(input$partner_ID, input$partner_name)
      
      partner_data <- data.frame(
        partner_ID = input$partner_ID,
        name = input$partner_name,
        amount = input$amount,
        service_cash_fixture_provided = input$service_cash_fixture_provided,
        date_given = input$date_given,
        contact_person_partner = input$contact_person_partner,
        contact_no_partner = input$contact_no_partner,
        stringsAsFactors = FALSE
      )
      
      if (is.null(values$editing_id)) {
        # Insert new partner
        result <- insert_data(con, "partners_table", partner_data)
      } else {
        # Update existing partner
        result <- update_data(con, "partners_table", partner_data, "id", values$editing_id)
      }
      
      if (result$success) {
        showNotification(result$message, type = "message")
        values$refresh <- values$refresh + 1
        toggleModal(session, "partner_modal", toggle = "close")
      } else {
        showNotification(result$message, type = "error")
      }
    })
    
    # Handle delete partner
    observeEvent(input$delete_partner, {
      req(values$editing_id)
      
      showModal(modalDialog(
        title = "Confirm Deletion",
        "Are you sure you want to delete this partner? This action cannot be undone.",
        footer = tagList(
          actionButton(ns("confirm_delete"), "Yes, Delete", class = "btn-danger"),
          modalButton("Cancel")
        )
      ))
    })
    
    # Handle confirm delete
    observeEvent(input$confirm_delete, {
      req(values$editing_id)
      
      result <- delete_data(con, "partners_table", "id", values$editing_id)
      
      if (result$success) {
        showNotification(result$message, type = "message")
        values$refresh <- values$refresh + 1
        removeModal()
        toggleModal(session, "partner_modal", toggle = "close")
      } else {
        showNotification(result$message, type = "error")
      }
    })
    
    # Handle cancel
    observeEvent(input$cancel_partner, {
      toggleModal(session, "partner_modal", toggle = "close")
    })
    
    # Dashboard Analytics
    output$total_partners <- renderText({
      req(values$partners_data)
      nrow(values$partners_data)
    })
    
    output$total_amount_received <- renderText({
      req(values$partners_data)
      total <- sum(values$partners_data$amount, na.rm = TRUE)
      paste("₱", format(total, big.mark = ",", nsmall = 2))
    })
    
    output$cash_donations <- renderText({
      req(values$partners_data)
      cash_count <- sum(grepl("cash|money|₱|peso", tolower(values$partners_data$service_cash_fixture_provided)), na.rm = TRUE)
      cash_count
    })
    
    output$service_donations <- renderText({
      req(values$partners_data)
      service_count <- sum(!grepl("cash|money|₱|peso", tolower(values$partners_data$service_cash_fixture_provided)) & 
                             !is.na(values$partners_data$service_cash_fixture_provided) &
                             values$partners_data$service_cash_fixture_provided != "", na.rm = TRUE)
      service_count
    })
    
    # Top Partners Chart
    output$top_partners_chart <- renderPlotly({
      req(values$partners_data)
      
      top_partners <- values$partners_data %>%
        filter(!is.na(amount) & amount > 0) %>%
        arrange(desc(amount)) %>%
        head(10)
      
      if (nrow(top_partners) > 0) {
        p <- plot_ly(top_partners, 
                     x = ~reorder(name, amount), 
                     y = ~amount,
                     type = 'bar',
                     marker = list(color = '#2ecc71'),
                     hovertemplate = '<b>%{x}</b><br>Amount: ₱%{y}<extra></extra>') %>%
          layout(title = "",
                 xaxis = list(title = "Partner"),
                 yaxis = list(title = "Amount (₱)"))
        p
      } else {
        plotly_empty() %>%
          layout(title = "No data available")
      }
    })
    
    # Donation Type Chart
    output$donation_type_chart <- renderPlotly({
      req(values$partners_data)
      
      # Categorize donations
      donation_types <- values$partners_data %>%
        filter(!is.na(service_cash_fixture_provided) & service_cash_fixture_provided != "") %>%
        mutate(
          type = case_when(
            grepl("cash|money|₱|peso", tolower(service_cash_fixture_provided)) ~ "Cash",
            grepl("equipment|computer|laptop|projector", tolower(service_cash_fixture_provided)) ~ "Equipment",
            grepl("service|training|consultation", tolower(service_cash_fixture_provided)) ~ "Service",
            grepl("book|material|supply", tolower(service_cash_fixture_provided)) ~ "Materials",
            TRUE ~ "Other"
          )
        ) %>%
        count(type)
      
      if (nrow(donation_types) > 0) {
        p <- plot_ly(donation_types, 
                     labels = ~type, 
                     values = ~n,
                     type = 'pie',
                     hovertemplate = '<b>%{label}</b><br>Count: %{value}<extra></extra>') %>%
          layout(title = "")
        p
      } else {
        plotly_empty() %>%
          layout(title = "No data available")
      }
    })
    
    # Monthly Trends Chart
    output$monthly_trends_chart <- renderPlotly({
      req(values$partners_data)
      
      monthly_data <- values$partners_data %>%
        filter(!is.na(date_given) & !is.na(amount) & amount > 0) %>%
        mutate(
          year_month = format(as.Date(date_given), "%Y-%m")
        ) %>%
        group_by(year_month) %>%
        summarise(
          total_amount = sum(amount, na.rm = TRUE),
          count = n(),
          .groups = 'drop'
        ) %>%
        arrange(year_month)
      
      if (nrow(monthly_data) > 0) {
        p <- plot_ly(monthly_data, x = ~year_month, y = ~total_amount, type = 'scatter', mode = 'lines+markers',
                     name = 'Amount', line = list(color = '#3498db'),
                     hovertemplate = '<b>%{x}</b><br>Amount: ₱%{y}<extra></extra>') %>%
          add_trace(y = ~count, name = 'Count', yaxis = 'y2', mode = 'lines+markers',
                    line = list(color = '#e74c3c'),
                    hovertemplate = '<b>%{x}</b><br>Count: %{y}<extra></extra>') %>%
          layout(title = "",
                 xaxis = list(title = "Month"),
                 yaxis = list(title = "Amount (₱)"),
                 yaxis2 = list(overlaying = 'y', side = 'right', title = "Count"),
                 legend = list(x = 0, y = 1))
        p
      } else {
        plotly_empty() %>%
          layout(title = "No data available")
      }
    })
    
    # =============================================================================
    # AI REPORT GENERATION FUNCTIONALITY - CLEANED (Only one button now)
    # =============================================================================
    
    # Handle full report generation (AI Report Analysis tab - the only one now)
    observeEvent(input$generate_full_report, {
      req(values$partners_data)
      
      if (nrow(values$partners_data) == 0) {
        showNotification("No partner data available for analysis.", type = "warning")
        return()
      }
      
      withProgress(message = 'Generating AI Analysis...', value = 0, {
        incProgress(0.2, detail = "Preparing partnership data...")
        
        tryCatch({
          incProgress(0.3, detail = "Running AI analysis...")
          
          result <- handle_partners_report_generation(values$partners_data, "Project ARAL Management System")
          
          incProgress(0.8, detail = "Finalizing report...")
          
          if (result$success) {
            values$report_result <- result
            values$ai_analysis <- result$ai_analysis
            
            incProgress(1.0, detail = "Complete!")
            
            showNotification("AI analysis completed successfully!", type = "success")
          } else {
            showNotification(paste("Analysis failed:", result$message), type = "error")
          }
          
        }, error = function(e) {
          showNotification(paste("Error generating analysis:", e$message), type = "error")
        })
      })
    })
    
    # Show analysis content conditionally
    output$show_analysis <- reactive({
      !is.null(values$ai_analysis)
    })
    outputOptions(output, "show_analysis", suspendWhenHidden = FALSE)
    
    # Show download button conditionally
    output$show_download <- reactive({
      !is.null(values$report_result) && values$report_result$success
    })
    outputOptions(output, "show_download", suspendWhenHidden = FALSE)
    
    # Render AI analysis preview
    output$ai_analysis_preview <- renderText({
      req(values$ai_analysis)
      
      preview_html <- paste0(
        "<h5 style='color: #2c3e50; margin-bottom: 15px;'>Executive Summary</h5>",
        "<p style='text-align: justify; margin-bottom: 20px;'>", values$ai_analysis$executive_summary, "</p>",
        
        "<h5 style='color: #2c3e50; margin-bottom: 15px;'>Key Insights</h5>",
        "<p style='text-align: justify; margin-bottom: 20px;'>", 
        substr(values$ai_analysis$contribution_analysis, 1, 300), "...</p>",
        
        "<h5 style='color: #2c3e50; margin-bottom: 15px;'>Strategic Recommendations</h5>",
        "<p style='text-align: justify;'>", 
        substr(values$ai_analysis$recommendations, 1, 400), "...</p>",
        
        "<div style='margin-top: 20px; padding: 10px; background-color: #e8f6f3; border-left: 4px solid #2ecc71;'>",
        "<strong>📊 Full detailed analysis is available in the downloadable Word document.</strong>",
        "</div>"
      )
      
      preview_html
    })
    
    # Download handler for the generated report
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("Partners_Analysis_Report_", format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        req(values$report_result, values$report_result$success)
        
        # Copy the temporary file to the download location
        file.copy(values$report_result$file_path, file)
      },
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    )
    
    # Clean up temporary files when session ends
    session$onSessionEnded(function() {
      if (!is.null(values$report_result) && values$report_result$success) {
        if (file.exists(values$report_result$file_path)) {
          file.remove(values$report_result$file_path)
        }
      }
    })
  })
}