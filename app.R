# Load required libraries
library(shiny)
library(tidyverse)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .logo-header { 
        text-align: left; 
        margin-bottom: 20px; 
      }
      
      /* Inactive tabs - gray and muted */
      .nav-tabs > li:not(.active) > a {
        background-color: #d3d3d3 !important;
        color: #666666 !important;
        border: 2px solid #999999 !important;
        font-weight: normal !important;
        font-size: 15px !important;
        opacity: 0.6 !important;
      }
      
      /* Inactive tabs on hover */
      .nav-tabs > li:not(.active) > a:hover {
        background-color: #c0c0c0 !important;
        color: #333333 !important;
        opacity: 0.8 !important;
      }
      
      /* Active tab - bright blue and bold */
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: #007bff !important;
        color: white !important;
        border: 2px solid #007bff !important;
        border-bottom-color: white !important;
        font-weight: normal !important;
        font-size: 15px !important;
        opacity: 1 !important;
      }
    "))
  ),
  
  # Image header
  div(class = "logo-header",
      tags$img(src = "logos2.png", height = "100px")
  ),
  
  tags$h2("DArT sample tracking file management tool", 
          style = "font-size: 24px; text-align: left; margin: 10px 0px; font-weight: normal;"),
  
  # Create tabset with two tabs
  tabsetPanel(
    id = "main_tabs",
    type = "tabs",
    
    # Tab 1: Generate sample tracking file template
    tabPanel(
      "Generate sample tracking file template",
      value = "generate_tab",
      
      # Brief description text below the tab title but above sidebarLayout
      p("Use this tab to generate a DArT sample tracking file template. 
        Specify organism, tissue, and the number of plates. 
        The app will automatically add PlateID, Row, and Column information."), 
      
      sidebarLayout(
        sidebarPanel(
          textInput("organism", "Organism (use the organism's common name)", 
                    placeholder = "e.g., Alfalfa, not Medicago"),
          
          selectInput("tissue", "Tissue Type",
                      choices = c("", "leaf", "seed", "root", "fin", "clip", "whole insect", 
                                  "insect leg", "insect head"),
                      selected = ""),
          
          textInput("num_plates", "Number of Plates", 
                    value = "1", placeholder = "Enter number of plates"),
          
          br(),
          # Display validation errors
          uiOutput("validation_message"),
          
          br(),
          helpText("Each plate contains 94 wells (G12 and H12 excluded per DArT requirements)."),
          helpText("Total samples: ", textOutput("total_samples", inline = TRUE)),
          
          br(),
          h4("Download"),
          uiOutput("download_button_ui")
        ),
          
        mainPanel(
          h4("Template Preview (first 12 rows)"),
          tableOutput("template_preview")
        )
      )
    ),
    
    # Tab 2: Convert breeder genotype IDs to DArT-friendly IDs
    tabPanel(
      "Convert breeder genotype IDs to DArT-friendly IDs",
      value = "convert_tab",
    
      # Brief description text below the tab title but above sidebarLayout
      p("After you filled out the sample tracking file template with genotype IDs (required) and 
      comments (optional), use this tab to generate a sample tracking file with DArT-friendly IDs."),
      
      sidebarLayout(
        sidebarPanel(
          fileInput("dartfile", "Upload your DArT tracking sample file", accept = ".csv"),
          textInput("outname", "Enter the base name for output files", 
                    placeholder = "ENTER PROJECT NAME", value = ""),
          
          h4("Download"),
          downloadButton("download_all", "Processed sample tracking files (.zip)")
        ),
        mainPanel(
          h4("Preview of processed data (first 12 rows)"),
          tableOutput("preview")
        )
      )
    )
  )
)


# Server

server <- function(input, output, session) {
  
  # ========== TAB 1: GENERATE DART SAMPLE TRACKING FILE TEMPLATE ==========
  
  # Reactive validation checker  # <<< ADDED
  validation_status <- reactive({
    # Return list with validation status and message
    
    # Check num_plates
    if (is.null(input$num_plates) || nchar(trimws(input$num_plates)) == 0) {
      return(list(valid = FALSE, message = "Please enter number of plates"))
    }
    
    num_plates_value <- suppressWarnings(as.numeric(input$num_plates))
    if (is.na(num_plates_value)) {
      return(list(valid = FALSE, message = "Number of plates must be a valid number"))
    }
    if (num_plates_value <= 0) {
      return(list(valid = FALSE, message = "Number of plates must be greater than 0"))
    }
    if (num_plates_value > 20) {
      return(list(valid = FALSE, message = "Number of plates must be 20 or less"))
    }
    
    # Check organism
    if (is.null(input$organism) || nchar(trimws(input$organism)) == 0) {
      return(list(valid = FALSE, message = "Please enter the organism name"))
    }
    if (!grepl("^[A-Z]", trimws(input$organism))) {
      return(list(valid = FALSE, message = "ERROR: Organism name must start with a CAPITAL letter"))
    }
    
    # Check tissue
    if (is.null(input$tissue) || nchar(trimws(input$tissue)) == 0) {
      return(list(valid = FALSE, message = "Please select a tissue type"))
    }
    
    # All validations passed
    return(list(valid = TRUE, message = ""))
  })
  
  # Display validation message  # <<< ADDED
  output$validation_message <- renderUI({
    status <- validation_status()
    if (!status$valid) {
      tags$div(
        style = "color: red; font-weight: bold; padding: 10px; background-color: #ffeeee; border: 1px solid red; border-radius: 5px;",
        status$message
      )
    } else {
      NULL
    }
  })
  
  # Conditional download button  # <<< ADDED
  output$download_button_ui <- renderUI({
    if (validation_status()$valid) {
      downloadButton("download_template", "Download sample tracking file (.csv)")
    } else {
      tags$button("Sample tracking file template (.csv)", 
                  class = "btn btn-default", 
                  disabled = "disabled",
                  style = "opacity: 0.5; cursor: not-allowed;")
    }
  })
  
  # Reactive expression to calculate total samples
  output$total_samples <- renderText({
    req(input$num_plates)
    
    # Check if input is empty or whitespace
    if (nchar(trimws(input$num_plates)) == 0) {
      return("")
    }
    
    # Convert to numeric and validate
    num <- suppressWarnings(as.numeric(input$num_plates))
    
    if (is.na(num) || num <= 0) {
      return("Invalid number")
    }
    
    paste(num * 94)
  })
  
  # Reactive expression to generate the template data
  generate_template <- reactive({
    # Only proceed if validation passes
    req(validation_status()$valid)
    
    num_plates_value <- as.numeric(input$num_plates)
    
    # Create plate layout for each plate
    plates_list <- lapply(1:num_plates_value, function(plate_num) {
      # Create all combinations of Row (A-H) and Column (1-12) for 96 wells
      plate_layout <- expand.grid(
        Row = LETTERS[1:8],      # A through H
        Column = 1:12,           # 1 through 12
        stringsAsFactors = FALSE
      )
      
      # Remove G12 and H12 wells (DArT requirement)
      plate_layout <- plate_layout %>%
        filter(!(Row == "G" & Column == 12),
               !(Row == "H" & Column == 12))
      
      # Add PlateID with two-digit formatting
      plate_layout$PlateID <- paste0("Plate_", sprintf("%02d", plate_num))
      
      # Add other columns
      plate_layout$Organism <- trimws(input$organism)
      plate_layout$Species <- ""
      plate_layout$Genotype <- ""
      plate_layout$Tissue <- input$tissue
      plate_layout$Comments <- ""
      
      # Reorder columns to match required format
      plate_layout <- plate_layout[, c("PlateID", "Row", "Column", "Organism", 
                                       "Species", "Genotype", "Tissue", "Comments")]
      
      return(plate_layout)
    })
    
    # Combine all plates into one data frame
    template_df <- do.call(rbind, plates_list)
    
    return(template_df)
  })
  
  # Preview table showing first 12 rows
  output$template_preview <- renderTable({
    if (validation_status()$valid) {
      head(generate_template(), 12)
    } else {
      NULL
    }
  }, rownames = FALSE)
  
  # Download handler for template
  output$download_template <- downloadHandler(
    filename = function() {
      num_plates_value <- as.numeric(input$num_plates)
      paste0("DArT_sample_tracking_file_template_", num_plates_value, "plates_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- generate_template()
      write.csv(df, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  
  # ========== TAB 2: CONVERT BREEDER GENOTYPE IDs TO DART-FRIENDLY IDs ==========
  
  raw_data <- reactive({
    req(input$dartfile)
    read_csv(input$dartfile$datapath, show_col_types = FALSE, col_types = cols(.default = "c"))  # <<< FIXED: reads all as character
  })
  
  processed <- reactive({
    raw_data() %>%
      # Keep original Genotype for Breeder_IDs
      mutate(Breeder_IDs = Genotype) %>%  
      # Clean only relevant character columns for DArT_IDs
      mutate(across(
        c(Genotype, PlateID, Row, Column),  # columns used to build DArT_IDs
        ~ str_squish(.) %>%                     # trim whitespace
          str_replace_all("[^\\x20-\\x7E]", "") %>%  # remove non-ASCII
          str_replace_all("[^A-Za-z0-9_]", "")       # remove symbols like # ? % @
      )) %>%
      mutate(
        PlateID = as.character(PlateID),        # allow Y1, Y2, etc.
        Column = as.character(as.integer(Column)),
        DArT_IDs = paste(
          "S",
          paste0(Genotype, "_"),
          PlateID,
          paste0(Row, Column),
          sep = "_"
        )
      )
  })
  
  processed_only <- reactive({
    processed() %>%
      mutate(Genotype = DArT_IDs) %>%   # overwrite Genotype column
      select(-DArT_IDs, -Breeder_IDs)   # drop helper columns
  })
  
  processed_both <- reactive({
    processed() %>%
      select(DArT_IDs, Breeder_IDs, everything())   # keep both IDs at front
  })
  
  # Keep preview same as before (processed file)
  output$preview <- renderTable({
    head(processed_only(), 12)
  })
  
  # Single download handler for all files zipped
  output$download_all <- downloadHandler(
    filename = function() {
      paste0(input$outname, "_tracking_files.zip")
    },
    content = function(file) {
      # Create a temporary directory
      tmpdir <- tempdir()
      
      # Define full paths for temporary files
      original_file <- file.path(tmpdir, paste0(input$outname, "_Breeder_IDs_sample_tracking_file.csv"))
      processed_file <- file.path(tmpdir, paste0(input$outname, "_DArT_IDs_sample_tracking_file.csv"))
      both_file <- file.path(tmpdir, paste0(input$outname, "_ID_key_file.csv"))
      
      # Write CSV files with na="" so empty cells stay empty
      raw_data() %>% write_csv(original_file, na = "")
      processed_only() %>% write_csv(processed_file, na = "")
      processed_both() %>% write_csv(both_file, na = "")
      
      # Create zip archive using full file paths
      zip::zip(zipfile = file, files = c(original_file, processed_file, both_file), mode = "cherry-pick")
    },
    contentType = "application/zip"
  )
}
# Run app
shinyApp(ui, server)