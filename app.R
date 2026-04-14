# Load required libraries
library(shiny)
library(tidyverse)
library(stringr)
library(tidyr)
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
      p("Use this tab to generate a DArT sample tracking file template.
        Specify project name, organism, tissue, and the number of plates.
        The app will automatically add PlateID, Row, and Column information."),
      sidebarLayout(
        sidebarPanel(
          textInput("project_name", "Project Name",
                    placeholder = "e.g., CRB007-2025"),
          textInput("organism", "Organism (use the organism's common name)",
                    placeholder = "e.g., Alfalfa, not Medicago"),
          selectInput("tissue", "Tissue Type",
                      choices = c("", "leaf", "seed", "root", "fin", "clip", "whole insect",
                                  "insect leg", "insect head"),
                      selected = ""),
          textInput("num_plates", "Number of Plates",
                    value = "1", placeholder = "Enter number of plates"),
          br(),
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
      p("After you filled out the sample tracking file template with genotype IDs (required) and
      comments (optional), use this tab to generate a sample tracking file with DArT-friendly IDs."),
      sidebarLayout(
        sidebarPanel(
          fileInput("dartfile", "Upload your DArT tracking sample file", accept = ".csv"),
          h4("Download"),
          helpText("The Project Name will be automatically extracted from your uploaded filename and added to the processed files."),
          downloadButton("download_all", "Download sample tracking files (.zip)")
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
  validation_status <- reactive({
    if (is.null(input$project_name) || nchar(trimws(input$project_name)) == 0) {
      return(list(valid = FALSE, message = "Please enter a project name"))
    }
    project_pattern <- "^[A-Z]{3}[0-9]{3}-[0-9]{4}$"
    if (!grepl(project_pattern, trimws(input$project_name))) {
      return(list(valid = FALSE, message = "ERROR: Project name must be in format XXX###-YYYY (e.g., ABC123-2025)"))
    }
    if (is.null(input$organism) || nchar(trimws(input$organism)) == 0) {
      return(list(valid = FALSE, message = "Please enter the organism name"))
    }
    if (!grepl("^[A-Z]", trimws(input$organism))) {
      return(list(valid = FALSE, message = "ERROR: Organism name must start with a CAPITAL letter"))
    }
    if (is.null(input$tissue) || nchar(trimws(input$tissue)) == 0) {
      return(list(valid = FALSE, message = "Please select a tissue type"))
    }
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
    if (num_plates_value > 100) {
      return(list(valid = FALSE, message = "Number of plates must be 100 or less"))
    }
    if (num_plates_value != as.integer(num_plates_value)) {
      return(list(valid = FALSE, message = "Number of plates must be a whole number"))
    }
    return(list(valid = TRUE, message = ""))
  })
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
  output$total_samples <- renderText({
    req(input$num_plates)
    if (nchar(trimws(input$num_plates)) == 0) {
      return("")
    }
    num <- suppressWarnings(as.numeric(input$num_plates))
    if (is.na(num) || num <= 0) {
      return("Invalid number")
    }
    paste(num * 94)
  })
  generate_template <- reactive({
    req(validation_status()$valid)
    num_plates_value <- as.numeric(input$num_plates)
    plates_list <- lapply(1:num_plates_value, function(plate_num) {
      plate_layout <- expand.grid(
        Row = LETTERS[1:8],
        Column = 1:12,
        stringsAsFactors = FALSE
      )
      plate_layout <- plate_layout %>%
        filter(!(Row == "G" & Column == 12),
               !(Row == "H" & Column == 12))
      plate_layout$PlateID  <- paste0(plate_num)
      plate_layout$Organism <- trimws(input$organism)
      plate_layout$Species  <- ""
      plate_layout$Genotype <- ""
      plate_layout$Tissue   <- input$tissue
      plate_layout$Comments <- ""
      plate_layout <- plate_layout[, c("PlateID", "Row", "Column", "Organism",
                                       "Species", "Genotype", "Tissue", "Comments")]
      return(plate_layout)
    })
    template_df <- do.call(rbind, plates_list)
    return(template_df)
  })
  output$template_preview <- renderTable({
    if (validation_status()$valid) {
      head(generate_template(), 12)
    } else {
      NULL
    }
  }, rownames = FALSE)
  output$download_template <- downloadHandler(
    filename = function() {
      paste0(input$project_name, "_sample_tracking.csv")
    },
    content = function(file) {
      df <- generate_template()
      df <- df[, c("PlateID", "Row", "Column", "Organism",
                   "Species", "Genotype", "Tissue", "Comments")]
      write_csv(df, file, na = "")
    },
    contentType = "text/csv"
  )
  # ========== TAB 2: CONVERT BREEDER GENOTYPE IDs TO DART-FRIENDLY IDs ==========
  go_proceed <- reactiveVal(FALSE)
  raw_data <- reactive({
    req(input$dartfile)
    read_csv(input$dartfile$datapath, show_col_types = FALSE, col_types = cols(.default = "c"))
  })
  observeEvent(input$dartfile, {
    df <- raw_data()
    missing <- any(trimws(as.character(df$Genotype)) == "" | is.na(df$Genotype))
    go_proceed(FALSE)
    if (missing) {
      showModal(modalDialog(
        title = "Warning",
        "Genotype IDs missing. Are you sure you want to proceed?",
        easyClose = FALSE,
        footer = tagList(
          actionButton("confirm_proceed", "Proceed"),
          modalButton("Cancel")
        )
      ))
    }
  })
  observeEvent(input$confirm_proceed, {
    removeModal()
    go_proceed(TRUE)
  })
  processed <- reactive({
    raw_data() %>%
      mutate(
        Breeder_IDs = Genotype,
        Genotype_processed = Genotype %>%
          str_replace_all("/", "-") %>%
          str_replace_all("[^A-Za-z0-9_]", "_")
      ) %>%
      mutate(
        across(
          where(is.character),
          ~ .x %>% str_replace_all("[;'\",]", " ")
        )
      ) %>%
      mutate(
        PlateID = as.character(PlateID),
        Column  = as.character(Column)
      ) %>%
      mutate(
        DArT_IDs = paste(
          "S",
          paste0(Genotype_processed, "_"),
          PlateID,
          paste0(
            Row,
            stringr::str_pad(Column, width = 2, pad = "0")
          ),
          sep = "_"
        )
      )
  })
  processed_only <- reactive({
    processed() %>%
      mutate(
        Genotype = DArT_IDs
      ) %>%
      select(-DArT_IDs, -Breeder_IDs, -Genotype_processed)
  })
  processed_both <- reactive({
    processed() %>%
      mutate(
        Genotype = Genotype_processed
      ) %>%
      select(
        DArT_IDs,
        Breeder_IDs,
        everything(),
        -Genotype_processed
      )
  })
  # Preview constrained to 8 columns, showing DArT IDs in Genotype column
  output$preview <- renderTable({
    head(processed_only() %>%
           select(any_of(c("PlateID","Row","Column","Organism","Species","Genotype","Tissue","Comments"))), 12)
  })
  output$download_all <- downloadHandler(
    filename = function() {
      uploaded_name <- input$dartfile$name
      project_code <- sub("_.*", "", uploaded_name)
      paste0(project_code, "_tracking_files.zip")
    },
    content = function(file) {
      uploaded_name <- input$dartfile$name
      project_code <- sub("_.*", "", uploaded_name)
      
      tmpdir <- tempdir()
      original_file  <- file.path(tmpdir, paste0(project_code, "_Breeder_IDs_sample_tracking.csv"))
      processed_file <- file.path(tmpdir, paste0(project_code, "_DArT_IDs_sample_tracking.csv"))
      both_file      <- file.path(tmpdir, paste0(project_code, "_ID_key.csv"))
      
      # Original file: Breeder IDs, constrained to 8 columns
      raw_data() %>%
        select(any_of(c("PlateID","Row","Column","Organism","Species","Genotype","Tissue","Comments"))) %>%
        write_csv(original_file, na = "")
      
      # DArT IDs file: Genotype replaced with DArT_IDs, constrained to 8 columns
      processed_only() %>%
        select(any_of(c("PlateID","Row","Column","Organism","Species","Genotype","Tissue","Comments"))) %>%
        write_csv(processed_file, na = "")
      
      # Key file: all columns kept, Genotype column dropped
      processed_both() %>%
        select(-Genotype) %>%
        write_csv(both_file, na = "")
      
      zip::zip(zipfile = file, files = c(original_file, processed_file, both_file), mode = "cherry-pick")
    },
    contentType = "application/zip"
  )
}
# Run app
shinyApp(ui, server)