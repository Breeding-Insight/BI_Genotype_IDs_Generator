# Load required libraries
library(shiny)
library(tidyverse)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML(".logo-header { text-align: center; margin-bottom: 20px; }"))
  ),
  
  # Image header
  div(class = "logo-header",
      tags$img(src = "logos2.png", height = "100px")
  ),
  
  titlePanel("DArTag Plate Formatter"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("dartfile", "Upload your DArT Submission file", accept = ".csv"),
      textInput("outname", "Enter the base name for output files", placeholder = "ENTER PROJECT NAME", value = ""),
      
      h4("Download Options"),
      downloadButton("download_all", "Download Tracking Files (.zip)")
    ),
    mainPanel(
      h4("Preview of processed data (first 10 rows)"),
      tableOutput("preview")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  raw_data <- reactive({
    req(input$dartfile)
    read_csv(input$dartfile$datapath, show_col_types = FALSE)
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
        Column = str_pad(as.integer(Column), width = 2, pad = "0"),
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
    head(processed_only(), 10)
  })
  
  # Single download handler for all files zipped
  output$download_all <- downloadHandler(
    filename = function() {
      paste0(input$outname, "_tracking_files.zip")
    },
    content = function(file) {
      # Temporary files
      tmpdir <- tempdir()
      original_file <- file.path(tmpdir, paste0(input$outname, "_Breeder_IDs.csv"))
      processed_file <- file.path(tmpdir, paste0(input$outname, "_DArT_IDs.csv"))
      both_file <- file.path(tmpdir, paste0(input$outname, "_ID_key.csv"))
      
      # Write CSVs
      raw_data() %>% write_csv(original_file)
      processed_only() %>% write_csv(processed_file)
      processed_both() %>% write_csv(both_file)
      
      # Zip them together
      zip::zip(zipfile = file, files = c(original_file, processed_file, both_file), mode = "cherry-pick")
    }
  )
}

# Run app
shinyApp(ui, server)