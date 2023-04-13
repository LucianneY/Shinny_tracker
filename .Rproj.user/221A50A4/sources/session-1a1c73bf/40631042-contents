# Define UI function
ui <- function() {
  
  # Define chromosomes in bigwig file
  chroms <- seqnames(bw_file)
  
  # Define user interface elements
  fluidPage(
    
    # Page title
    titlePanel("Bigwig Viewer"),
    
    # Sidebar with input controls
    sidebarLayout(
      
      sidebarPanel(
        
        # Chromosome selection
        selectInput("chrom", "Chromosome:", choices = chroms, selected = chroms[1]),
        
        # Start and end position inputs
        numericInput("start", "Start position:", value = 1),
        numericInput("end", "End position:", value = 1000),
        
        # Zoom level indicator
        h3("Zoom level:"),
        verbatimTextOutput("zoom_level")
        
      ),
      
      # Main panel with output displays
      mainPanel(
        
        # Data table of selected region
        dataTableOutput("selected_region_table")
        
      )
    )
  )
}


# Run app
shinyApp(ui = ui, server = server)
