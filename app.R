library(shiny)
library(shinydashboard)
library(dplyr)
library(data.table)
library(rtracklayer)
library(Gviz)

#bw_file <- import("./Bigwigs/test_chr1.bw",format = "bigWig")

gen = "hg38"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "chr", label = "Chromosome", value = "chr1"),
      numericInput(inputId = "start", label = "Start position", value = 89993254),
      numericInput(inputId = "end", label = "End position", value = 93715990),
      actionButton("update_button", "Update plot")
    ),
    mainPanel(
      plotOutput(outputId = "myplot", width = "100%", height = "600px") # Add width and height
    )
  )
)
 
# load bigwig files and assign to objects
bigwigs <- list.files(path = "./Bigwigs", pattern = "\\.bw$", full.names = TRUE)
for (bw_file in bigwigs) {
  bw_obj_name <- gsub("\\.bw", "", basename(bw_file))
  assign(bw_obj_name, import(bw_file, format = "bigWig"))
}
# Extract target strings from file names
target_objects <- gsub("\\.bw", "", basename(bigwigs))


server <- function(input, output) {
  # Load packages
  library(dplyr)
  library(data.table)
  library(rtracklayer)
  library(Gviz)
  
  # create reactive values for chr, start, and end
  chr <- reactive({input$chr})
  start <- reactive({input$start})
  end <- reactive({input$end})
  
  # Define function to read bigwig files
  read_bigwig_file <- function() {
    
    # Prompt user to select a file
    bw_file <- file.choose()
    
    # Prompt user to specify variable name for imported object
    bw_obj_name <- readline(prompt = "Enter a variable name for the imported object: ")
    
    # Import bigWig file and assign to variable with user-specified name
    assign(bw_obj_name, import(bw_file, format = "bigWig"), envir = .GlobalEnv)
    
    # Return the name of the variable assigned to the imported object
    return(bw_obj_name)
  }
  
  # create genome region
  gr1 <- reactive({
    gr <- GRanges(seqnames=chr(),
                  ranges=IRanges(start(), end()),
                  strand=c("+", "+"))
    as.data.frame(gr)
  })
  
  # create gTrack
  gTrack <- reactive({
    BiomartGeneRegionTrack(genome = "hg38",
                           chromosome = chr(),
                           start = start(),
                           end = end(),
                           name = "ENSEMBL")
  })
  
  # create dTracks
  dTracks <- lapply(target_objects, function(obj) {
    DataTrack(get(obj),
              type = "l",
              name = obj)
  })
  
  # create track.list
  track.list <- reactive({
    c(dTracks, list(gTrack()))
  })
  
  # create plot
  output$myplot <- renderPlot({
    plotTracks(track.list(),from=start(),to=end(),chromosome=chr(),
               type = "hist",
               col.title = "black",
               col.axis = "black",
               background.title = "white",
               cex.title = 0.75)
  })
  
  # display the selected track range
  output$range <- renderPrint({
    paste("Selected range: ", start(), " - ", end())
  })
  
  # create input controls
  output$inputPanel <- renderUI({
    tagList(
      selectInput(inputId = "chr",
                  label = "Select chromosome:",
                  choices = c("chr1", "chr2", "chr3"),
                  selected = "chr1"),
      numericInput(inputId = "start",
                   label = "Start:",
                   value = 89993254),
      numericInput(inputId = "end",
                   label = "End:",
                   value = 93715990)
    )
  })
}


shinyApp(ui, server)
