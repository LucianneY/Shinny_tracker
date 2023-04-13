library(shiny)
library(shinydashboard)
library(dplyr)
library(data.table)
library(rtracklayer)
library(Gviz)

bw_file <- import("./Bigwigs/Sample_Ruoxi6928_2019_02_22_Mphage_TNF-IL4_6hrs_Donor1_treat_pileup.bw",format = "bigWig")
gen = "hg38"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "chr", label = "Chromosome", value = "chr1"),
      numericInput(inputId = "start", label = "Start position", value = 89993254),
      numericInput(inputId = "end", label = "End position", value = 93715990)
    ),
    mainPanel(
      plotOutput(outputId = "myplot", width = "100%", height = "600px") # Add width and height
    )
  )
)
 

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
  
  # Read in bigwig file
  bw_file <- reactive({import(paste0("./Bigwigs/test_",chr(),".bw"),format = "bigWig")})
  
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
  
  # create dTrack1
  dTrack1 <- reactive({
    DataTrack(bw_file(),
              type = "l",
              name="track1")
  })
  
  # create track.list
  track.list <- reactive({
    print(dTrack1())
    print(gTrack())
    list(dTrack1(), gTrack())
  })
  
  # create plot
  output$myplot <- renderPlot({
    plotTracks(track.list(),from=start(),to=end(),chromosome=chr())
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
