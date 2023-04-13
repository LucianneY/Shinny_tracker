# Load packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(data.table)
library(rtracklayer)
library(Gviz)

# Read in bigwig file
bw_file <- import("./Bigwigs/test_chr1.bw",format = "bigWig")


# create genome region
gr1 <- GRanges(
  seqnames=c("chr1", "chr1"),
  ranges=IRanges(c(89993254,93667062), c(89998989, 93715990)),
  strand=c("+", "+")
)

gen = "hg38"
gr1a <- as.data.frame(gr1)
start=gr1a[1,2]
end=gr1a[1,3]

gTrack <- BiomartGeneRegionTrack(
  genome = gen,
  chromosome = "chr1",
  start = start,
  end = end,
  name = "ENSEMBL")

dTrack1 <- DataTrack(
  bw_file,
  type = "l",
  name="track1")

track.list=list(dTrack1, gTrack)
plotTracks(track.list,from=start,to=end,chromsome="chr1")

server <- function(input, output) {
  
  # Load the bigWig file
  bw_file <- import(bw_path)
  
  # Define reactive values
  chroms <- sort(unique(as.character(seqinfo(read.bigWig(bw_file))$seqnames)))
  chrom_sizes <- as.data.frame(seqinfo(read.bigWig(bw_file)))
  
  # Define reactive function to read data from bigWig file
  bw_data <- reactive({
    req(input$chrom)
    req(input$start)
    req(input$end)
    read.bigWig(bw_file, which = input$chrom, start = input$start, end = input$end)
  })
  
  # Output the plot
  output$plot <- renderPlot({
    plot(bw_data(), main = "BigWig Data", xlab = "Position", ylab = "Value")
  })
  
  # Update the chromEnd field when the chromosome changes
  observe({
    if (!is.null(input$chrom)) {
      updateNumericInput(session, "end", max(seqinfo(read.bigWig(bw_file))[input$chrom]))
    }
  })
  
  # Define reactive function to return chrom sizes for selected chromosome
  chrom_sizes_reactive <- reactive({
    chrom_sizes[chrom_sizes$seqnames == input$chrom, c("seqnames", "seqlength")]
  })
  
  # Output the chromosome sizes table
  output$chrom_sizes <- renderTable({
    chrom_sizes_reactive()
  })
  
  # Define the select input for chromosomes
  output$chrom <- renderUI({
    selectInput("chrom", "Chromosome:", choices = chroms)
  })
  
  # Define the numeric input for start position
  output$start <- renderUI({
    numericInput("start", "Start Position:", min = 1, max = chrom_sizes_reactive()$seqlength[1], value = 1)
  })
  
  # Define the numeric input for end position
  output$end <- renderUI({
    numericInput("end", "End Position:", min = 1, max = chrom_sizes_reactive()$seqlength[1], value = chrom_sizes_reactive()$seqlength[1])
  })
}










