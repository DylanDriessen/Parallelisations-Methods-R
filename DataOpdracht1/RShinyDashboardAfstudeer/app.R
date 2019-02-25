#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
ram <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/ram_data.rds")
benchmark <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/microbenchmark_data.rds")
foreachDara <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_doparallel_foreach_peakRAM.rds")
clusterApplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_clusterapply_peakRAM.rds")
parlapplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_parlapply_peakRAM.rds")
sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_sequential_peakRAM.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
 sidebarPanel(
       helpText("Select wich axes"),
      selectInput(inputId = "input_coresX", label = "Choose a variabel to display:",
                  choices = c("Elapsed_Time_sec", "Total_RAM_Used_MiB", "Peak_RAM_Used_MiB", "Start_Time", "End_Time"),
                  selected = "Elapsed_Time_sec"
                  )),
        
    
#sidebarPanel(
  # selectInput(inputId = "input_plotX", label = "Choose a variabel to display:",
              #choices = c("Elapsed_Time_sec", "Total_RAM_Used_MiB", "Peak_RAM_used_MiB", "Start_Time", "End_Time"),
              #selected = "Total_RAM_used_MIB"
   #)),
 
  
  
  mainPanel(
        
    
    plotlyOutput("RamoutputSeqential"),
    plotlyOutput("ScatterplotCPU"),
    plottyOutput("ElapsedTime"),
    
    
      #plot_ly(
    #  x = row.names(ram),
      #y = ram[,"Elapsed_Time_sec"],
      #type = "bar")
    #),
 
 
    
  DT::dataTableOutput("table"),
  DT::dataTableOutput("tableBenchMark")
  )
)
  # sidebarPanel(
      #selectInput("input_id", "Choose a variabel to display:", 
                 # c(" ", "percent black", "percent Hispanic", "percent Asian"),
                  #"Percent White")
   # )
   
  
     


# Define server logic required to draw a histogram
server <- function(input, output) {
  #output$RamoutputSeqential <- renderPlot({ 
   #ggplot(data = sequentialData , aes(x = input$input_coresX, y = input$input_coresY)) + geom_bar()
  #}) 
  

  output$RamoutputSeqential <- renderPlotly({
   plot_ly(data = sequentialData, x = row.names(sequentialData) , y = sequentialData[,input$input_coresX], type = 'bar' ,
           mode = 'markers' )
  })
  
  output$ScatterplotCPU <- renderPlotly({
  plot_ly(data = foreachDara, x = foreachDara$Start_Time , y = foreachDara$Process_Id, type = 'scatter',
  mode = 'lines')
  })
  
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- sequentialData
  }))
 
   #output$ScatterplotCPU <- renderPlotly({
   # plot_ly ( x = input, y =
      # ,
     # type = ‘scatter’ ,
      #mode = ‘lines’
    #)
  #})
    
  #plot_ly(
  #  x = row.names(ram),
  #y = ram[,"Elapsed_Time_sec"],
  #type = "bar")
  #),
  
  
  
 
   
   #output$tableBenchMark <- DT::renderDataTable(DT::datatable({
    # data <- benchmark
  # }))
   
   #setwd("../")
  # source("lib/readFiles_peakRAM.r")
   #read_peakRAM_to_rds()
}

# Run the application 
shinyApp(ui = ui, server = server)

