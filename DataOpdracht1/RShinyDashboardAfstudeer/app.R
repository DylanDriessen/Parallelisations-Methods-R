#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

import(c("shiny", "ggplot2", "plotly", "DT"))

setwd("../")
source("lib/readFiles_peakRAM.r")
source("lib/readFiles.r")
source("lib/realtime_sysinfo.r")


ram <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/ram_data.rds")

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
        
    actionButton("excecuteSequential", "Render Sequential"),
    actionButton("excecuteForEach", "Render forEach"),
    actionButton("excecuteclusterApplyData", "Render Cluster"),
    actionButton("excecuteparlapply", "Render Parlapply"),
    actionButton("overallTime", "Render overall time"),
    
    plotlyOutput("RAMoutputFunctions"),
    plotlyOutput("ram_vector"),
    plotlyOutput("OverallScore"),
    plotlyOutput("vector"),  
    
    
    
  
    
    
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
   


#foreachData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_doparallel_foreach_peakRAM.rds")
#clusterApplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_clusterapply_peakRAM.rds")
#parlapplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_parlapply_peakRAM.rds")
#sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_sequential_peakRAM.rds")


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$ram_vector <- renderPlotly({
    plot_ly(y = ram_vector,
            type = "scatter",
            mode = "lines")
  })
  
  observeEvent(input$overallTime, {
    benchmark_read()
    benchmark <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/benchmarkReadFilesSmall.rds")
    output$OverallScore <- renderPlotly({
      plot_ly(data = benchmark, x = benchmark$expr, y = benchmark$time * 10^-9, type = 'bar', mode = 'markers')
    })
  })
    
  
 observeEvent(input$excecuteSequential,{
   start_monitor()
   read_sequential_peakRAM()
   end_monitor()
    sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_sequential_peakRAM.rds")
    output$RAMoutputFunctions <- renderPlotly({
      plot_ly(data = sequentialData, x = row.names(sequentialData) , y = sequentialData[,input$input_coresX], type = 'bar' ,
              mode = 'markers' )
    })
  })
  
  observeEvent(input$excecuteForEach, {
    read_doparallel_foreach_peakRAM()
     foreachData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_doparallel_foreach_peakRAM.rds")
     output$RAMoutputFunctions <- renderPlotly({
      plot_ly(data = foreachData, x = row.names(foreachData) , y = foreachData[,input$input_coresX], type = 'bar' ,
              mode = 'markers' )
    })
  })
  
  observeEvent(input$excecuteclusterApplyData,{
    read_clusterapply_peakRAM()
    clusterApplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_clusterapply_peakRAM.rds")
    output$RAMoutputFunctions <- renderPlotly({
      plot_ly(data = clusterApplyData, x = row.names(clusterApplyData) , y = clusterApplyData[,input$input_coresX], type = 'bar' ,
              mode = 'markers' )
    })
  })
  
  observeEvent(input$excecuteparlapply, {
    read_parlapply_peakRAM()
    parlapplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_parlapply_peakRAM.rds")
    output$RAMoutputFunctions <- renderPlotly({
      plot_ly(data = parlapplyData, x = row.names(parlapplyData) , y = parlapplyData[,input$input_coresX], type = 'bar' ,
              mode = 'markers' )
    })
  })
  
  output$vector <- renderPlotly({
    plot_ly ( x = c(1,2,3,4),
              type = "scatter",
              mode = "lines"
    )
  })
  
  
  
  
  
  
  #output$table <- DT::renderDataTable(DT::datatable({
   # data <- sequentialData
#  }))
 
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

