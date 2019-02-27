#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

import(c("shiny", "ggplot2", "plotly", "DT", "future", "promises"))

setwd("../")
source("lib/readFiles_peakRAM.r")
source("lib/readFiles.r")
source("lib/realtime_sysinfo.r")
import(c("readr","tibble","data.table", "peakRAM", "foreach", "doParallel", "parallel", "microbenchmark"))




ram <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/ram_data.rds")
#benchmark <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/microbenchmark_data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
 sidebarPanel(
      helpText("Select wich function"),
      selectInput(inputId = "callFunction", label = "Choose a function to display:",
                  choices = c( ReadFile = "Read", createCorpus = "Corpus", createDTM = "DTM",  deriveVocabulary = "Voc", Cluster = "Cluster"),
                  selected = "Elapsed_Time_sec"
                  ),
 
      conditionalPanel(
        condition = "input.callFunction == 'Read'",
        selectInput(inputId = "callMethodReadFiles", label = "Choose method to display",
                   choices = c("sequential", "clusterapply", "parlapply", "foreach"))
      ),
 
      conditionalPanel(
        condition = "input.callFunction == 'Corpus'", 
        selectInput(inputId= "callMethodCorpus", label = "Choose a function to display",
              choices = c("VCorpChunk", "VCorp", "Quan"))),
  
      
      conditionalPanel(
        condition = "input.callFunction == 'DTM'", 
        selectInput(inputId= "callMethodDTM", label = "Choose a function to display",
               choices = c("methods to implement"))),
      
      conditionalPanel(
        condition = "input.callFunction == 'Voc'", 
        selectInput(inputId= "callMethodVoc", label = "Choose a function to display",
               choices = c("methods to implement"))),
  
     conditionalPanel(
         condition = "input.callFunction == 'Cluster'", 
          selectInput(inputId= "callMethodCluster", label = "Choose a function to display",
               choices = c("methods to implement"))),
     
    selectInput(inputId = "sizeBatch", label = "Choose a batch to calculate",
                choices = c("mini batch","small batch", "medium batch", "big batch")),
     
      actionButton("runApp", "RUN")
     
 ),
  mainPanel(
    
    conditionalPanel(condition = "input.callFunction == 'Read'",
                     actionButton("excecuteSequential", "Render Sequential"),
                     actionButton("excecuteForEach", "Render forEach"),
                     actionButton("excecuteclusterApplyData", "Render Cluster"),
                     actionButton("excecuteparlapply", "Render Parlapply")),
        
    conditionalPanel(condition = "input.callFunction == 'Corpus'",
                    actionButton("excecuteVCorpChunk", "Show VCorpChunk"),
                    actionButton("excecuteVCorp", "Show VCorp"),
                    actionButton("excecuteQuan", "Show Quan")
),
    
    
    


    plotlyOutput("RAMoutputFunctions"),
    imageOutput("CPUusage"),
    plotlyOutput("ram_vector"),
    plotlyOutput("OverallScore"),
    plotlyOutput("vector")
  )
)
    
    
    
  
    
    
      #plot_ly(
    #  x = row.names(ram),
      #y = ram[,"Elapsed_Time_sec"],
      #type = "bar")
    #),
 
 
    
  
  

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
server <- function(input, output, session) {
  
  #fileReaderData <- reactiveFileReader(500, NULL,
                                     #  "~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/ram_vector.rds", readRDS)

  
 
   output$ram_vector <- renderPlotly({
    invalidateLater(1000, session)
    plot_ly(y = ram_vector,
            x = c(0: (length(ram_vector)-1)),
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
     read_sequential_peakRAM()
     sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_sequential_peakRAM.rds")
     output$RAMoutputFunctions <- renderPlotly({
       plot_ly(data = sequentialData, x = row.names(sequentialData) , y = sequentialData[,input$input_coresX], type = 'bar' ,
               mode = 'markers' )
     })
     
     output$CPUusage <- renderImage({
       return(list(
         src = "~/R/Afstudeerwerk/DataOpdracht1/docs/read_Sequential.png",
         contentType = "image/png",
         alt = "Face"
       ))
     },deleteFile = FALSE)
     
 })
  
 observeEvent(input$excecuteForEach, {
    read_doparallel_foreach_peakRAM()
     foreachData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_doparallel_foreach_peakRAM.rds")
     output$RAMoutputFunctions <- renderPlotly({
      plot_ly(data = foreachData, x = row.names(foreachData) , y = foreachData[,input$input_coresX], type = 'bar' ,
              mode = 'markers' )
    })
     
     output$CPUusage <- renderImage({
       return(list(
         src = "~/R/Afstudeerwerk/DataOpdracht1/docs/read_doparallel_foreach_PNG.png",
         contentType = "image/png",
         alt = "Face"
       ))
     },deleteFile = FALSE)
     
  })
  
  observeEvent(input$excecuteclusterApplyData,{
    read_clusterapply_peakRAM()
    clusterApplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_clusterapply_peakRAM.rds")
    output$RAMoutputFunctions <- renderPlotly({
      plot_ly(data = clusterApplyData, x = row.names(clusterApplyData) , y = clusterApplyData[,input$input_coresX], type = 'bar' ,
              mode = 'markers' )
    })
    output$CPUusage <- renderImage({
      return(list(
        src = "~/R/Afstudeerwerk/DataOpdracht1/docs/readFiles_clusterapply.png",
        contentType = "image/png",
        alt = "Face"
      ))
    },deleteFile = FALSE)
    
  })
  
  observeEvent(input$excecuteparlapply, {
    read_parlapply_peakRAM()
    parlapplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_parlapply_peakRAM.rds")
    output$RAMoutputFunctions <- renderPlotly({
      plot_ly(data = parlapplyData, x = row.names(parlapplyData) , y = parlapplyData[,input$input_coresX], type = 'bar' ,
              mode = 'markers' )
    })
    output$CPUusage <- renderImage({
      return(list(
        src = "~/R/Afstudeerwerk/DataOpdracht1/docs/read_parLapply.png",
        contentType = "image/png",
        alt = "Face"
      ))
    },deleteFile = FALSE)
    
  })
  
  
}

shinyApp(ui = ui, server = server)


 




