#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

import(c("shiny", "ggplot2", "plotly", "DT", "future", "promises", "markdown"))

setwd("../")
source("lib/readFiles_peakRAM.r")
source("lib/readFiles.r")
source("lib/realtime_sysinfo.r")
source("util/SaveFunctionData.r")
source("lib/preProcess_peakRAM.r")

import(c("readr","tibble","data.table", "peakRAM", "foreach", "doParallel", "parallel", "microbenchmark"))
plan(multiprocess)



ram <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/ram_data.rds")
#benchmark <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/microbenchmark_data.rds")

# Define UI for application that draws a histogram
ui <- shinyServer(fluidPage(
  
  #=======================#
  #conditions to adapt the selectInput to userInput
  #==========================#
  
  
  sidebarPanel(
    helpText("Select wich function"),
    selectInput(inputId = "callFunction", label = "Choose a function to display:",
                choices = c( ReadFile = "Read", PreProcess = "Pre", createCorpus = "Corpus", createDTM = "DTM",  deriveVocabulary = "Voc", Cluster = "Cluster"),
                selected = "Read"),
  
    
  
    
   
    
    
    conditionalPanel(
      condition = "input.callFunction == 'Read'",
      selectInput(inputId = "callMethodReadFiles", label = "Choose method to display",
                  choices = c("sequential", "clusterapply", "parlapply", "foreach"),
                              selected = "sequential")),
    
    conditionalPanel(
      condition = "input.callFunction == 'Pre'",
      selectInput(inputId = "callMethodPre", label = "Choose a method to display",
                  choices = c("Sequential", "Cluster", "DoParallelChunked", "ParallelChunked", "ClusterChunked"),
                  selected = "Cluster"
      )), 
    
    conditionalPanel(
      condition = "input.callFunction == 'Corpus'", 
      selectInput(inputId= "callMethodCorpus", label = "Choose a method to display",
                  choices = c("VCorpChunk", "VCorp", "Quan"))),
    
    
    conditionalPanel(
      condition = "input.callFunction == 'DTM'", 
      selectInput(inputId= "callMethodDTM", label = "Choose a method to display",
                  choices = c("methods to implement"))),
    
    conditionalPanel(
      condition = "input.callFunction == 'Voc'", 
      selectInput(inputId= "callMethodVoc", label = "Choose a method to display",
                  choices = c("methods to implement"))),
    
    conditionalPanel(
      condition = "input.callFunction == 'Cluster'", 
      selectInput(inputId= "callMethodCluster", label = "Choose a method to display",
                  choices = c("methods to implement"))),
    
    
    selectInput(inputId = "sizeBatch", label = "Choose a batch to calculate",
                choices = c("mini_batch","small_batch", "medium_batch", "big_batch",
                            selected = "mini_batch")),
    
    
    
    #==========================#
    #excecute Button
    #==========================#
    
    actionButton("runApp", "RUN")
    
    , width = 2),
  mainPanel(
    
    
    navbarPage("Choose your tab",
               tabPanel("Overzicht",
                 fluidRow(plotlyOutput("benchMarkSummary"), align = "center")
                 
              
                ),
               tabPanel("CorsInfo",
                        column( plotlyOutput("RAMoutputFunctions"), width = 6),
                        column(imageOutput("CPUusage"), width = 6)
                        ),
               tabPanel("Resources"
                         
               ),
               tabPanel("Live Feed",
                         fluidRow(
                           plotlyOutput("first_column"),
                           plotlyOutput("second_column")
          )
        )
      )
    , width = 10)
  )
)

    
#foreachData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_doparallel_foreach_peakRAM.rds")
#clusterApplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_clusterapply_peakRAM.rds")
#parlapplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_parlapply_peakRAM.rds")
#sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_sequential_peakRAM.rds")


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session){
  
  
 
  #====================================#
  #BenchmarkForDifferenctFunctions
  #====================================#
  
  output$benchMarkSummary <- renderPlotly({
      if(input$callFunction == "Read"){
        benchmarkReadSmall <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/benchmarkReadFilesSmall.rds")
        plot_ly(data = benchmarkReadSmall, x = benchmarkReadSmall$expr, y = benchmarkReadSmall$time * 10 ^-9) }
  })
  
      
  
  
  #====================================#
  #Asbenoeming
  #====================================#
  
    

    Elapsed <- list(
        title = "Elapsed_Time_sec"
    )
    Process <- list(
      title = "Process_id"
      )
    
  observeEvent(input$runApp, {
   
    #==========================#
    #eerste If else structuur voor de functie ReadFiles en de verschillende methoden
    #==========================#
    
    
    if(input$callFunction == "Read"){
     #==========================#
      #SequentialRead
      #==========================#
       if(input$callMethodReadFiles == "sequential"){
        
        future (read_sequential_peakRAM())
        sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_sequential_peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                  type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
          
                 
        })
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/docs/read_Sequential.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)}
      
      
      
      #==========================#
      #ClusterRead
      #==========================#
      
      else if(input$callMethodReadFiles == "clusterapply"){
        future(read_clusterapply_peakRAM())
        clusterApplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_clusterapply_peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = clusterApplyData, x = clusterApplyData$Elapsed_Time_sec , y = clusterApplyData$Process_id, type = 'bar' ,
                  mode = 'markers' ) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/docs/read_clusterapply.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
      }
      
      #==========================#
      #parLapplyRead
      #==========================#
      
       else if(input$callMethodReadFiles == "parlapply"){
        future(read_parlapply_peakRAM())
        parlapplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_parlapply_peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = parlapplyData, x = parlapplyData$Elapsed_Time_sec , y = parlapplyData$Process_id, type = 'bar' ,
                  mode = 'markers' ) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/docs/read_parLapply.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
      }
    
    #==========================#
    #forEachRead
    #==========================#
    
    else if(input$callMethodReadFiles == "foreach"){
      print("HELLO")
      future(read_doparallel_foreach_peakRAM())
      foreachData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_doparallel_foreach_peakRAM.rds")
      output$RAMoutputFunctions <- renderPlotly({
        plot_ly(data = foreachData, x = foreachData$Elapsed_Time_sec , y = foreachData$Process_id , type = 'bar' ,
                mode = 'markers' ) %>% layout(xaxis = Elapsed, yaxis = Process)
      })
      
      output$CPUusage <- renderImage({
        return(list(
          src = "~/R/Afstudeerwerk/DataOpdracht1/docs/read_doparallel_foreach_PNG.png",
          contentType = "image/png",
          alt = "Face"
        ))
      },deleteFile = FALSE)
      
      }
    }
    
    #conditionalPanel(
     # condition = "input.callFunction == 'Pre'",
    #  selectInput(inputId = "callMethodPre", label = "Choose a method to display",
        #          choices = c("Cluster", "DoParallelChunked", "ParallelChunked", "ClusterChunked"),
       #           selected = "Cluster"
      #)), 
    
    else if(input$callFunction == "Pre" ){
      if(input$callMethodPre == "Sequential"){
        future(saveFunctionData(preProcessSequential_peakRAM, "results/preProcess/sequential")
              
                )
      }
          
        
      
      }
    })
  
  #   benchmark_read()
   # benchmark <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/benchmarkReadFilesSmall.rds")
    #output$OverallScore <- renderPlotly({
     # plot_ly(data = benchmark, x = benchmark$expr, y = benchmark$time * 10^-9, type = 'bar', mode = 'markers')
    #})
  #})
  
  
  #selectInput(inputId= "callMethodCorpus", label = "Choose a function to display",
   #           choices = c("VCorpChunk", "VCorp", "Quan"))),

  
  
  
  Elapsed <- list(
    title = "Elapsed_Time_sec"
  )
  RAM <- list(
    title = "RAMusage"
  )
  
  RAM2 <- list(
    title = "CPUusage"
  )
  
 
  
  #====================================#
  #Code voor de REAL time RAM usage
  #====================================#
  
    onStop(function() tclTaskDelete())
  
  
    
    get_new_data <- function(){
      data <-c(time = as.numeric(Sys.time())  , ram = as.numeric(system("../scripts/my_ram_usage.sh", intern = TRUE))/1024/1024) %>% rbind %>% data.frame
      return(data)
    }
    
    my_data <<- get_new_data()
    
    update_data <- function(){
      my_data <<- rbind(get_new_data(), my_data)
    }
    
    
    
    output$first_column <- renderPlotly({
      print("Render")
      invalidateLater(1000, session)
      update_data()
      print(my_data)
      plot_ly(data = my_data, x = my_data$time, y = my_data$ram,  type = "scatter",
              mode = "lines", height = 300, width = 1100) %>% layout(xaxis = Elapsed, yaxis = RAM)
    })
    
    
    #====================================#
    #Code voor de REAL time CPU usage
    #====================================#
    
    get_new_data2 <- function(){
      data <-c(time = as.numeric(Sys.time())  , ram = as.numeric(system("../scripts/my_cpu_usage.sh", intern = TRUE))/1024/1024) %>% rbind %>% data.frame
      return(data)
    }
    
    my_data2 <<- get_new_data2()
    
    update_data2 <- function(){
      my_data2 <<- rbind(get_new_data(), my_data2)
    }
    
    
    
    output$second_column <- renderPlotly({
      print("Render")
      invalidateLater(1000, session)
      update_data2()
      print(my_data)
      plot_ly(data = my_data2, x = my_data2$time, y = my_data2$ram,  type = "scatter",
              mode = "lines", height = 300, width = 1100) %>% layout(xaxis = Elapsed, yaxis = RAM2)
    })
    
    
    
 })

shinyApp(ui = ui, server = server)







