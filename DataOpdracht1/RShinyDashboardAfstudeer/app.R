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
                choices = c( ReadFile = "Read", createCorpus = "Corpus", createDTM = "DTM",  deriveVocabulary = "Voc", Cluster = "Cluster"),
                selected = "Elapsed_Time_sec"
    ),
    
    conditionalPanel(
      condition = "input.callFunction == 'Read'",
      selectInput(inputId = "callMethodReadFiles", label = "Choose method to display",
                  choices = c("sequential", "clusterapply", "parlapply", "foreach", "all",
                              selected = "sequential"))
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
                choices = c("mini_batch","small_batch", "medium_batch", "big_batch",
                            selected = "mini_batch")),
    
    #==========================#
    #excecute Button
    #==========================#
    
    actionButton("runApp", "RUN")
    
    , width = 2),
  mainPanel(
    
    fluidRow(
     
      #==========================#
       #Buttons to show all the plots from different method/functions
      #==========================#
      
      
      conditionalPanel(condition = "input.callFunction == 'Read'",
                       actionButton("excecuteSequential", "Render Sequential"),
                       actionButton("excecuteForEach", "Render forEach"),
                       actionButton("excecuteclusterApplyData", "Render Cluster"),
                       actionButton("excecuteparlapply", "Render Parlapply")),
      
      conditionalPanel(condition = "input.callFunction == 'Corpus'",
                       actionButton("excecuteVCorpChunk", "Show VCorpChunk"),
                       actionButton("excecuteVCorp", "Show VCorp"),
                       actionButton("excecuteQuan", "Show Quan"))
      
      
    ),
    
    fluidRow( class = "MyRow2",
      column( plotlyOutput("RAMoutputFunctions"), width = 6),
      column( imageOutput("CPUusage"), width = 6)
      
        
      ),
    
    fluidRow(
    plotlyOutput("first_column")
    )
    
   
  )
  ))

    
#foreachData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_doparallel_foreach_peakRAM.rds")
#clusterApplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_clusterapply_peakRAM.rds")
#parlapplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_parlapply_peakRAM.rds")
#sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_sequential_peakRAM.rds")


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session){
 
  
  #====================================#
  #ObserveEvents voor de knoppen van readFiles met image en plot
  #====================================#
  
  #conditionalPanel(
    #condition = "input.callFunction == 'Read'",
    #selectInput(inputId = "callMethodReadFiles", label = "Choose method to display",
    #            choices = c("sequential", "clusterapply", "parlapply", "foreach",
   #                         selected = "sequential"))
  #),
    
  observeEvent(input$runApp, {
   
    #==========================#
    #eerste If else structuur voor de functie ReadFiles en de verschillende methoden
    #==========================#
    
    
    if(input$callFunction == "Read"){
      
      Elapsed <- list(
        title = "Elapsed_Time_sec"
      )
      Process <- list(
        title = "Process_id"
      )
                      
      #==========================#
      #SequentialRead
      #==========================#
       if(input$callMethodReadFiles == "sequential"){
        future (read_sequential_peakRAM())
        sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/read_sequential_peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, type = 'bar')%>% layout(xaxis = Elapsed, yaxis = Process)
          
                 
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
                mode = 'markers' )%>% layout(xaxis = Elapsed, yaxis = Process)
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
  })
  
  #   benchmark_read()
   # benchmark <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/benchmarkReadFilesSmall.rds")
    #output$OverallScore <- renderPlotly({
     # plot_ly(data = benchmark, x = benchmark$expr, y = benchmark$time * 10^-9, type = 'bar', mode = 'markers')
    #})
  #})
  
  
  

  
  
  
  
 
  
  #====================================#
  #Code voor de REAL time RAM usage
  #====================================#
  
    onStop(function() tclTaskDelete())
  
  
    
    get_new_data <- function(){
      data <-c(time = as.numeric(Sys.time())  , ram = as.numeric(system("../scripts/my_ram_usage.sh", intern = TRUE))/1024/1024) %>% rbind %>% data.frame
      return(data)
    }
    
    # Initialize my_data
    my_data <<- get_new_data()
    
    # Function to update my_data
    update_data <- function(){
      my_data <<- rbind(get_new_data(), my_data)
    }
    
    # Plot the 30 most recent values
    output$first_column <- renderPlotly({
      print("Render")
      invalidateLater(1000, session)
      update_data()
      print(my_data)
      plot_ly(data = my_data, x = my_data$time, y = my_data$ram,  type = "scatter",
              mode = "lines", height = 300, width = 1100)
    })
 })

shinyApp(ui = ui, server = server)







