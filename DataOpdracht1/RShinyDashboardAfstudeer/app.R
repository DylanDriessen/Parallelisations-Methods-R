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
  
  
  
    
    
    
    #==========================#
    #excecute Button
    #==========================#
    

 
    
    
    navbarPage("Choose your tab",
               tabPanel("Overzicht",
                        sidebarPanel(
                          helpText("Select wich function"),
                          selectInput(inputId = "callFunction", label = "Choose a function to display:",
                                      choices = c( ReadFile = "Read", PreProcess = "Pre", createCorpus = "Corpus", createDTM = "DTM", Cluster = "Cluster")
                                      ),
                          actionButton("showResult", "showResult"),
                          width = 2),
                         mainPanel(
                        fluidRow(plotlyOutput("benchMarkSummary"), align = "center"))
                   ),
               tabPanel("CorsInfo",
                        sidebarPanel(
                          helpText("Select wich function"),
                          selectInput(inputId = "callFunction2", label = "Choose a function to display:",
                                      choices = c( ReadFile = "Read", PreProcess = "Pre", createCorpus = "Corpus", createDTM = "DTM", Cluster = "Cluster")
                                      ),
                          
                          conditionalPanel(
                            condition = "input.callFunction2 == 'Read'",
                            selectInput(inputId = "callMethodReadFiles", label = "Choose method to display",
                                        choices = c("sequential", "clusterapply", "parlapply", "foreach"))),
                          
                          conditionalPanel(
                            condition = "input.callFunction2 == 'Pre'",
                            selectInput(inputId = "callMethodPre", label = "Choose a method to display",
                                        choices = c("Sequential", "Cluster", "DoParallelChunked", "ParallelChunked", "ClusterChunked")
                            )), 
                          
                          conditionalPanel(
                            condition = "input.callFunction2 == 'Corpus'", 
                            selectInput(inputId= "callMethodCorpus", label = "Choose a method to display",
                                        choices = c("Cluster", "VCorp", "Quan"))),
                          
                          
                          conditionalPanel(
                            condition = "input.callFunction2 == 'DTM'", 
                            selectInput(inputId= "callMethodDTM", label = "Choose a method to display",
                                        choices = c("createDfmChunks", "createDFM", "createDFMasDTM" ))),
                          
                           conditionalPanel(
                            condition = "input.callFunction2 == 'Cluster'", 
                            selectInput(inputId= "callMethodCluster", label = "Choose a method to display",
                                        choices = c("methods to implement"))), 
                          actionButton("showResultCors", "showResultCors"),
                          width = 2),
                        
                         mainPanel(
                        column( plotlyOutput("RAMoutputFunctions"), width = 6),
                        column(imageOutput("CPUusage"), width = 6), width = 10)
               ),
               tabPanel("Resources",
                        sidebarPanel(
                          helpText("Select wich function"),
                          selectInput(inputId = "callFunction3", label = "Choose a function to display:",
                                      choices = c( ReadFile = "Read", PreProcess = "Pre", createCorpus = "Corpus", createDTM = "DTM", Cluster = "Cluster")),
                          
                          conditionalPanel(
                            condition = "input.callFunction3 == 'Read'",
                            selectInput(inputId = "callMethodReadFiles", label = "Choose method to display",
                                        choices = c("sequential", "clusterapply", "parlapply", "foreach"))),
                          
                          conditionalPanel(
                            condition = "input.callFunction3 == 'Pre'",
                            selectInput(inputId = "callMethodPre", label = "Choose a method to display",
                                        choices = c("Sequential", "Cluster", "DoParallelChunked", "ParallelChunked", "ClusterChunked")
                                        
                            )), 
                          
                          conditionalPanel(
                            condition = "input.callFunction3 == 'Corpus'", 
                            selectInput(inputId= "callMethodCorpus", label = "Choose a method to display",
                                        choices = c("Cluster", "VCorp", "Quan"))),
                          
                          
                          conditionalPanel(
                            condition = "input.callFunction3 == 'DTM'", 
                            selectInput(inputId= "callMethodDTM", label = "Choose a method to display",
                                        choices = c("createDfmChunks", "createDFM", "createDFMasDTM" ))),
                          
                          
                          
                          conditionalPanel(
                            condition = "input.callFunction3 == 'Cluster'", 
                            selectInput(inputId= "callMethodCluster", label = "Choose a method to display",
                                        choices = c("methods to implement"))),
                          actionButton("showResultResources", "showResultResources"),
                          width = 2),
                        
                        
                        
                        
                        mainPanel(
                        column(plotlyOutput("RamUsagePlot"), width = 6),
                        column(plotlyOutput("CPUusagePlot"),width = 6), width = 10)
               ),
               tabPanel("Compare",
                        sidebarPanel(
                          helpText("Select wich function"),
                          selectInput(inputId = "callFunction4", label = "Choose a function to compare",
                                      choices = c( ReadFile = "Read", PreProcess = "Pre", createCorpus = "Corpus", createDTM = "DTM", Cluster = "Cluster")),
                          #===================#
                          conditionalPanel(
                            condition = "input.callFunction4 == 'Read'",
                            selectInput(inputId = "callMethodReadFiles", label = "Choose method to compare",
                                        choices = c("sequential", "clusterapply", "parlapply", "foreach"))),
                          #CompareCondition
                          conditionalPanel(
                            condition = "input.callFunction4 == 'Read'",
                            selectInput(inputId = "callMethodReadFilesCompare", label = "Choose method to compare",
                                        choices = c("sequential", "clusterapply", "parlapply", "foreach")
                                        )),
                          #===================#
                          #===================#
                          conditionalPanel(
                            condition = "input.callFunction4 == 'Pre'",
                            selectInput(inputId = "callMethodPre", label = "Choose a method to compare",
                                        choices = c("Sequential", "Cluster", "DoParallelChunked", "ParallelChunked", "ClusterChunked"))), 
                          #CompareCondition
                          conditionalPanel(
                            condition = "input.callFunction4 == 'Pre'",
                            selectInput(inputId = "callMethodPreCompare", label = "Choose a method to compare",
                                        choices = c("Sequential", "Cluster", "DoParallelChunked", "ParallelChunked", "ClusterChunked"))),
                          #===================#
                          #===================#
                          conditionalPanel(
                            condition = "input.callFunction4 == 'Corpus'", 
                            selectInput(inputId= "callMethodCorpus", label = "Choose a method to compare",
                                        choices = c("Cluster", "VCorp", "Quan"))),
                          #CompareCondition
                          
                          conditionalPanel(
                            condition = "input.callFunction4 == 'Corpus'", 
                            selectInput(inputId= "callMethodCorpusCompare", label = "Choose a method to compare",
                                        choices = c("Cluster", "VCorp", "Quan"))),
                          #===================#
                          #===================#
                          
                          conditionalPanel(
                            condition = "input.callFunction4 == 'DTM'", 
                            selectInput(inputId= "callMethodDTM", label = "Choose a method to compare",
                                        choices = c("createDfmChunks", "createDFM", "createDFMasDTM" ))),
                          
                          conditionalPanel(
                            condition = "input.callFunction4 == 'DTM'", 
                            selectInput(inputId= "callMethodDTMCompare", label = "Choose a method to compare",
                                        choices = c("createDfmChunks", "createDFM", "createDFMasDTM" ))),
                          #===================#
                          #===================#
                          conditionalPanel(
                            condition = "input.callFunction4 == 'Cluster'", 
                            selectInput(inputId= "callMethodCluster", label = "Choose a method to compare",
                                        choices = c("methods to implement"))),
                          
                        
                        conditionalPanel(
                          condition = "input.callFunction4 == 'Cluster'", 
                          selectInput(inputId= "callMethodClusterCompare", label = "Choose a method to compare",
                                      choices = c("methods to implement"))), 
                        actionButton("showResult", "showResult"),
                        width = 2),
                       
                        
                        mainPanel(
                          
                        ))
                      ,
               
               tabPanel("Live Feed",
                        fluidRow(
                          plotlyOutput("first_column"),
                          plotlyOutput("second_column")
                        )
               )
               
    )))
    




# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session){
  start_monitor()
  
  
  
  #====================================#
  #Asbenoeming
  #====================================#
  
  
  
  Elapsed <- list(
    title = "Elapsed_Time_sec"
  )
  Process <- list(
    title = "Process_id"
  )
  
  
  observeEvent(input$showResult, {
    
    #==========================#
    #overzicht Structuur   
    #==========================#
    
    
    if(input$callFunction == "Read"){
      print(input$callFunction2)
      
      print("CHECKREAD")
      output$benchMarkSummary <- renderPlotly({
      benchmarkReadSmall <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/microbenchmark.rds")
      plot_ly(data = benchmarkReadSmall, x = benchmarkReadSmall$expr, y = as.numeric(benchmarkReadSmall$time) *10^-9) 
        
      })
      
      if(input$callMethodReadFiles == "sequential"){
        
        future(saveFunctionData(read_sequential_peakRAM, "results/readFiles/sequential"))
        sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/sequential/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                  type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
          
          
        })
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/sequential/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/sequential/resources.rds")
        
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
      }
      else if(input$callMethodReadFiles == "clusterapply"){
        #future(read_clusterapply_peakRAM())
        clusterApplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = clusterApplyData, x = clusterApplyData$Elapsed_Time_sec , y = clusterApplyData$Process_id, type = 'bar' ,
                  mode = 'markers' ) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodReadFiles == "parlapply"){
        future(read_parlapply_peakRAM())
        parlapplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = parlapplyData, x = parlapplyData$Elapsed_Time_sec , y = parlapplyData$Process_id, type = 'bar' ,
                  mode = 'markers' ) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
      }
      else if(input$callMethodReadFiles == "foreach"){
        
        future(read_doparallel_foreach_peakRAM())
        #future(saveFunctionData(read_doparallel_foreach_peakRAM,"results/read/foreach"))
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
    else if(input$callFunction == "Pre"){
      print("CHECK")
      output$benchMarkSummary <- renderPlotly({
      benchmarkReadSmall <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/microbenchmark.rds")
      plot_ly(data = benchmarkReadSmall, x = benchmarkReadSmall$expr, y = as.numeric(benchmarkReadSmall$time) *10^-9)
      
        
      })
      
      if(input$callMethodPre == "Sequential"){
        #future(saveFunctionData(preProcessSequential_peakRAM, "results/preProcess/sequential"))
        sequential <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/sequential/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = sequential, x = sequential$Elapsed_Time_sec , y = sequential$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/sequential/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/sequential/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodPre == "Cluster"){
       # future(saveFunctionData(preProcessCluster_peakRAM,"results/preProcess/cluster"))
        sequential <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/cluster/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = sequential, x = sequential$Elapsed_Time_sec , y = sequential$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/cluster/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/cluster/resources.rds")
        print("Read")
        print(resources)
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodPre == "DoParallelChunked"){
        #future(saveFunctionData(preProcessDoparallelChunked_peakRAM, "results/preProcess/doparallelChunked"))
        sequential <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/doparallelChunked/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = sequential, x = sequential$Elapsed_Time_sec , y = sequential$Process_id , type = 'bar' ,
                  mode = 'markers',height = 480 ) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/doparallelChunked/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/doparallelChunked/resources.rds")
        print("Read")
        print(resources)
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodPre == "ParallelChunked"){
        #future(saveFunctionData(preProcessParallelChunked_peakRAM, "results/preProcess/parallelChunked"))
        sequential <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/parallelChunked/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = sequential, x = sequential$Elapsed_Time_sec , y = sequential$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/parallelChunked/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/parallelChunked/resources.rds")
        print("Read")
        print(resources)
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodPre == "ClusterChunked"){
        #future(saveFunctionData(preProcessClusterChunked_peakRAM, "results/preProcess/clusterChunked"))
        sequential <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/clusterChunked/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = sequential, x = sequential$Elapsed_Time_sec , y = sequential$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/clusterChunked/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/clusterChunked/resources.rds")
        print("Read")
        print(resources)
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
    }
    else if(input$callFunction == "DTM"){
      output$benchMarkSummary <- renderPlotly({
        benchmarkReadSmall <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/microbenchmark.rds")
        plot_ly(data = benchmarkReadSmall, x = benchmarkReadSmall$expr, y = as.numeric(benchmarkReadSmall$time) *10^-9) 
        
      })
      if(input$callMethodDTM == "createDfmChunks"){
        future(saveFunctionData(createDfmChunks_peakRAM, "results/createDTM/dfmChunks"))
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmChunks/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmChunks/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmChunks/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodDTM == "createDFM"){
        future(saveFunctionData(createDfmChunks_peakRAM, "results/createDTM/dfm"))
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfm/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfm/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfm/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodDTM == "createDFMasDTM"){
        future(saveFunctionData(createDFMasDTM_peakRAM, "results/createDTM/dfmASdtm"))
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmASdtm/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmASdtm/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmASdtm/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
      }
    }
    else if(input$callFunction == "Corpus"){
      
      output$benchMarkSummary <- renderPlotly({
        benchmarkReadSmall <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/microbenchmark.rds")
        plot_ly(data = benchmarkReadSmall, x = benchmarkReadSmall$expr, y = as.numeric(benchmarkReadSmall$time) *10^-9)
      
      })
      
      if(input$callMethodCorpus == "VCorpChunk"){
         #saveFunctionData(VCorp_peakRAM, "results/createCorpus/Vcorp")
         result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Vcorp/peakRAM.rds")
         output$RAMoutputFunctions <- renderPlotly({
           plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                   mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
         })
         
         output$CPUusage <- renderImage({
           return(list(
             src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Vcorp/snow_plot.png",
             contentType = "image/png",
             alt = "Face"
           ))
         },deleteFile = FALSE)
         
         resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Vcorp/resources.rds")
         
         
         output$RamUsagePlot <- renderPlotly({
           invalidateLater(2000, session)
           plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                   mode = "lines")
         })
         
         output$CPUusagePlot <- renderPlotly({
           plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                   mode = "lines")
           
         })
       }
      else if(input$callMethodCorpus =="Cluster"){
        #saveFunctionData(createCorpusCluster_peakRAM, "results/createCorpus/Cluster")
         result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Cluster/peakRAM.rds")
         print(result)
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Cluster/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Cluster/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
      }
      else if(input$callMethodCorpus =="Quan"){
        saveFunctionData(Quan_peakRAM, "results/createCorpus/Quan")
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Quan/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Quan/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Quan/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
      }
        
    }
})
  
  #==========================#
  #CoreInfoEachFunction   
  #==========================#
  observeEvent(input$showResultCors, {
    
  if(input$callFunction2 == "Read"){
    print(input$callFunction2)
    
    print("CHECKREAD")
    if(input$callMethodReadFiles == "sequential"){
      
      future(saveFunctionData(read_sequential_peakRAM, "results/readFiles/sequential"))
      sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/sequential/peakRAM.rds")
      output$RAMoutputFunctions <- renderPlotly({
        plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
        
        
      })
      output$CPUusage <- renderImage({
        return(list(
          src = "~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/sequential/snow_plot.png",
          contentType = "image/png",
          alt = "Face"
        ))
      },deleteFile = FALSE)
      
      resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/sequential/resources.rds")
      
      
      
      output$RamUsagePlot <- renderPlotly({
        plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                mode = "lines")
      })
      
      output$CPUusagePlot <- renderPlotly({
        plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                mode = "lines")
        
      })
    }
    else if(input$callMethodReadFiles == "clusterapply"){
      #future(read_clusterapply_peakRAM())
      clusterApplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/peakRAM.rds")
      output$RAMoutputFunctions <- renderPlotly({
        plot_ly(data = clusterApplyData, x = clusterApplyData$Elapsed_Time_sec , y = clusterApplyData$Process_id, type = 'bar' ,
                mode = 'markers' ) %>% layout(xaxis = Elapsed, yaxis = Process)
      })
      output$CPUusage <- renderImage({
        return(list(
          src = "~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/snow_plot.png",
          contentType = "image/png",
          alt = "Face"
        ))
      },deleteFile = FALSE)
      
      resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/resources.rds")
      
      
      output$RamUsagePlot <- renderPlotly({
        invalidateLater(2000, session)
        plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                mode = "lines")
      })
      
      output$CPUusagePlot <- renderPlotly({
        plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                mode = "lines")
        
      })
      
    }
    else if(input$callMethodReadFiles == "parlapply"){
      future(read_parlapply_peakRAM())
      parlapplyData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/peakRAM.rds")
      output$RAMoutputFunctions <- renderPlotly({
        plot_ly(data = parlapplyData, x = parlapplyData$Elapsed_Time_sec , y = parlapplyData$Process_id, type = 'bar' ,
                mode = 'markers' ) %>% layout(xaxis = Elapsed, yaxis = Process)
      })
      output$CPUusage <- renderImage({
        return(list(
          src = "~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/snow_plot.png",
          contentType = "image/png",
          alt = "Face"
        ))
      },deleteFile = FALSE)
      
      resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/resources.rds")
      
      
      output$RamUsagePlot <- renderPlotly({
        invalidateLater(2000, session)
        plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                mode = "lines")
      })
      
      output$CPUusagePlot <- renderPlotly({
        plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                mode = "lines")
        
      })
    }
    else if(input$callMethodReadFiles == "foreach"){
      
      future(read_doparallel_foreach_peakRAM())
      #future(saveFunctionData(read_doparallel_foreach_peakRAM,"results/read/foreach"))
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
  else if(input$callFunction2 == "Pre"){
      
      
      if(input$callMethodPre == "Sequential"){
        #future(saveFunctionData(preProcessSequential_peakRAM, "results/preProcess/sequential"))
        sequential <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/sequential/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = sequential, x = sequential$Elapsed_Time_sec , y = sequential$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/sequential/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/sequential/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodPre == "Cluster"){
        # future(saveFunctionData(preProcessCluster_peakRAM,"results/preProcess/cluster"))
        sequential <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/cluster/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = sequential, x = sequential$Elapsed_Time_sec , y = sequential$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/cluster/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/cluster/resources.rds")
        print("Read")
        print(resources)
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodPre == "DoParallelChunked"){
        #future(saveFunctionData(preProcessDoparallelChunked_peakRAM, "results/preProcess/doparallelChunked"))
        sequential <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/doparallelChunked/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = sequential, x = sequential$Elapsed_Time_sec , y = sequential$Process_id , type = 'bar' ,
                  mode = 'markers',height = 480 ) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/doparallelChunked/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/doparallelChunked/resources.rds")
        print("Read")
        print(resources)
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodPre == "ParallelChunked"){
        #future(saveFunctionData(preProcessParallelChunked_peakRAM, "results/preProcess/parallelChunked"))
        sequential <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/parallelChunked/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = sequential, x = sequential$Elapsed_Time_sec , y = sequential$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/parallelChunked/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        
        
      }
      else if(input$callMethodPre == "ClusterChunked"){
        #future(saveFunctionData(preProcessClusterChunked_peakRAM, "results/preProcess/clusterChunked"))
        sequential <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/clusterChunked/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = sequential, x = sequential$Elapsed_Time_sec , y = sequential$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/clusterChunked/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/clusterChunked/resources.rds")
        print("Read")
        print(resources)
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
    }
  else if(input$callFunction2 == "DTM"){
      
      if(input$callMethodDTM == "createDfmChunks"){
        future(saveFunctionData(createDfmChunks_peakRAM, "results/createDTM/dfmChunks"))
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmChunks/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmChunks/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmChunks/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodDTM == "createDFM"){
        future(saveFunctionData(createDfmChunks_peakRAM, "results/createDTM/dfm"))
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfm/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfm/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfm/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodDTM == "createDFMasDTM"){
        future(saveFunctionData(createDFMasDTM_peakRAM, "results/createDTM/dfmASdtm"))
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmASdtm/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmASdtm/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmASdtm/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
      }
    }
  else if(input$callFunction2 == "Corpus"){
      
      if(input$callMethodCorpus == "VCorpChunk"){

        #saveFunctionData(VCorp_peakRAM, "results/createCorpus/Vcorp")
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Vcorp/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Vcorp/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
      }
      else if(input$callMethodCorpus =="Cluster"){
        #saveFunctionData(createCorpusCluster_peakRAM, "results/createCorpus/Cluster")
        print("CHECKCLUSTER")
        
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Cluster/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Cluster/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
       
      }
      else if(input$callMethodCorpus =="Quan"){
        saveFunctionData(Quan_peakRAM, "results/createCorpus/Quan")
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Quan/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Quan/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        
      }
      
    }
  })
  
  #==========================#
  #CoreInfoEachFunction   
  #==========================#
  
  observeEvent(input$showResultResources,{
    if(input$callFunction3 == "Read"){
      print(input$callFunction2)
      
      print("CHECKREAD")
      if(input$callMethodReadFiles == "sequential"){
        
      
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/sequential/resources.rds")
        
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
      }
      else if(input$callMethodReadFiles == "clusterapply"){
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodReadFiles == "parlapply"){
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
      }
      else if(input$callMethodReadFiles == "foreach"){
        
        future(read_doparallel_foreach_peakRAM())
        #future(saveFunctionData(read_doparallel_foreach_peakRAM,"results/read/foreach"))
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
    else if(input$callFunction3 == "Pre"){
      
      
      if(input$callMethodPre == "Sequential"){
       
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/sequential/resources.rds")
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodPre == "Cluster"){
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/cluster/resources.rds")
        print("Read")
        print(resources)
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodPre == "DoParallelChunked"){
        
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/doparallelChunked/resources.rds")
        print("Read")
        print(resources)
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodPre == "ParallelChunked"){
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/parallelChunked/resources.rds")
        print("Read")
        print(resources)
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodPre == "ClusterChunked"){
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/clusterChunked/resources.rds")
        print("Read")
        print(resources)
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
    }
    else if(input$callFunction3 == "DTM"){
      
      if(input$callMethodDTM == "createDfmChunks"){
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmChunks/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodDTM == "createDFM"){
        
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfm/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodDTM == "createDFMasDTM"){
        
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmASdtm/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
      }
    }
    else if(input$callFunction3 == "Corpus"){
      
      if(input$callMethodCorpus == "VCorpChunk"){
        
        #saveFunctionData(VCorp_peakRAM, "results/createCorpus/Vcorp")
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Vcorp/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Vcorp/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Vcorp/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
      }
      else if(input$callMethodCorpus =="Cluster"){
        #saveFunctionData(createCorpusCluster_peakRAM, "results/createCorpus/Cluster")
        print("CHECKCLUSTER")
        
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Cluster/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Cluster/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Cluster/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
      }
      else if(input$callMethodCorpus =="Quan"){
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/Quan/resources.rds")
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
      }
      
    }
    
  })

  
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
  #Code voor de REAL time RAM usage & CPU usage 
  #====================================#
  
  #onStop(end_monitor)
  
  
  
  get_new_data <- function(){
    data <-c(time = format(Sys.time(), format = "%H:%M:%S")  , ram = as.numeric(system("../scripts/my_ram_usage.sh", intern = TRUE))/1024/1024, cpu = as.numeric(system("../scripts/my_cpu_usage.sh", intern = TRUE))) %>% rbind %>% data.frame
    return(data)
  }
  
  
  update_data <- function(){
    my_data <<- rbind(get_new_data(), my_data)
  }
  
  
  
  output$first_column <- renderPlotly({
    invalidateLater(1000, session)
    #update_data()
    plot_ly(data = my_data, x = as.POSIXct(my_data$time, origin ="1970-01-01"), y = my_data$ram,  type = "scatter",
            mode = "lines", height = 300, width = 1100) %>% layout(xaxis = Elapsed, yaxis = RAM)
  })
  
  output$second_column <- renderPlotly({
    invalidateLater(1000, session)
    #update_data()
    plot_ly(data = my_data, x = as.POSIXct(my_data$time, origin ="1970-01-01"), y = my_data$cpu,  type = "scatter",
            mode = "lines", height = 300, width = 1100) %>% layout(xaxis = Elapsed, yaxis = RAM2)
  })
})

shinyApp(ui = ui, server = server)







