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
  includeCSS("RShinyDashboardAfstudeer/www/custom.css"),
  
  titlePanel(img(src='logo.png', align = "right", width = "350", height = "80")),

  navbarPage("Choose your tab",
               tabPanel("Overzicht",
                        sidebarPanel(
                          helpText("Select which function"),
                          selectInput(inputId = "callFunction", label = "Choose a function to display:",
                                      choices = c( ReadFile = "Read", PreProcess = "Pre", createCorpus = "Corpus", createDTM = "DTM", Cluster = "Cluster")
                                      ),
                          actionButton("showResult", "showResult"),
                          width = 2),
                         mainPanel(
                        fluidRow(plotlyOutput("benchMarkSummary"), align = "center"))
                   ),
               tabPanel("CoresInfo",
                        sidebarPanel(
                          helpText("Select wich function"),
                          selectInput(inputId = "callFunctionCors", label = "Choose a function to display:",
                                      choices = c( ReadFile = "Read", PreProcess = "Pre", createCorpus = "Corpus", createDTM = "DTM", Cluster = "Cluster")
                                      ),
                          
                          ###
                          conditionalPanel(
                            condition = "input.callFunctioCors == 'Read'",
                            selectInput(inputId = "callMethodReadFilesCORS", label = "Choose method to display",
                                        choices = c("sequentialCors", "clusterapplyCors", "parlapplyCors", "foreachCors"))),
                          
                          conditionalPanel(
                            condition = "input.callFunctionCors == 'Pre'",
                            selectInput(inputId = "callMethodPreCORS", label = "Choose a method to display",
                                        choices = c("SequentialCors", "ClusterCors", "DoParallelChunkedCors", "ParallelChunkedCors", "ClusterChunkedCors")
                            )), 
                          
                          conditionalPanel(
                            condition = "input.callFunctionCors == 'Corpus'", 
                            selectInput(inputId= "callMethodCorpusCORS", label = "Choose a method to display",
                                        choices = c("ClusterCors", "VCorpCors", "QuanCors"))),
                          
                          
                          conditionalPanel(
                            condition = "input.callFunctionCors == 'DTM'", 
                            selectInput(inputId= "callMethodDTMCORS", label = "Choose a method to display",
                                        choices = c("createDfmChunksCors", "createDFMCors", "createDFMasDTMCors" ))),
                          
                           conditionalPanel(
                            condition = "input.callFunctionCors == 'Cluster'", 
                            selectInput(inputId= "callMethodClusterCORS", label = "Choose a method to display",
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
                          selectInput(inputId = "callFunctionRes", label = "Choose a function to display:",
                                      choices = c( ReadFile = "Read", PreProcess = "Pre", createCorpus = "Corpus", createDTM = "DTM", Cluster = "Cluster")),
                          
                          ####
                          conditionalPanel(
                            condition = "input.callFunctionRes == 'Read'",
                            selectInput(inputId = "callMethodReadFilesRES", label = "Choose method to display",
                                        choices = c("sequentialRes", "clusterapplyRes", "parlapplyRes", "foreachRes"))),
                          
                          conditionalPanel(
                            condition = "input.callFunctionRes == 'Pre'",
                            selectInput(inputId = "callMethodPreRES", label = "Choose a method to display",
                                        choices = c("SequentialRes", "ClusterRes", "DoParallelChunkedRes", "ParallelChunkedRes", "ClusterChunkedRes")
                                        
                            )), 
                          
                          conditionalPanel(
                            condition = "input.callFunctionRes == 'Corpus'", 
                            selectInput(inputId= "callMethodCorpusRES", label = "Choose a method to display",
                                        choices = c("ClusterRes", "VCorpRes", "QuanRes"))),
                          
                          
                          conditionalPanel(
                            condition = "input.callFunctionRes == 'DTM'", 
                            selectInput(inputId= "callMethodDTMRES", label = "Choose a method to display",
                                        choices = c("createDfmChunksRes", "createDFMRes", "createDFMasDTMRes" ))),
                          
                          
                          
                          conditionalPanel(
                            condition = "input.callFunctionRes == 'Cluster'", 
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
                          selectInput(inputId = "callFunctionComp", label = "Choose a function to compare",
                                      choices = c( ReadFile = "Read", PreProcess = "Pre", createCorpus = "Corpus", createDTM = "DTM", Cluster = "Cluster")),
                          #===================#
                          conditionalPanel(
                            condition = "input.callFunctionComp == 'Read'",
                            selectInput(inputId = "callMethodReadFilesFIRST", label = "Choose method to compare",
                                        choices = c("sequentialFirst", "clusterapplyFirst", "parlapplyFirst", "foreachFirst"))),
                          #CompareCondition
                          conditionalPanel(
                            condition = "input.callFunctionComp == 'Read'",
                            selectInput(inputId = "callMethodReadFilesSECOND", label = "Choose method to compare",
                                        choices = c("sequentialSecond", "clusterapplySecond", "parlapplySecond", "foreachSecond")
                                        )),
                          #===================#
                          #===================#
                          conditionalPanel(
                            condition = "input.callFunctionComp == 'Pre'",
                            selectInput(inputId = "callMethodPreFIRST", label = "Choose a method to compare",
                                        choices = c("SequentialFirst", "ClusterFirst", "DoParallelChunkedFirst", "ParallelChunkedFirst", "ClusterChunkedFirst"))), 
                          #CompareCondition
                          conditionalPanel(
                            condition = "input.callFunctionComp == 'Pre'",
                            selectInput(inputId = "callMethodPreSECOND", label = "Choose a method to compare",
                                        choices = c("SequentialSecond", "ClusterSecond", "DoParallelChunkedSecond", "ParallelChunkedSecond", "ClusterChunkedSecond"))),
                          #===================#
                          #===================#
                          conditionalPanel(
                            condition = "input.callFunctionComp == 'Corpus'", 
                            selectInput(inputId= "callMethodCorpusFIRST", label = "Choose a method to compare",
                                        choices = c("ClusterFirst", "VCorpFirst", "QuanFirst"))),
                          #CompareCondition
                          
                          conditionalPanel(
                            condition = "input.callFunctionComp == 'Corpus'", 
                            selectInput(inputId= "callMethodCorpusSECOND", label = "Choose a method to compare",
                                        choices = c("ClusterSecond", "VCorpSecond", "QuanSecond"))),
                          #===================#
                          #===================#
                          
                          conditionalPanel(
                            condition = "input.callFunctionComp == 'DTM'", 
                            selectInput(inputId= "callMethodDTMFIRST", label = "Choose a method to compare",
                                        choices = c("createDfmChunksFirst", "createDFMFirst", "createDFMasDTMFirst" ))),
                          
                          conditionalPanel(
                            condition = "input.callFunctionComp == 'DTM'", 
                            selectInput(inputId= "callMethodDTMSECOND", label = "Choose a method to compare",
                                        choices = c("createDfmChunksSecond", "createDFMSecond", "createDFMasDTMSecond" ))),
                          #===================#
                          #===================#
                          conditionalPanel(
                            condition = "input.callFunctionComp == 'Cluster'", 
                            selectInput(inputId= "callMethodClusterFIRST", label = "Choose a method to compare",
                                        choices = c("methods to implement"))),
                          
                        
                        conditionalPanel(
                          condition = "input.callFunctionComp == 'Cluster'", 
                          selectInput(inputId= "callMethodClusterSECOND", label = "Choose a method to compare",
                                      choices = c("methods to implement"))),
                        
                        selectInput(inputId = "compare", label = "Choose what to compare",
                        choices = c("peakRAM", "CPUusage", "RAMusage", "plotCPUtime")),
                        
                        actionButton("showCompare", "showCompare"),
                        width = 2),
                       
                        
                        mainPanel(
                          
                            column(plotlyOutput("Compare"), width = 6),
                            column(plotlyOutput("CompareSecond"), width = 6)
                          , width = 10)
                        ),
                      
               
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
  
  #==========================#
  #OverzichInfoEachFunction   
  #==========================#
  
  observeEvent(input$showResult, {
    
    #==========================#
    #overzicht Structuur   
    #==========================#
    
    
    if(input$callFunction == "Read"){
      print(input$callFunction2)
      
      print("CHECKREADOVERZICHT")
      output$benchMarkSummary <- renderPlotly({
      benchmarkReadSmall <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/microbenchmark.rds")
      plot_ly(data = benchmarkReadSmall, x = benchmarkReadSmall$expr, y = as.numeric(benchmarkReadSmall$time) *10^-9) 
        
      })
    }
    else if(input$callFunction == "Pre"){
      print("CHECK")
      output$benchMarkSummary <- renderPlotly({
      benchmarkReadSmall <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/microbenchmark.rds")
      plot_ly(data = benchmarkReadSmall, x = benchmarkReadSmall$expr, y = as.numeric(benchmarkReadSmall$time) *10^-9)
      
        
      })
    }
    else if(input$callFunction == "DTM"){
        output$benchMarkSummary <- renderPlotly({
        benchmarkReadSmall <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/microbenchmark.rds")
        plot_ly(data = benchmarkReadSmall, x = benchmarkReadSmall$expr, y = as.numeric(benchmarkReadSmall$time) *10^-9) 
        
      })
    }
    else if(input$callFunction == "Corpus"){
      
      output$benchMarkSummary <- renderPlotly({
        benchmarkReadSmall <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/microbenchmark.rds")
        plot_ly(data = benchmarkReadSmall, x = benchmarkReadSmall$expr, y = as.numeric(benchmarkReadSmall$time) *10^-9)
      
      })
      

        
    }
})
  
  #==========================#
  #CoreInfoEachFunction   
  #==========================#
  observeEvent(input$showResultCors, {
    
  if(input$callFunctionCors == "Read"){
    print(input$callFunction2)
    
    print("CHECKREADCORE")
    if(input$callMethodReadFilesCORS == "sequentialCors"){
      
      #future(saveFunctionData(read_sequential_peakRAM, "results/readFiles/sequential"))
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
    }
    else if(input$callMethodReadFilesCORS == "clusterapplyCors"){
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
      
      
    }
    else if(input$callMethodReadFilesCORS == "parlapplyCors"){
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
      
      
    }
    else if(input$callMethodReadFilesCORS == "foreachCors"){
      
      #future(saveFunctionData(read_doparallel_foreach_peakRAM,"results/read/foreach"))
      foreachData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/foreach/peakRAM.rds")
      output$RAMoutputFunctions <- renderPlotly({
        plot_ly(data = foreachData, x = foreachData$Elapsed_Time_sec , y = foreachData$Process_id , type = 'bar' ,
                mode = 'markers' ) %>% layout(xaxis = Elapsed, yaxis = Process)
      })
      
      output$CPUusage <- renderImage({
        return(list(
          src = "~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/foreach/snow_plot.png",
          contentType = "image/png",
          alt = "Face"
        ))
      },deleteFile = FALSE)
      
    }
  }
    else if(input$callFunctionCors == "Pre"){
      
      
      if(input$callMethodPreCORS == "SequentialCors"){
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
      else if(input$callMethodPreCORS == "ClusterCors"){
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
      else if(input$callMethodPreCORS == "DoParallelChunkedCors"){
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
      else if(input$callMethodPreCORS == "ParallelChunkedCors"){
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
      else if(input$callMethodPreCORS == "ClusterChunkedCors"){
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
    else if(input$callFunctionCors == "DTM"){
      
      if(input$callMethodDTMCORS == "createDfmChunksCors"){
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
        },deleteFile = FALSE)}
      else if(input$callMethodDTMCORS == "createDFMCors"){
        #future(saveFunctionData(createDfmChunks_peakRAM, "results/createDTM/dfm"))
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
        
        
        
      }
      else if(input$callMethodDTMCORS == "createDFMasDTMCors"){
        #future(saveFunctionData(createDFMasDTM_peakRAM, "results/createDTM/dfmASdtm"))
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
       
      }
      
    }
    else if(input$callFunctionCors == "Corpus"){
      
      if(input$callMethodCorpusCORS == "VCorpChunkCors"){

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
      else if(input$callMethodCorpusCORS =="ClusterCors"){
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
      else if(input$callMethodCorpusCORS =="QuanCors"){
       # saveFunctionData(Quan_peakRAM, "results/createCorpus/Quan")
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
  #ResourceInfoEachFunction   
  #==========================#
  
  observeEvent(input$showResultResources,{
    if(input$callFunctionRes == "Read"){
      print("CHECKREADRESOURCE")
      if(input$callMethodReadFilesRES == "sequentialRes"){
        print("CHECKREADRESOURCE")
        
      
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
      else if(input$callMethodReadFilesRES == "clusterapplyRes"){
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/resources.rds")
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
        
      }
      else if(input$callMethodReadFilesRES == "parlapplyRes"){
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/resources.rds")
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines")
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")
          
        })
      }
      else if(input$callMethodReadFilesRES == "foreachRes"){
        
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/foreach/resources.rds")
        
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
      else if(input$callFunctionRes == "Pre"){
        if(input$callMethodPreRES == "SequentialRes"){
       
        
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
        else if(input$callMethodPreRES == "ClusterRes"){
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
        else if(input$callMethodPreRES == "DoParallelChunkedRes"){
        
        
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
        else if(input$callMethodPreRES == "ParallelChunkedRes"){
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
        else if(input$callMethodPreRES == "ClusterChunkedRes"){
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
      else if(input$callFunctionRes == "DTM"){
        if(input$callMethodDTMRES == "createDfmChunksRes"){
        
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
        else if(input$callMethodDTMRES == "createDFMRes"){
        
        
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
        else if(input$callMethodDTMRES == "createDFMasDTMRes"){
        
        
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
      else if(input$callFunctionRes == "Corpus"){
        if(input$callMethodCorpusRES == "VCorpChunkRes"){
        
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
        else if(input$callMethodCorpusRES =="ClusterRes"){
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
        else if(input$callMethodCorpusRES =="QuanRes"){
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
  #CompareInfoEachFunction   
  #==========================#
  
 
  #==================#
  #First Compare
  #==================#
  
  observeEvent(input$showCompare, {   
    if(input$callFunctionComp == "Read"){

      if(input$callMethodReadFilesFIRST == "sequentialFirst"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/sequential/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/sequential/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/sequential/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/sequential/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodReadFilesFIRST == "clusterapplyFirst"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodReadFilesFIRST == "parlapplyFirst"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodReadFilesFIRST == "foreachFirst"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/foreach/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/foreach/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/foreach/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/foreach/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      
    }
    else if(input$callFunctionComp == "Pre"){
      if(input$callMethodPreFIRST == "SequentialFirst"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/sequential/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/sequential/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/sequential/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/sequential/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodPreFIRST == "DoParallelChunkedFirst"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/doparallelChunked/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/doparallelChunked/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/doparallelChunked/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/doparallelChunked/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodPreFIRST == "ParallelChunkedFirst"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/parallelChunked/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/parallelChunked/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/parallelChunked/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/parallelChunked/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodPreFIRST == "ClusterChunkedFirst"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/clusterChunked/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/clusterChunked/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/clusterChunked/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/clusterChunked/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
    }
    
    })
  
  #==================#
  #Second Compare
  #==================#
  
  
  observeEvent(input$showCompare, {   
    if(input$callFunctionComp == "Read"){
      if(input$callMethodReadFilesSECOND == "sequentialSecond"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/sequential/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/sequential/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/sequential/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareSecondImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/sequential/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodReadFilesSECOND == "clusterapplySecond"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodReadFilesSECOND == "parlapplySecond"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodReadFilesSECOND == "foreachSecond"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/foreach/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/foreach/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/foreach/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/foreach/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
    }
    else if(input$callFunctionComp == "Pre"){
      if(input$callMethodPreSECOND == "SequentialSecond"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/sequential/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/sequential/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/sequential/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/sequential/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodPreSECOND == "DoParallelChunkedSecond"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/doparallelChunked/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/doparallelChunked/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/doparallelChunked/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/doparallelChunked/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodPreSECOND == "ParallelChunkedSecond"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/parallelChunked/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/parallelChunked/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/parallelChunked/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/parallelChunked/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodPreSECOND == "ClusterChunkedSecond"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/clusterChunked/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/clusterChunked/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/clusterChunked/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/clusterChunked/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
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








