setwd("../")
plan(multiprocess)

# load all packages and source files
source("startupDashboard.r")

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
                            condition = "input.callFunctionCors == 'Read'",
                            selectInput(inputId = "callMethodReadFilesCORS", label = "Choose method to display",
                                        choices = c("sequentialCors", "clusterapplyCors", "parlapplyCors", "foreachCors"))),
                          
                          conditionalPanel(
                            condition = "input.callFunctionCors == 'Pre'",
                            selectInput(inputId = "callMethodPreCORS", label = "Choose a method to display",
                                        choices = c("SequentialCors", "DoParallelChunkedCors", "ParallelChunkedCors", "ClusterChunkedCors")
                            )), 
                          
                          conditionalPanel(
                            condition = "input.callFunctionCors == 'Corpus'", 
                            selectInput(inputId= "callMethodCorpusCORS", label = "Choose a method to display",
                                        choices = c("QuanRCors", "TMCorpusCors", "TMCorpusChunkCors", "TMForeachOneLoopCors"))),
                          
                          
                          conditionalPanel(
                            condition = "input.callFunctionCors == 'DTM'", 
                            selectInput(inputId= "callMethodDTMCORS", label = "Choose a method to display",
                                        choices = c("createDfmChunksCors", "createDFMCors", "createDFMasDTMCors" ))),
                          
                           conditionalPanel(
                            condition = "input.callFunctionCors == 'Cluster'", 
                            selectInput(inputId= "callMethodClusterCORS", label = "Choose a method to display",
                                        choices = c("doParallelCors", "doParIterCors", "parallelCors", "parIterCors", "sequentialCors"))), 
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
                                        choices = c("SequentialRes", "DoParallelChunkedRes", "ParallelChunkedRes", "ClusterChunkedRes")
                                        
                            )), 

                          conditionalPanel(
                            condition = "input.callFunctionRes == 'Corpus'", 
                            selectInput(inputId= "callMethodCorpusRES", label = "Choose a method to display",
                                        choices = c("QuanRRes", "TMCorpusRes", "TMCorpusChunkRes", "TMForeachOneLoopRes"))),
                          
                          
                          conditionalPanel(
                            condition = "input.callFunctionRes == 'DTM'", 
                            selectInput(inputId= "callMethodDTMRES", label = "Choose a method to display",
                                        choices = c("createDfmChunksRes", "createDFMRes", "createDFMasDTMRes" ))),
                          
                          
                          
                          conditionalPanel(
                            condition = "input.callFunctionRes == 'Cluster'", 
                            selectInput(inputId= "callMethodClusterRES", label = "Choose a method to display",
                                        choices = c("doParallelRes", "doParIterRes", "parallelRes", "parIterRes", "sequentialRes"))),
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
                                        choices = c("SequentialFirst", "DoParallelChunkedFirst", "ParallelChunkedFirst", "ClusterChunkedFirst"))), 
                          #CompareCondition
                          conditionalPanel(
                            condition = "input.callFunctionComp == 'Pre'",
                            selectInput(inputId = "callMethodPreSECOND", label = "Choose a method to compare",
                                        choices = c("SequentialSecond", "DoParallelChunkedSecond", "ParallelChunkedSecond", "ClusterChunkedSecond"))),
                          #===================#
                          #===================#
                          conditionalPanel(
                            condition = "input.callFunctionComp == 'Corpus'", 
                            selectInput(inputId= "callMethodCorpusFIRST", label = "Choose a method to compare",
                                        choices = c("QuanRFirst", "TMCorpusFirst", "TMCorpusChunkFirst", "TMForeachOneLoopFirst"))),
                          #CompareCondition
                          
                          conditionalPanel(
                            condition = "input.callFunctionComp == 'Corpus'", 
                            selectInput(inputId= "callMethodCorpusSECOND", label = "Choose a method to compare",
                                        choices = c("QuanRSecond", "TMCorpusSecond", "TMCorpusChunkSecond", "TMForeachOneLoopSecond"))),
                          #===================#
                          #===================#
                          
                          conditionalPanel(
                            condition = "input.callFunctionComp == 'DTM'", 
                            selectInput(inputId= "callMethodDTMFIRST", label = "Choose a method to compare",
                                        choices = c("createDfmChunksFirst", "createDFMFirst", "createDFMasDTMFirst", "DTMFirst", "DTMchunkedFirst" ))),
                          
                          conditionalPanel(
                            condition = "input.callFunctionComp == 'DTM'", 
                            selectInput(inputId= "callMethodDTMSECOND", label = "Choose a method to compare",
                                        choices = c("createDfmChunksSecond", "createDFMSecond", "createDFMasDTMSecond", "DTMSecond", "DTMchunkedSecond"))),
                          #===================#
                          #===================#
                          conditionalPanel(
                            condition = "input.callFunctionComp == 'Cluster'", 
                            selectInput(inputId= "callMethodClusterFIRST", label = "Choose a method to compare",
                                        choices = c("doParallelFirst", "doParIterFirst", "parallelFirst", "parIterFirst", "sequentialFirst"))),
                          
                        
                        conditionalPanel(
                          condition = "input.callFunctionComp == 'Cluster'", 
                          selectInput(inputId= "callMethodClusterSECOND", label = "Choose a method to compare",
                                      choices = c("doParallelSecond", "doParIterSecond", "parallelSecond", "parIterSecond", "sequentialSecond"))),
                        
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
                        sidebarPanel(
                          helpText("Select wich function"),
                          selectInput(inputId = "callFunctionLive", label = "Choose a function to display:",
                                      choices = c( ReadFile = "Read", PreProcess = "Pre", createCorpus = "Corpus", createDTM = "DTM", Cluster = "Cluster")),
                          
                          ####
                          conditionalPanel(
                            condition = "input.callFunctionLive == 'Read'",
                            selectInput(inputId = "callMethodReadFilesLIVE", label = "Choose method to display",
                                        choices = c("sequentialLive", "clusterapplyLive", "parlapplyLive", "foreachLive"))),
                          
                          conditionalPanel(
                            condition = "input.callFunctionLive == 'Pre'",
                            selectInput(inputId = "callMethodPreLIVE", label = "Choose a method to display",
                                        choices = c("SequentialLive", "DoParallelChunkedLive", "ParallelChunkedLive", "ClusterChunkedLive")
                                        
                            )), 
                          
                          conditionalPanel(
                            condition = "input.callFunctionLive == 'Corpus'", 
                            selectInput(inputId= "callMethodCorpusLIVE", label = "Choose a method to display",
                                        choices = c("QuanRLive", "TMCorpusLive", "TMCorpusChunkRes", "TMForeachOneLoopLive"))),
                          
                          
                          conditionalPanel(
                            condition = "input.callFunctionLive == 'DTM'", 
                            selectInput(inputId= "callMethodDTMLIVE", label = "Choose a method to display",
                                        choices = c("createDfmChunksLive", "createDFMLive", "createDFMasDTMLive",  "DTMLive", "DTMchunkedLive"))),
                          
                          
                          
                          conditionalPanel(
                            condition = "input.callFunctionLive == 'Cluster'", 
                            selectInput(inputId= "callMethodClusterLIVE", label = "Choose a method to display",
                                        choices = c("doParallelLive", "doParIterLive", "parallelLive", "parIterLive", "sequentialLive"))),
                          actionButton("RunLive", "RunLive"),
                          width = 2),
                       
                        
                        
                        mainPanel(
                        fluidRow(
                          plotlyOutput("first_column"),
                          plotlyOutput("second_column")
                        ), width=10)
               )
               
    )))
    




# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session){
  start_monitor()
  
  
  onSessionEnded(end_monitor)
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
      else if(input$callFunction == "Cluster"){
        
        output$benchMarkSummary <- renderPlotly({
          benchmarkReadSmall <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/microbenchmark.rds")
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
      

      if(input$callMethodCorpusCORS == "QuanRCors"){

        #saveFunctionData(VCorp_peakRAM, "results/createCorpus/Vcorp")
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/QuanR/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })

        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/QuanR/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)

      }
      

      else if(input$callMethodCorpusCORS =="TMCorpusCors"){
        print("CHECKCLUSTER")

        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpus/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })

        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpus/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)


      }

      else if(input$callMethodCorpusCORS =="TMCorpusChunkCors"){
       # saveFunctionData(Quan_peakRAM, "results/createCorpus/Quan")
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpusChunk/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })

        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpusChunk/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)


      }
      else if(input$callMethodCorpusCORS =="TMForeachOneLoopCors"){
        # saveFunctionData(Quan_peakRAM, "results/createCorpus/Quan")
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMForeachOneLoop/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMForeachOneLoop/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
        
      }
      
    }
    else if(input$callFunctionCors == "Cluster"){
      print("CHECK")
      
      if(input$callMethodClusterCORS == "doParallelCors"){
        
        print("CHECK")
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParallel/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParallel/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
      }
      else if(input$callMethodClusterCORS =="doParIterCors"){
        print("CHECK")
        
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParIter/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParIter/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
      }
      else if(input$callMethodClusterCORS =="parallelCors"){
        print("CHECK")
        
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parallel/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parallel/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
      }
      
      else if(input$callMethodClusterCORS =="parIterCors"){
        print("CHECK")
        
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parIter/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parIter/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
      }
      
      else if(input$callMethodClusterCORS =="sequentialCors"){
        
        result <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/sequential/peakRAM.rds")
        output$RAMoutputFunctions <- renderPlotly({
          plot_ly(data = result, x = result$Elapsed_Time_sec , y = result$Process_id , type = 'bar' ,
                  mode = 'markers' ,height = 480) %>% layout(xaxis = Elapsed, yaxis = Process)
        })
        
        output$CPUusage <- renderImage({
          return(list(
            src = "~/R/Afstudeerwerk/DataOpdracht1/results/cluster/sequential/snow_plot.png",
            contentType = "image/png",
            alt = "Face"
          ))
        },deleteFile = FALSE)
        
      }
      
        
      
      
    }
    
  }) #in orde
  
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
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines")%>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
      }
      else if(input$callMethodReadFilesRES == "clusterapplyRes"){
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/clusterapply/resources.rds")
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
        
      }
      else if(input$callMethodReadFilesRES == "parlapplyRes"){
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/parlapply/resources.rds")
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
      }
      else if(input$callMethodReadFilesRES == "foreachRes"){
        
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/readFiles/foreach/resources.rds")
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
    }
    }
      else if(input$callFunctionRes == "Pre"){
        if(input$callMethodPreRES == "SequentialRes"){
       
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/sequential/resources.rds")
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
        
      }
        else if(input$callMethodPreRES == "ClusterRes"){
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/cluster/resources.rds")
        print("Read")
        print(resources)
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
        
      }
        else if(input$callMethodPreRES == "DoParallelChunkedRes"){
        
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/doparallelChunked/resources.rds")
        print("Read")
        print(resources)
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
        
      }
        else if(input$callMethodPreRES == "ParallelChunkedRes"){
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/parallelChunked/resources.rds")
        print("Read")
        print(resources)
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
        
      }
        else if(input$callMethodPreRES == "ClusterChunkedRes"){
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/preProcess/clusterChunked/resources.rds")
        print("Read")
        print(resources)
        
        
        output$RamUsagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
        
      }
    }
      else if(input$callFunctionRes == "DTM"){
        if(input$callMethodDTMRES == "createDfmChunksRes"){
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmChunks/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
        
      }
        else if(input$callMethodDTMRES == "createDFMRes"){
        
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfm/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
        
      }
        else if(input$callMethodDTMRES == "createDFMasDTMRes"){
        
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmASdtm/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
      }
    }
      else if(input$callFunctionRes == "Corpus"){
        #choices = c("QuanRRes", "TMCorpusRes", "TMCorpusChunkRes", "TMForeachOneLoopRes"))),

        if(input$callMethodCorpusRES == "QuanRRes"){
        

        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/QuanR/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
      }
        else if(input$callMethodCorpusRES =="TMCorpusRes"){
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpus/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
      }
        else if(input$callMethodCorpusRES =="TMCorpusChunkRes"){
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpusChunk/resources.rds")
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
        }
        else if(input$callMethodCorpusRES =="TMForeachOneLoopRes"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMForeachOneLoop/resources.rds")
          
          output$RamUsagePlot <- renderPlotly({
            invalidateLater(2000, session)
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
          })
          
          output$CPUusagePlot <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
            
          })
        }
        
      }
    
      else if(input$callFunctionRes == "Cluster"){
      if(input$callMethodClusterRES == "doParallelRes"){
          print("CHECK")
        
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParallel/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
        })
        
      }
      else if(input$callMethodClusterRES == "doParIterRes"){
        

        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParIter/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
        
      }
      else if(input$callMethodClusterRES == "parallelRes"){
        

        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parallel/resources.rds")
        
        
        output$RamUsagePlot <- renderPlotly({
          invalidateLater(2000, session)
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
        })
        
        output$CPUusagePlot <- renderPlotly({
          plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                  mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
          
        })
      }
      else if(input$callMethodClusterRES == "parIterRes"){
          

          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parIter/resources.rds")
          
          
          output$RamUsagePlot <- renderPlotly({
            invalidateLater(2000, session)
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
          })
          
          output$CPUusagePlot <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
            
          })
        }
      else if(input$callMethodClusterRES == "sequentialRes"){
        resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/sequential/resources.rds")
          output$RamUsagePlot <- renderPlotly({
            invalidateLater(2000, session)
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM)
          })
          
          output$CPUusagePlot <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines") %>% layout(xaxis = Elapsed, yaxis = RAM2)
            
          })
        }
    }
      
    
  }) #in orde 

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
    else if(input$callFunctionComp == "Corpus"){
      #choices = c("QuanRFirst", "TMCorpusFirst", "TMCorpusChunkFirst", "TMForeachOneLoopFirst"))),

      if(input$callMethodCorpusFIRST == "QuanRFirst"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/QuanR/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/QuanR/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/QuanR/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/QuanR/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodCorpusFIRST == "TMCorpusFirst"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpus/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpus/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpus/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpus/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodCorpusFIRST == "TMCorpusChunkFirst"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpusChunk/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpusChunk/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpusChunk/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpusChunk/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodCorpusFIRST == "TMForeachOneLoopFirst"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMForeachOneLoop/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMForeachOneLoop/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMForeachOneLoop/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMForeachOneLoop/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
     }
    else if(input$callFunctionComp == "Cluster"){
      if(input$callMethodClusterFIRST == "doParallelFirst"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParallel/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParallel/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParallel/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParallel/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodClusterFIRST == "doParIterFirst"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParIter/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParIter/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParIter/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParIter/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodClusterFIRST == "parallelFirst"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parallel/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parallel/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parallel/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parallel/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodClusterFIRST == "parIterFirst"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parIter/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parIter/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parIter/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parIter/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodClusterFIRST == "sequentialFirst"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/sequential/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/sequential/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/sequential/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/cluster/sequential/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      
      
      
    }
    else if(input$callFunctionComp == "DTM"){
      if(input$callMethodDTMFIRST == "createDfmChunksFirst"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmChunks/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmChunks/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmChunks/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmChunks/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodDTMFIRST == "createDFMFirst"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfm/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfm/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfm/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfm/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodDTMFIRST == "createDFMasDTMFirst"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmASdtm/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmASdtm/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmASdtm/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmASdtm/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
        }
      else if(input$callMethodDTMFIRST == "DTMFirst"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/DTM/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/DTM/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/DTM/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/DTM/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodDTMFIRST == "DTMchunkedFirst"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/DTMchunked/peakRAM.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/DTMchunked/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/DTMchunked/resources.rds")
          output$Compare <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImage <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/DTMchunked/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }

      
      
    }


    }) # in orde
  
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
    else if(input$callFunctionComp == "Cluster"){
      if(input$callMethodClusterSECOND == "doParallelSecond"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParallel/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParallel/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParallel/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParallel/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodClusterSECOND == "doParIterSecond"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParIter/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParIter/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParIter/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/cluster/doParIter/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodClusterSECOND == "parallelSecond"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parallel/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parallel/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parallel/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parallel/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodClusterSECOND == "parIterSecond"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parIter/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parIter/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parIter/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/cluster/parIter/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodClusterSECOND == "sequentialSecond"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/sequential/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/sequential/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/cluster/sequential/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/cluster/sequential/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      
    }
    else if(input$callFunctionComp == "DTM"){
      if(input$callMethodDTMSECOND == "createDfmChunksSecond"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmChunks/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmChunks/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmChunks/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmChunks/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodDTMSECOND == "createDFMSecond"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfm/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfm/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfm/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfm/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodDTMSECOND == "createDFMasDTMSecond"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmASdtm/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmASdtm/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmASdtm/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/dfmASdtm//snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodDTMSECOND == "DTMSecond"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/DTM/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/DTM/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/DTM/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/DTM/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodDTMSECOND == "DTMchunkedSecond"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/DTMchunked/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/DTMchunked/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/DTMchunked/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createDTM/DTMchunked/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
    }
    else if(input$callFunctionComp == "Corpus"){
      #choices = c("QuanRFirst", "TMCorpusFirst", "TMCorpusChunkFirst", "TMForeachOneLoopFirst"))),
      
      if(input$callMethodCorpusSECOND == "QuanRSecond"){
        
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/QuanR/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/QuanR/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/QuanR/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/QuanR/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodCorpusSECOND == "TMCorpusSecond"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpus/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpus/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpus/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpus/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodCorpusSECOND == "TMCorpusChunkSecond"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpusChunk/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpusChunk/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpusChunk/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMCorpusChunk/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
      else if(input$callMethodCorpusSECOND == "TMForeachOneLoopSecond"){
        if(input$compare == "peakRAM"){
          sequentialData <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMForeachOneLoop/peakRAM.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = sequentialData, x = sequentialData$Elapsed_Time_sec , y = sequentialData$Process_id, 
                    type = 'bar', height = 480)%>% layout(xaxis = Elapsed, yaxis = Process)
            
            
          })
        }
        else if(input$compare == "CPUusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMForeachOneLoop/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$cpu, type = "scatter",
                    mode = "lines")
            
          })
        }
        else if(input$compare == "RAMusage"){
          resources <- readRDS("~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMForeachOneLoop/resources.rds")
          output$CompareSecond <- renderPlotly({
            plot_ly(data = resources, x = as.POSIXct(resources$time, origin ="1970-01-01"), y = resources$ram, type = "scatter",
                    mode = "lines")
          })
        }
        else if(input$compare == "plotCPUtime"){
          output$CompareImageSecond <- renderImage({
            return(list(
              src = "~/R/Afstudeerwerk/DataOpdracht1/results/createCorpus/TMForeachOneLoop/snow_plot.png",
              contentType = "image/png",
              alt = "Face"
            ))
          },deleteFile = FALSE)
        }
      }
    }
  }) # in orde 
  
  
  
  
  
  
  
  Elapsed <- list(
    title = "Elapsed Time (sec)"
  )
  RAM <- list(
    title = "RAMusage(GB)"
  )
  
  RAM2 <- list(
    title = "CPUusage(%)"
  )
  
  
  
  #====================================#
  #Code voor de REAL time RAM usage & CPU usage 
  #====================================#
  
  #onStop(end_monitor)
  
 
  
  #========#
  #Live functies Tonen
  #========#
  
  observeEvent(input$RunLive, {
    
    if(input$callFunctionLive == "Read"){
      if(input$callMethodReadFilesLIVE == "sequentialLive"){
        
        future(saveFunctionData(read_sequential_peakRAM, "test/test"))
      }
      else if(input$callMethodReadFilesLIVE == "clusterapplyLive"){
        readFiles_clusterapply()
      }
      else if(input$callMethodReadFilesLIVE == "parlapplyLive"){
        future(readFiles_parlapply())
      }
      else if(input$callMethodReadFilesLIVE == "foreachLive"){
        future(readFiles_doparallel_foreach())
      }
    }
    else if(input$callFunctionLive == "Pre"){
      print("checkLive")
      
      if(input$callMethodPreLIVE == "SequentialLive"){
        print("checkLive")
        future(preProcessSequential())
      }
      else if(input$callMethodPreLIVE == "DoParallelChunkedLive"){
        future(preProcessDoparallelChunked())
      }
      else if(input$callMethodPreLIVE == "ParallelChunkedLive"){
       future(preProcessParallelChunked())
     }
      else if(input$callMethodPreLIVE == "ClusterChunkedLive"){
        future(preProcessClusterChunked())
       }
   }
    else if(input$callFunctionLive == "Corpus"){
      if(input$callMethodCorpusLIVE == "QuanRLive"){
        future(QuantedaCorpus())
      }
      else if(input$callMethodCorpusLIVE == "TMCorpusLive"){
        future(TMCorpus())
      }
      
      else if(input$callMethodCorpusLIVE == "TMCorpusChunkRes"){
        future(TMCorpusChunk())
      }
      else if(input$callMethodCorpusLIVE == "TMForeachOneLoopLive"){
        future(TMCorpusChunk1Loop())
      }
      
    }
    else if(input$callFunctionLive == "DTM"){
      if(input$callMethodDTMLIVE == "createDfmChunksLive"){
          future(createDFMChunks())
      }
      else if(input$callMethodDTMLIVE == "createDFMLive"){
        future(createDFM())
      }
      else if(input$callMethodDTMLIVE == "createDFMasDTMLive"){
        future(createDFMasDTM())
      }
      else if(input$callMethodDTMLIVE == "DTMLive"){
        future(createDTM())
      }
      else if(nput$callMethodDTMLIVE == "DTMchunkedLive"){
       future(createDTMChunked()) 
      }
    }
    else if(input$callFunctionLive == "Cluster"){
      if(input$callMethodClusterLIVE == "doParallelLive"){
        future(skmeansClusterDoPar())
      }
      else if(input$callMethodClusterLIVE == "doParIterLive"){
        future(skmeansClusterDoParIter())
      }
      else if(input$callMethodClusterLIVE  == "parallelLive"){
        future(skmeansClusterPar())
      }
      else if(input$callMethodClusterLIVE  == "parIterLive"){
        future(skmeansClusterParIter())
      }
      else if(input$callMethodClusterLIVE  == "sequentialLive"){
        future(skmeansCluster())
      }
    }
   
  })
    
    
    
    
  
  
  
  
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








