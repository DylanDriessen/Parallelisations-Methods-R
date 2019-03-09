runScriptAfterSource <- TRUE

#Run preProcess Benchmarks ----

runPreProcessBenchmarks <- function(){
  source("lib/preProcess.r")
  source("loadPackages.R")
  cat("\014")  
  
  no_cores <<- detectCores()
  docs <<- readRDS(file="data/docs.rds")
  
  #PreprocessSequential ====
  
  print("PreprocessSequential")
  saveRDS(object = preProcessBenchmark(preProcessSequential),file = "preprocessSequentialChunkedBenchmarkOverNoLinesResult.rds")
  
  
  #PreprocessClusterChunkedBenchmarks ====
  
  print("preProcessClusterChunked")
  saveRDS(object = preProcessBenchmark(preProcessClusterChunked),file = "preProcessClusterChunkedBenchmarkOverNoLinesResult.rds")
  
  #PreprocessParallelChunkedBenchmarks ====
  
  print("preProcessParallelChunked")
  saveRDS(object = preProcessBenchmark(preProcessParallelChunked),file = "preProcessParallelChunkedBenchmarkOverNoLinesResult.rds")
  
  #PreprocessDoParallelChunkedBenchmarks ====
  
  print("preProcessDoParallelChunked")
  saveRDS(object = preProcessBenchmark(preProcessDoparallelChunked),file = "preProcessDoparallelChunkedBenchmarkOverNoLinesResult.rds")
}

#PreProcess Benchmark over number of lines ----
preProcessBenchmark <- function(f){
  counter <<- 0
  
  res <- microbenchmark(runPreProcess(f,no_lines = "8.000.000"),
                        runPreProcess(f,no_lines = "6.400.000"),
                        runPreProcess(f,no_lines = "4.800.000"),
                        runPreProcess(f,no_lines = "3.200.000"),
                        runPreProcess(f,no_lines = "1.500.000"),
                        runPreProcess(f,no_lines = "500.000"),
                        setup = benchmarkSetup(), #Runs without getting benchmarked 
                        control = list(order="inorder"),
                        times = 1)
  
  return(res)
}

runPreProcess <- function(f,no_lines){
  f()
}

benchmarkSetup <- function(){
  counter <<- counter + 1
  if(counter == 1){
    print("8000000 lines")
    docs <<- head(docs,n=8000000)
  }else if(counter == 2){
    print( "6400000 lines")
    docs <<- head(docs,n= 6400000)
  }else if(counter == 3){
    print("4800000 lines")
    docs <<- head(docs,n= 4800000)
  }else if(counter == 4){
    print("3200000 lines")
    docs <<- head(docs,n= 3200000)
  }else if(counter == 5){
    print("1500000 lines")
    docs <<- head(docs,n= 1500000)
  }else if(counter == 6){
    print("500000 lines")
    docs <<- head(docs,n= 500000)
  }
}

if(runScriptAfterSource){
  runPreProcessBenchmarks()  
}
