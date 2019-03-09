########################################
#
# PreProcess Benchmark Code
#
########################################

source("lib/preProcess.r")
source("loadPackages.R")

runPreProcessBenchmark <- function(f){
  res <- microbenchmark(preProcess(f,no_lines = "8.000.000"),
                        preProcess(f,no_lines = "6.400.000"),
                        preProcess(f,no_lines = "4.800.000"),
                        preProcess(f,no_lines = "3.200.000"),
                        preProcess(f,no_lines = "1.500.000"),
                        preProcess(f,no_lines = "500.000"),
                        setup = benchmarkSetup(),
                        control = list(order="inorder"),
                        times = 1)
  
  return(res)
}

preProcess <- function(f,no_lines){
  f()
}

benchmarkSetup <- function(){
  counter <<- counter + 1
  print(counter)
  if(counter == 1){
    print("test 1, 8000000 lines")
    docs <<- head(docs,n=8000000)
  }else if(counter == 2){
    print("test 2, 6400000 lines")
    docs <<- head(docs,n= 6400000)
  }else if(counter == 3){
    print("test 3, 4800000 lines")
    docs <<- head(docs,n= 4800000)
  }else if(counter == 4){
    print("test 4, 3200000 lines")
    docs <<- head(docs,n= 3200000)
  }else if(counter == 5){
    print("test 5, 1500000 lines")
    docs <<- head(docs,n= 1500000)
  }else if(counter == 6){
    print("test 6, 500000 lines")
    docs <<- head(docs,n= 500000)
  }
}

no_cores <- detectCores()
docs <- readRDS(file="data/docs.rds")

########################################
#
# PreprocessClusterChunkedBenchmarks
#
########################################

counter <- 0
preProcessClusterChunkedBenchmarkOverNoLinesResult <- runPreProcessBenchmark(preProcessClusterChunked)
saveRDS(object = preProcessClusterChunkedBenchmarkOverNoLinesResult,file = "preProcessClusterChunkedBenchmarkOverNoLinesResult.rds")

########################################
#
# PreprocessParallelChunkedBenchmarks
#
########################################

counter <- 0
preProcessParallelChunkedBenchmarkOverNoLinesResult <- runPreProcessBenchmark(preProcessParallelChunked)
saveRDS(object = preProcessParallelChunkedBenchmarkOverNoLinesResult,file = "preProcessParallelChunkedBenchmarkOverNoLinesResult.rds")

########################################
#
# PreprocessDoParallelChunkedBenchmarks
#
########################################

counter <- 0
preProcessDoparallelChunkedBenchmarkOverNoLinesResult <- runPreProcessBenchmark(preProcessDoparallelChunked)
saveRDS(object = preProcessDoparallelChunkedBenchmarkOverNoLinesResult,file = "preProcessDoparallelChunkedBenchmarkOverNoLinesResult.rds")
