#500000
#1500000
#3200000
#4800000
#6400000
#8000000

source("lib/preProcess.r")
source("loadPackages.R")
no_cores <- detectCores()
counter <- 0
docs <- readRDS(file="data/docs.rds")

runPreProcessBenchmark <- function(){
  res <- microbenchmark(preProcess(no_lines = "8.000.000"),
                        preProcess(no_lines = "6.400.000"),
                        preProcess(no_lines = "4.800.000"),
                        preProcess(no_lines = "3.200.000"),
                        preProcess(no_lines = "1.500.000"),
                        preProcess(no_lines = "500.000"),
                        setup = benchmarkSetup(),
                        control = list(order="inorder"),
                        times = 1)
  
  return(res)
}

preProcess <- function(no_lines){
  preProcessClusterChunked()
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

preProcessBenchmarkOverNLinesResult <- runPreProcessBenchmark()
preProcessBenchmarkOverNLinesResult
saveRDS(object = preProcessBenchmarkOverNLinesResult,file = "preProcesBenchmarkOverNLinesResult.rds")




