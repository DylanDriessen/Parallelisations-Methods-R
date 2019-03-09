#NOTES + TODO ----
#CTRL+click op functies in comments werkt ook
#TODO filepaths in benchmarkOverNoLinesSetup() moeten aangepast worden
#TODO filepath in clusterOverKBenchmark() moet aangepast worden. Wordt misschien best op een kleine dataset uitgevoerd. maxiter en nruns kan ook nog aangepast worden

runScriptAfterSource <- TRUE

#Run cluster Benchmarks----

runClusterBenchmarks <- function(){
  source("lib/cluster.R")
  source("loadPackages.R")
  cat("\014")  
  
  no_cores <<- detectCores()
  
  #SKMEANS Sequential====
  
  print("skmeansCluster")
  saveRDS(object = clusterOverNoLinesBenchmark(skmeansCluster),file = "skmeansClusterBenchmarkOverNoLinesResult.rds")
  saveRDS(object = clusterOverKBenchmark(skmeansCluster),file = "skmeansClusterBenchmarkOverKResult.rds")
  
  #SKMEANS Parallel ====

  print("skmeansClusterPar")
  saveRDS(object = clusterOverNoLinesBenchmark(skmeansClusterPar),file = "skmeansClusterParBenchmarkOverNoLinesResult.rds")
  saveRDS(object = clusterOverKBenchmark(skmeansClusterPar),file = "skmeansClusterParBenchmarkOverKResult.rds")

  #SKMEANS doParallel====

  print("skmeansClusterDoPar")
  saveRDS(object = clusterOverNoLinesBenchmark(skmeansClusterDoPar),file = "skmeansClusterDoParBenchmarkOverNoLinesResult.rds")
  saveRDS(object = clusterOverKBenchmark(skmeansClusterDoPar),file = "skmeansClusterDoParBenchmarkOverKResult.rds")

  #SKMEANSclusterApply Iteration ====

  print("skmeansClusterParIter")
  saveRDS(object = clusterOverNoLinesBenchmark(skmeansClusterParIter),file = "skmeansClusterParIterBenchmarkOverNoLinesResult.rds")
  saveRDS(object = clusterOverKBenchmark(skmeansClusterParIter),file = "skmeansClusterParIterBenchmarkOverKResult.rds")

  #SKMEANS doParallel Iterations ====


  print("skmeansClusterDoParIter")
  saveRDS(object = clusterOverNoLinesBenchmark(skmeansClusterDoParIter),file = "skmeansClusterDoParIterBenchmarkOverNoLinesResult.rds")
  saveRDS(object = clusterOverKBenchmark(skmeansClusterDoParIter),file = "skmeansClusterDoParIterBenchmarkOverKResult.rds")
}


#Cluster over number of lines Benchmark ----

clusterOverNoLinesBenchmark <- function(f){
  counter <<- 0
  
  print("Benchmark cluster over")
  
  res <- microbenchmark(runClusterOverNoLines(f,no_lines = "8.000.000"),
                        runClusterOverNoLines(f,no_lines = "6.400.000"),
                        runClusterOverNoLines(f,no_lines = "4.800.000"),
                        runClusterOverNoLines(f,no_lines = "3.200.000"),
                        runClusterOverNoLines(f,no_lines = "1.500.000"),
                        runClusterOverNoLines(f,no_lines = "500.000"),
                        setup = benchmarkOverNoLinesSetup(), #Runs without getting benchmarked 
                        control = list(order="inorder"),
                        times = 1)
  
  return(res)
}

runClusterOverNoLines <- function(f,no_lines){
  f(k=10,nstarts=10,maxiter=10)
}

benchmarkOverNoLinesSetup <- function(){
  
  counter <<- counter + 1
  if(counter == 1){
    print("8000000 lines")
    DFM <<- readRDS(file="data/DFM.rds")
  }else if(counter == 2){
    print("6400000 lines")
    DFM <<- readRDS(file="data/DFM.rds")
  }else if(counter == 3){
    print("4800000 lines")
    DFM <<- readRDS(file="data/DFM.rds")
  }else if(counter == 4){
    print("3200000 lines")
    DFM <<- readRDS(file="data/DFM.rds")
  }else if(counter == 5){
    print("1500000 lines")
    DFM <<- readRDS(file="data/DFM.rds")
  }else if(counter == 6){
    print("500000 lines")
    DFM <<- readRDS(file="data/DFM.rds")
  }
}

#Cluster over k's Benchmark ----

clusterOverKBenchmark <- function(f){
  counter <<- 0
  DFM <<- readRDS(file="data/DFM.rds")
  
  print("Benchmark cluster over")
  
  res <- microbenchmark(runClusterOverK(f,k = 25),
                        runClusterOverK(f,k = 50),
                        runClusterOverK(f,k = 100),
                        control = list(order="inorder"),
                        times = 1)
  return(res)
}

runClusterOverK <- function(f,k){
  print(paste0("k=",k))
  f(k=k,nstarts=10,maxiter=10)
}

if(runScriptAfterSource){
  runClusterBenchmarks()  
}
