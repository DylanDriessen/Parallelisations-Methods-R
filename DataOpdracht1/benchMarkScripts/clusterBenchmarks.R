#NOTES + TODO ----
#CTRL+click op functies in comments werkt ook
#TODO In benchmarkOverNoLinesSetup() filePaths aanpassen naar de corresponderende DFM.rds files
#TODO In clusterOverKBenchmark() filepath aanpassen. Wordt misschien best op een kleine dataset uitgevoerd. 
#TODO In runClusterOverK() KAN nstarts en maxiter nog aangepast worden om sneller te laten runnen.


runScriptAfterSource <- TRUE

#Run cluster Benchmarks----

runClusterBenchmarks <- function(){
  source("lib/cluster.R")
  source("loadPackages.R")
  cat("\014")  
  
  no_cores <<- detectCores()
  
  #SKMEANS Sequential====
  
  # print("skmeansCluster")
  # saveRDS(clusterOverNoLinesBenchmark(skmeansCluster),file = "skmeansClusterBenchmarkOverNoLinesResult.rds")
  # saveRDS(clusterOverKBenchmark(skmeansCluster),file = "skmeansClusterBenchmarkOverKResult.rds")
  
  #SKMEANS Parallel ====

  # print("skmeansClusterPar")
  # saveRDS(clusterOverNoLinesBenchmark(skmeansClusterPar),file = "skmeansClusterParBenchmarkOverNoLinesResult.rds")
  # saveRDS(clusterOverKBenchmark(skmeansClusterPar),file = "skmeansClusterParBenchmarkOverKResult.rds")

  #SKMEANS doParallel====

  # print("skmeansClusterDoPar")
  # saveRDS(clusterOverNoLinesBenchmark(skmeansClusterDoPar),file = "skmeansClusterDoParBenchmarkOverNoLinesResult.rds")
  # saveRDS(clusterOverKBenchmark(skmeansClusterDoPar),file = "skmeansClusterDoParBenchmarkOverKResult.rds")

  #SKMEANSclusterApply Iteration ====

  print("skmeansClusterParIter")
  saveRDS(clusterOverNoLinesBenchmark(skmeansClusterParIter),file = "skmeansClusterParIterBenchmarkOverNoLinesResult.rds")
  saveRDS(clusterOverKBenchmark(skmeansClusterParIter),file = "skmeansClusterParIterBenchmarkOverKResult.rds")

  #SKMEANS doParallel Iterations ====


  print("skmeansClusterDoParIter")
  saveRDS(clusterOverNoLinesBenchmark(skmeansClusterDoParIter),file = "skmeansClusterDoParIterBenchmarkOverNoLinesResult.rds")
  saveRDS(clusterOverKBenchmark(skmeansClusterDoParIter),file = "skmeansClusterDoParIterBenchmarkOverKResult.rds")
}


#Cluster over number of lines Benchmark ----

clusterOverNoLinesBenchmark <- function(f){
  counter <<- 0
  
  print("Benchmark cluster over")
  
  res <- microbenchmark(runClusterOverNoLines(f,no_lines = "4.800.000"),
                        runClusterOverNoLines(f,no_lines = "3.200.000"),
                        runClusterOverNoLines(f,no_lines = "1.500.000"),
                        runClusterOverNoLines(f,no_lines = "500.000"),
                        runClusterOverNoLines(f,no_lines = "50.000"),
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
    print("50000 lines")
    DFM <<- readRDS(file="data/DFM_50k.rds")
  }else if(counter == 2){
    print("500000 lines")
    DFM <<- readRDS(file="data/DFM_500k.rds")
  }
  else if(counter == 3){
    print("1500000 lines")
    DFM <<- readRDS(file="data/DFM_1.5m.rds")
  }
  else if(counter == 4){
    print("3200000 lines")
    DFM <<- readRDS(file="data/DFM_3.2m.rds")
  }
  # else if(counter == 4){
  #   print("4800000 lines")
  #   DFM <<- readRDS(file="data/DFM_4.8m.rds")
  # }
}

#Cluster over k's Benchmark ----

clusterOverKBenchmark <- function(f){
  counter <<- 0
  DFM <<- readRDS(file="data/DFM_50k.rds")
  
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

#Check if script must run on source ----

if(runScriptAfterSource){
  runClusterBenchmarks()  
}
