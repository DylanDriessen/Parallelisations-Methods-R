source("lib/cluster.R")
no_clusters <- 10

list_to_df <- function(l){ return(as.data.frame(do.call(rbind, l))) }

# ==============================================================================
#
#                                 SKMEANS Sequential
#
# ==============================================================================

skmeansCluster_peakRAM <- function(){
  t <- Sys.time()
  df <- peakRAM(skmeansCluster(no_clusters))
  cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
}

# ==============================================================================
#
#                                 SKMEANS Parallel 
#
# ==============================================================================

skmeansClusterPar_peakRAM <- function() {
  #genetic
  set.seed(125)
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores, outfile = "")
  clusterExport(cl, "no_clusters")
  clusterEvalQ(cl, {library("quanteda");library("skmeans");library("peakRAM")})
  clusterSetRNGStream(cl, iseed = 1236)
  registerDoSNOW(cl)
  nstart <- 8
  nstartv <- rep(floor(nstart / no_cores), no_cores)
  result <-
    clusterApply(cl, nstartv, function(n, x) {
      t <- Sys.time()
      df <- peakRAM(skmeans(x, no_clusters, method = "pclust", control = list(nruns = n ,maxiter = 10,verbose = TRUE)))
      cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
    }, DFM)
  stopCluster(cl)
  
  return(list_to_df(result))
}

# ==============================================================================
#
#                                 SKMEANS doParallel 
#
# ==============================================================================

skmeansClusterDoPar_peakRAM <- function() {
  #genetic
  set.seed(125)
  cl <- makeCluster(no_cores, outfile = "")
  clusterExport(cl, "no_clusters")
  registerDoSNOW(cl)
  clusterSetRNGStream(cl, iseed = 1236)
  nstart <- 8
  nstartv <- rep(ceiling(nstart / no_cores), no_cores)
  registerDoParallel(cl)
  
  result <- 
    foreach(n=nstartv,
            .export= "DFM",
            .packages = c("skmeans","quanteda", "peakRAM"),
            .combine = rbind) %dopar% {
              t <- Sys.time()
              df <- peakRAM(skmeans(DFM, no_clusters ,method = "pclust",control = list(nruns = n ,maxiter = 10,verbose = TRUE)))
              cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
            }
  stopCluster(cl)
  return(result)
}

# ==============================================================================
#
#                         SKMEANS  clusterApply Iterations
#
# ==============================================================================

skmeansClusterParIter <- function() {
  #genetic
  set.seed(125)
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores, outfile = "")
  clusterExport(cl, "no_clusters")
  clusterEvalQ(cl, {library("quanteda");library("skmeans");library("peakRAM")})
  clusterSetRNGStream(cl, iseed = 1236)
  registerDoSNOW(cl)
  nstart <- 10
  nstartv <- rep(floor(nstart / no_cores), no_cores)
  result <-
    clusterApply(cl, nstartv, function(n, x) {
      t <- Sys.time()
      df <- peakRAM(skmeans(x, no_clusters, method = "pclust", control = list(nruns = 8 ,maxiter = n,verbose = TRUE)))
      cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
    }, DFM)
  stopCluster(cl)
  return(list_to_df(result))
}

# ==============================================================================
#
#                         SKMEANS doParallel Iterations
#
# ==============================================================================

skmeansClusterDoParIter <- function() {
  #genetic
  set.seed(125)
  cl <- makeCluster(no_cores, outfile = "")
  clusterExport(cl, "no_clusters")
  registerDoSNOW(cl)
  clusterSetRNGStream(cl, iseed = 1236)
  nstart <- 10
  nstartv <- rep(ceiling(nstart / no_cores), no_cores)
  registerDoParallel(cl)
  result <- 
    foreach(n=nstartv,
            #.export= "DFM",
            .packages = c("skmeans","quanteda", "peakRAM"),
            .export = "DFM",
            .combine = rbind) %dopar% {
              t <- Sys.time()
              df <- peakRAM(skmeans(DFM, no_clusters ,method = "pclust",control = list(nruns = 8 ,maxiter = n,verbose = TRUE)))
              cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
            }
  stopCluster(cl)
  return(result)
}
