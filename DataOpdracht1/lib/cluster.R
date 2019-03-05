clusterMatrix <- function() {
  import(c("biganalytics", "cluster", "skmeans"))
  
  k <- 10
  
  # skmeansCluster(k)
  skmeansClusterPar(k)
  # skmeansClusterDoPar(k)
  # skmeansClusterParIter(k)
  # skmeansClusterDoParIter(k)
  
}

# ==============================================================================
#
#                                 SKMEANS Sequential
#
# ==============================================================================

skmeansCluster <- function(k){
  result <- skmeans(DFM, k ,method = "pclust", control = list(nruns = 8, maxiter = 10, verbose = TRUE))
  return(result)
}

# ==============================================================================
#
#                                 SKMEANS Parallel 
#
# ==============================================================================

skmeansClusterPar <- function(k) {
  #genetic
  set.seed(125)
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores, outfile = "")
  #clusterExport(cl, "skmeans")
  clusterEvalQ(cl, {library("quanteda");library("skmeans")})
  clusterSetRNGStream(cl, iseed = 1236)
  registerDoParallel(cl)
  nstart <- 8
  nstartv <- rep(floor(nstart / no_cores), no_cores)
  result <-
    clusterApply(cl, nstartv, function(n, x)
      skmeans(x, k, method = "pclust", control = list(nruns = n ,maxiter = 10,verbose = TRUE)), DFM)
  stopCluster(cl)
  return(result[[1]])
}

# ==============================================================================
#
#                                 SKMEANS doParallel 
#
# ==============================================================================

skmeansClusterDoPar <- function(k) {
  #genetic
  set.seed(125)
  cl <- makeCluster(no_cores, outfile = "")
  ##clusterExport(cl, "skmeans")
  registerDoParallel(cl)
  clusterSetRNGStream(cl, iseed = 1236)
  nstart <- 8
  nstartv <- rep(ceiling(nstart / no_cores), no_cores)
  registerDoParallel(cl)
  
  result <- 
    foreach(n=nstartv,
            .export= "DFM",
            .packages = c("skmeans","quanteda")) %dopar% {
              skmeans(DFM, k ,method = "pclust",control = list(nruns = n ,maxiter = 10,verbose = TRUE))
    }
  
  stopCluster(cl)
  return(result[[1]])
}

# ==============================================================================
#
#                         SKMEANS  clusterApply Iterations
#
# ==============================================================================

skmeansClusterParIter <- function(k) {
  #genetic
  set.seed(125)
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores, outfile = "")
  clusterExport(cl, "skmeans")
  clusterEvalQ(cl, library("quanteda"))
  clusterSetRNGStream(cl, iseed = 1236)
  registerDoParallel(cl)
  nstart <- 10
  nstartv <- rep(floor(nstart / no_cores), no_cores)
  result <-
    clusterApply(cl, nstartv, function(n, x)
      skmeans(x, k, method = "pclust", control = list(nruns = 8 ,maxiter = n,verbose = TRUE)), DFM)
  stopCluster(cl)
  return(result[[1]])
}

# ==============================================================================
#
#                         SKMEANS doParallel Iterations
#
# ==============================================================================

skmeansClusterDoParIter <- function(k) {
  #genetic
  set.seed(125)
  cl <- makeCluster(no_cores, outfile = "")
  ##clusterExport(cl, "skmeans")
  registerDoParallel(cl)
  clusterSetRNGStream(cl, iseed = 1236)
  nstart <- 10
  nstartv <- rep(ceiling(nstart / no_cores), no_cores)
  registerDoParallel(cl)
  
  result <- 
    foreach(n=nstartv,
            #.export= "DFM",
            .packages = c("skmeans","quanteda")) %dopar% {
              skmeans(DFM, k ,method = "pclust",control = list(nruns = 8 ,maxiter = n,verbose = TRUE))
            }
  
  stopCluster(cl)
  return(result[[1]])
}
