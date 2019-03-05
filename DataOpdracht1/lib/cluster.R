clusterMatrix <- function() {
  import(c("biganalytics", "cluster", "skmeans", "kmndirs"))
  
  # skmeansCluster()
  skmeansClusterPar10()
  # skmeansClusterPar100()
  # 1000 is teveel momenteel -> crash
  # skmeansClusterPar1000()
  
}

# ==============================================================================
#
#                                 SKMEANS Sequential
#
# ==============================================================================

skmeansCluster <- function(){
  result <- skmeans(DFM, 10 ,method = "pclust", control = list(nruns = 8, maxiter = 10, verbose = TRUE))
  return(result)
}

# ==============================================================================
#
#                                 SKMEANS Parallel 10
#
# ==============================================================================

skmeansClusterPar10 <- function() {
  #genetic
  set.seed(125)
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores, outfile = "")
  clusterExport(cl, "skmeans")
  clusterEvalQ(cl, library("quanteda"))
  clusterSetRNGStream(cl, iseed = 1236)
  registerDoParallel(cl)
  nstart <- 8
  nstartv <- rep(floor(nstart / no_cores), no_cores)
  result <-
    clusterApply(cl, nstartv, function(n, x)
      skmeans(x, 10, method = "pclust", control = list(nruns = n ,maxiter = 10,verbose = TRUE)), DFM)
  stopCluster(cl)
  return(result)
}

# ==============================================================================
#
#                                 SKMEANS Parallel 100
#
# ==============================================================================

skmeansClusterPar100 <- function() {
  #genetic
  set.seed(125)
  cl <- makeCluster(no_cores, outfile = "")
  clusterExport(cl, "skmeans")
  registerDoParallel(cl)
  clusterSetRNGStream(cl, iseed = 1236)
  nstart <- 8
  nstartv <- rep(ceiling(nstart / no_cores), no_cores)
  result <-
    clusterApply(cl, nstartv, function(n, x)
      skmeans(x,100,method = "pclust",control = list(nruns = n ,maxiter = 10,verbose = TRUE)), DFM)
  stopCluster(cl)
  return(result)
}

# ==============================================================================
#
#                           SKMEANS Parallel 1000
#
# ==============================================================================

# 1000 is teveel momenteel -> crash

skmeansClusterPar1000 <- function() {
  #genetic
  set.seed(125)
  cl <- makeCluster(no_cores, outfile = "")
  clusterExport(cl, "skmeans")
  registerDoParallel(cl)
  clusterSetRNGStream(cl, iseed = 1236)
  nstart <- 8
  nstartv <- rep(ceiling(nstart / no_cores), no_cores)
  result <-
    clusterApply(cl, nstartv, function(n, x)
      skmeans(x,1000,method = "pclust",control = list(nruns = n ,maxiter = 10,verbose = TRUE)), DFM)
  stopCluster(cl)
  return(result)
}