clusterMatrix <- function() {
  import(c("biganalytics", "cluster", "skmeans"))
  
  # skmeansCluster(k)
  skmeansClusterPar(k = 10,nstarts = 10,maxiter = 10)
  # skmeansClusterDoPar(k)
  # skmeansClusterParIter(k)
  # skmeansClusterDoParIter(k)
  
}

# ==============================================================================
#
#                                 SKMEANS Sequential
#
# ==============================================================================

skmeansCluster <- function(k,nstarts,maxiter){
  result <- skmeans(DFM, k ,method = "pclust", control = list(nruns = 8, maxiter = 10, verbose = TRUE))
  return(result)
}

# ==============================================================================
#
#                                 SKMEANS Parallel 
#
# ==============================================================================

skmeansClusterPar <- function(k,nstarts,maxiter) {
  nstartv <- divide(x = nstarts,ncores = no_cores)

  cl <- makeCluster(no_cores, outfile = "")
  clusterEvalQ(cl, {library("quanteda");library("skmeans")})

  result <-
    clusterApply(cl, nstartv, function(n, dfm,maxiter)
      skmeans(dfm, k, method = "pclust", control = list(nruns = n ,maxiter = maxiter,verbose = TRUE)), DFM, maxiter)
  stopCluster(cl)
  return(result[[1]])
}

# ==============================================================================
#
#                                 SKMEANS doParallel 
#
# ==============================================================================

skmeansClusterDoPar <- function(k,nstarts,maxiter) {
  nstartv <- divide(x = nstarts,ncores = no_cores)

  cl <- makeCluster(no_cores, outfile = "")
  registerDoParallel(cl)
  
  result <- 
    foreach(n=nstartv,
            .packages = c("skmeans","quanteda"),
            .export= "DFM") %dopar% {
              skmeans(DFM, k ,method = "pclust",control = list(nruns = n ,maxiter = maxiter,verbose = TRUE))
    }
  
  stopCluster(cl)
  return(result[[1]])
}

# ==============================================================================
#
#                         SKMEANS  clusterApply Iterations
#
# ==============================================================================

skmeansClusterParIter <- function(k,nstarts,maxiter) {
  niterv <- divide(x=maxiter,ncores = no_cores)
  
  cl <- makeCluster(no_cores,outfile="")
  clusterEvalQ(cl, {library("quanteda");library("skmeans")})

  result <-
    clusterApply(cl, niterv, function(n, x)
      skmeans(x, k, method = "pclust", control = list(nruns = nstarts ,maxiter = n,verbose = TRUE)), DFM)
  stopCluster(cl)
  return(result[[1]])
}

# ==============================================================================
#
#                         SKMEANS doParallel Iterations
#
# ==============================================================================

skmeansClusterDoParIter <- function(k,nstarts,maxiter) {
  niterv <- divide(x=maxiter,ncores=no_cores)

  cl <- makeCluster(no_cores, outfile = "")
  registerDoParallel(cl)
  
  result <- 
    foreach(n=niterv,
            .packages = c("skmeans","quanteda"),
            .export = "DFM") %dopar% {
              skmeans(DFM, k ,method = "pclust",control = list(nruns = nstarts ,maxiter = n,verbose = TRUE))
    }
  
  stopCluster(cl)
  return(result[[1]])
}

divide <- function(x,ncores){
  if(x<ncores){
    return(rep(1,x))
  }
  list<-rep(0,ncores)
  
  for(i in 1:x){
    if(i>ncores){
      i=i%%ncores+1
    }
    list[[i]]=list[[i]]+1
  }
  return(list)
}

