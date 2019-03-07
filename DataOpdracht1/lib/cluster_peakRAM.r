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
  df <- peakRAM(skmeans(DFM, k ,method = "pclust", control = list(nruns = 8, maxiter = 10, verbose = TRUE)))
  cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
}

# ==============================================================================
#
#                                 SKMEANS Parallel 
#
# ==============================================================================

skmeansClusterPar_peakRAM <- function() {
  nstartv <- divide_peakRAM(x = nstarts,ncores = no_cores)
  
  cl <- makeCluster(no_cores, outfile = "")
  clusterEvalQ(cl, {library("quanteda");library("skmeans");library("peakRAM")})
  
  
  registerDoSNOW(cl)
  
  result <-
    clusterApply(cl, nstartv, function(n, dfm,maxiter) {
      t <- Sys.time()
      df <- peakRAM(skmeans(dfm, k, method = "pclust", control = list(nruns = n ,maxiter = maxiter,verbose = TRUE)))
      cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
    }, DFM, maxiter)
  stopCluster(cl)
  
  return(list_to_df(result))
}

# ==============================================================================
#
#                                 SKMEANS doParallel 
#
# ==============================================================================

skmeansClusterDoPar_peakRAM <- function() {
  nstartv <- divide_peakRAM(x = nstarts,ncores = no_cores)
  
  cl <- makeCluster(no_cores, outfile = "")
  registerDoSNOW(cl)
  
  result <- 
    foreach(n=nstartv,
            .packages = c("skmeans","quanteda", "peakRAM"),
            .export= "DFM",
            .combine = rbind) %dopar% {
              t <- Sys.time()
              df <- peakRAM(skmeans(DFM, k ,method = "pclust",control = list(nruns = n ,maxiter = maxiter,verbose = TRUE)))
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
  niterv <- divide_peakRAM(x = maxiter,ncores = no_cores)
  
  cl <- makeCluster(no_cores)
  
  clusterEvalQ(cl, {library("quanteda");library("skmeans");library("peakRAM")})
  
  registerDoSNOW(cl)
  
  result <-
    clusterApply(cl, niterv, function(n, x) {
      t <- Sys.time()
      df <- peakRAM(skmeans(x, k, method = "pclust", control = list(nruns = nstarts ,maxiter = n,verbose = TRUE)))
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
  niterv <- divide_peakRAM(x = maxiter,ncores = no_cores)
  
  cl <- makeCluster(no_cores, outfile = "")
  registerDoSNOW(cl)
  
  result <- 
    foreach(n=niterv,
            .packages = c("skmeans","quanteda", "peakRAM"),
            .export = "DFM",
            .combine = rbind) %dopar% {
              t <- Sys.time()
              df <- peakRAM(skmeans(DFM, k ,method = "pclust",control = list(nruns = nstarts ,maxiter = n,verbose = TRUE)))
              cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
            }
  stopCluster(cl)
  return(result)
}

divide_peakRAM <- function(x,ncores){
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
