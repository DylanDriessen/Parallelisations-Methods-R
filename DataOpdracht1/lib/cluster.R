clusterMatrix <- function() {
  import(c("biganalytics", "cluster", "skmeans", "kmndirs"))
  
  skmeansCluster()
  # kmeansSparseCluster()
  
}

# ==============================================================================
#
#                                 SKMEANS
#
# ==============================================================================

skmeansCluster <- function() {
  #genetic
  set.seed(125)
  cl <- makeCluster(no_cores, outfile = "")
  clusterExport(cl, "skmeans")
  registerDoParallel(cl)
  clusterSetRNGStream(cl, iseed = 1236)
  nstart <- 10
  nstartv <- rep(ceiling(nstart / no_cores), no_cores)
  result2 <-
    clusterApply(cl, nstartv, function(n, x)
      skmeans(x,5,method = "genetic",control = list(nstart = n,maxiter = 10,verbose = TRUE)), DFM2)
  stopCluster(cl)
  return(result2)
  
  # library (vegan)
  # library (cluster)
  # 
  # dis = dist(DFM2)^2
  # sil = silhouette (result2$cluster, dis)
  # windows() 
  # plot(sil)
  
  # i <- sapply(result, function(result) kfitSK2$
  
  
  # cl <- makeCluster(7, outfile = "")
  # registerDoParallel(cl)
  # kfitSK3 <- foreach(i = 1:7, .export = "skmeans") %dopar%
  #   skmeans(DFM2, 5, method = "pclust", control = list(nruns = i, verbose = TRUE))
  # stopCluster(cl)
  
  # kfit2 <- skmeans(DFM2, 5, m = 1.1,
  #         control = list(nruns = 5, verbose = TRUE))
  # ## Hard partition into 5 clusters.
  # hparty <- skmeans(DFM2, 5, control = list(verbose = TRUE))
  # ## Criterion value obtained:
  # hparty$value
  # ## Compare with "true" classifications:
  # class_ids <- attr(DFM2, "rclass")
  # table(class_ids, hparty$cluster)
  # require("cluster")
  # plot(silhouette(hparty))
}

# ==============================================================================
#
#                            KMeansSparseCluster
#
# ==============================================================================

kmeansSparseCluster <- function() {
  
}