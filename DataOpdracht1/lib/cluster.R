clusterMatrix <- function() {
  import(c("biganalytics", "cluster", "skmeans"))
  
  skmeansCluster()
  # kmeansSparseCluster()
  
}

# ==============================================================================
#
#                                 SKMEANS
#
# ==============================================================================

skmeansCluster <- function() {
  kfitSK <- skmeans(DFM2, 2)
}

# ==============================================================================
#
#                            KMeansSparseCluster
#
# ==============================================================================

kmeansSparseCluster <- function(){
  
}