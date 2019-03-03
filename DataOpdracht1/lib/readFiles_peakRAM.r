source("lib/readFiles.r")

### MAKE FILE CLUSTER
makeReadFileClusterPeakRAM <- function() {
  cl <- makeReadFileCluster()
  clusterExport(cl, c("read_batch_peakRAM"))
  clusterEvalQ(cl, library("peakRAM"))
  return(cl)
}

### LOOPED FUNC
read_batch_peakRAM <- function(x) { 
  t <- Sys.time()
  df <- peakRAM(read_batch(x))
  cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
}

### LOOPS
# read_doparallel_foreach_peakRAM2 <- function() {
#   png(paste0('test', '/snow_plot.png'))
#   plot(snow.time({
#     cl <- makeCluster(min(no_cores, batches), outfile = "")
#     clusterExport(cl, c("read_batch", "read_batch_peakRAM", "ifn", "ifp", "ofn"))
#     clusterEvalQ(cl, { library("readr"); library("tibble"); library("data.table"); library("peakRAM") })
#     registerDoSNOW(cl)
#     res <- foreach(batch_nr = 1:batches, .combine = rbind  ) %dopar% read_batch_peakRAM(batch_nr)
#     stopCluster(cl)}
#   ))
#   dev.off()
#   return(res)
# }
read_doparallel_foreach_peakRAM <- function() {
  cl <- makeReadFileClusterPeakRAM()
  registerDoSNOW(cl)
  on.exit(stopCluster(cl))
  return(foreach(batch_nr = 1:batches, .combine = rbind  ) %dopar% read_batch_peakRAM(batch_nr))
}
read_clusterapply_peakRAM<- function() {
  cl <- makeReadFileClusterPeakRAM()
  on.exit(stopCluster(cl))
  return(list_to_df(clusterApply(cl, 1:batches, read_batch_peakRAM)))
}
read_parlapply_peakRAM <- function() {
  cl <- makeReadFileClusterPeakRAM()
  on.exit(stopCluster(cl))
  return(list_to_df(parLapply(cl, 1:batches, read_batch_peakRAM)))
}
read_sequential_peakRAM <- function() {
  return(list_to_df(lapply(1:batches, read_batch_peakRAM)))
}