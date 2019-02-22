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
read_doparallel_foreach_peakRAM <- function() {
  cl <- makeReadFileClusterPeakRAM()
  registerDoParallel(cl)
  res <- foreach(batch_nr = 1:batches, .combine = rbind  ) %dopar% 
    read_batch_peakRAM(batch_nr)
  stopCluster(cl)
  return(res)
}
read_clusterapply_peakRAM<- function() {
  cl <- makeReadFileClusterPeakRAM()
  res <- clusterApply(cl, 1:batches, read_batch_peakRAM)
  stopCluster(cl)
  return(as.data.frame(do.call(rbind, res)))
}
read_parlapply_peakRAM <- function() {
  cl <- makeReadFileClusterPeakRAM()
  res <- parLapply(cl, 1:batches, read_batch_peakRAM)
  stopCluster(cl)
  return(as.data.frame(do.call(rbind, res)))
}
read_sequential_peakRAM <- function() {
  res <- lapply(1:batches, read_batch_peakRAM)
  return(as.data.frame(do.call(rbind, res)))
}

#functie roept de 4 soorten functies op 
read_peakRAM_to_rds <- function(){
  import(c("readr","tibble","data.table", "peakRAM", "foreach", "doParallel", "parallel"))
  sp <- "RShinyDashboardAfstudeer/data/"
  saveRDS(read_doparallel_foreach_peakRAM(), file = paste0(sp, "read_doparallel_foreach_peakRAM.rds"))
  saveRDS(read_parlapply_peakRAM(), file = paste0(sp, "read_parlapply_peakRAM.rds"))
  saveRDS(read_clusterapply_peakRAM(), file = paste0(sp, "read_clusterapply_peakRAM.rds"))
  saveRDS(read_sequential_peakRAM(), file = paste0(sp, "read_sequential_peakRAM.rds"))
}