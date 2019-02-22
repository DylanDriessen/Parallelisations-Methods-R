source("readFiles.r")
#RAM TEST

#PEAKRAM
makeReadFileClusterPeakRAM <- function() {
  cl <- makeReadFileCluster()
  clusterEvalQ(cl, import("peakRAM"))
  return(cl)
}

read_doparallel_foreach_peakRAM <- function() {
  cl <- makeReadFileClusterPeakRAM()
  registerDoParallel(cl)
  res <- foreach(batch_nr = 1:batches, .combine = rbind  ) %dopar% 
    peakRAM(read_batch(batch_nr))[1,2:4]
  stopCluster(cl)
  return(colSums(res))
}

#PEAKRAM
read_clusterapply_peakRAM<- function() {
  cl <- makeReadFileClusterPeakRAM()
  res <- clusterApply(cl, 1:batches, function(x) { peakRAM(read_batch(x))[,2:4] })
  stopCluster(cl)
  return(colSums(as.data.frame(do.call(rbind, res))))
}

#PEAKRAM
read_parlapplyPeakRAM <- function() {
  cl <- makeReadFileClusterPeakRAM()
  res <- parLapply(cl, 1:batches, function(x) { peakRAM(read_batch(x))[,2:4] })
  stopCluster(cl)
  return(colSums(as.data.frame(do.call(rbind, res))))
}

#functie roept de 4 soorten functies op 
call_functions_for_ram <- function(){
  import("readr","tibble","data.table", "peakRAM", "dplyr", "foreach", "doParallel", "parallel")
  ram_data <- rbind(parallelForeach = read_doparallel_foreach_peakRAM(), 
                    clusterApply = read_clusterapply_peakRAM(), 
                    parLapplyParallel = read_parlapplyPeakRAM())
  saveRDS(ram_data, file = "~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/ram_data.rds")
}