# Save peakRAM, cpu+ram, microbenchmark en snow.time plot 
#  naar de gespecifieerde folder voor functie f.
# vb. : saveFunctionData(read_sequential_peakRAM, "results/readFiles/sequential")
# Verwacht peakram functie, anders gebruik peakRAM = FALSE

source("lib/preProcess_peakRAM.r")
source("lib/readFiles_peakRAM.r")
source("lib/createDTM_peakRAM.r")
source("lib/createCorpus_peakRAM.r")

saveFunctionData <- function(f, fpath) {
  dir.create(fpath, recursive = TRUE, showWarnings = FALSE) # gooi geen error als 
  start.time <- Sys.time()
  microbenchmarkResult <- microbenchmark(snowtime <- snow.time(peakRAMResult <- f()), times = 1)
  print("CHECK")
  print(my_data[!(my_data$time < start.time),])
  saveRDS(my_data[!(my_data$time < start.time),], file = paste0(fpath, '/resources.rds'))
  saveRDS(microbenchmarkResult, file = paste0(fpath, '/microbenchmark.rds'))
  saveRDS(peakRAMResult, file = paste0(fpath, '/peakRAM.rds'))
  png(paste0(fpath, '/snow_plot.png')); plot(snowtime); dev.off()
}

#saveFunctionData(createCorpusCluster_peakRAM, "results/createCorpus/Cluster")
#saveFunctionData(VCorp_peakRAM, "results/createCorpus/Vcorp")
#saveFunctionData(Quan_peakRAM, "results/createCorpus/Quan")


#saveFunctionData(createDfmChunks_peakRAM, "results/createDTM/dfmChunks")
#saveFunctionData(createDFM_peakRAM, "results/createDTM/dfm")
saveFunctionData(createDFMasDTM, "results/createDTM/dfmASdtm")


#saveFunctionData(read_doparallel_foreach_peakRAM, "results/preProcess/test" )
#saveFunctionData(preProcessSequential_peakRAM, "results/preProcess/sequential")
#saveFunctionData(preProcessParallel_peakRAM, "results/preProcess/parallel")
#saveFunctionData(preProcessDoparallel_peakRAM, "results/preProcess/doparallel")
#saveFunctionData(preProcessCluster_peakRAM,"results/preProcess/cluster" )
#saveFunctionData(preProcessDoparallelChunked_peakRAM, "results/preProcess/doparallelChunked")
#saveFunctionData(preProcessParallelChunked_peakRAM, "results/preProcess/parallelChunked")
#saveFunctionData(preProcessClusterChunked_peakRAM, "results/preProcess/clusterChunked")

# saveFunctionData_doSNOW <- function(f, fpath) {
#   dir.create(fpath, recursive = TRUE, showWarnings = FALSE) # gooi geen error als 
#   start.time <- Sys.time()
#   snowplot <- plot(doSNOW::snow.time(microbenchmarkResult <- microbenchmark(peakRAMResult <- f(), times = 1)))
#   saveRDS(my_data[!(my_data$time < start.time),], file = paste0(fpath, '/resources.rds'))
#   saveRDS(microbenchmarkResult, file = paste0(fpath, '/microbenchmark.rds'))
#   saveRDS(peakRAMResult, file = paste0(fpath, '/peakRAM.rds'))
#   png(paste0(fpath, '/snow_plot.png')); snowplot; dev.off()
# }




