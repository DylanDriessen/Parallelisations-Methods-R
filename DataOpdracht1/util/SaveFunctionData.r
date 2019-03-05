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
  
  ### voer functie uit
  microbenchmarkResult <- microbenchmark(snowtime <- snow.time(peakRAMResult <- f()), times = 1)

  saveRDS(my_data[!(my_data$time < start.time),], file = paste0(fpath, '/resources.rds'))
  ### save peakram
  saveRDS(peakRAMResult, file = paste0(fpath, '/peakRAM.rds'))
  
  #saveRDS(microbenchmarkResult, file = paste0(fpath, '/microbenchmark.rds'))
  ### save microbenchmark in shared file
  ## split path op in functie path en method
  splitpath <- unlist(strsplit(fpath, '/(?=[^/]+$)', perl=TRUE))
  benchmarkpath <- paste0(splitpath[1], '/microbenchmark.rds')
  ## als file exists, load file. anders maak nieuwe lege dataframe
  if(file.exists(benchmarkpath))
    microbenchmark <- readRDS(benchmarkpath)
  else
    microbenchmark <- data.frame(expr = character(), time = as.numeric(character()), stringsAsFactors = FALSE)
  ## slaag microbenchmarkresult op als nieuwe rij, of overschrijf rij
  microbenchmark[splitpath[2],] <- c(expr = splitpath[2], time = microbenchmarkResult[1,2])
  ## slaag op in file
  saveRDS(microbenchmark, benchmarkpath)
  
  ### save snowplot
  png(paste0(fpath, '/snow_plot.png')); plot(snowtime); dev.off()
}


saveFunctionData(read_doparallel_foreach_peakRAM, "results/readFiles/foreach")
saveFunctionData(read_clusterapply_peakRAM, "results/readFiles/clusterapply")
saveFunctionData(read_parlapply_peakRAM , "results/readFiles/parlapply")
saveFunctionData(read_sequential_peakRAM, "results/readFiles/sequential")

saveFunctionData(createCorpusCluster_peakRAM, "results/createCorpus/Cluster")
saveFunctionData(VCorp_peakRAM, "results/createCorpus/Vcorp")
saveFunctionData(Quan_peakRAM, "results/createCorpus/Quan")


 saveFunctionData(createDfmChunks_peakRAM, "results/createDTM/dfmChunks")
 saveFunctionData(createDFM_peakRAM, "results/createDTM/dfm")
 saveFunctionData(createDFMasDTM_peakRAM, "results/createDTM/dfmASdtm")




# saveFunctionData(preProcessSequential_peakRAM, "results/preProcess/sequential")
#saveFunctionData(preProcessParallel_peakRAM, "results/preProcess/parallel") niet uitvoeren
#saveFunctionData(preProcessDoparallel_peakRAM, "results/preProcess/doparallel") niet uitvoeren
#saveFunctionData(preProcessCluster_peakRAM,"results/preProcess/cluster" ) niet uitvoeren

# saveFunctionData(preProcessDoparallelChunked_peakRAM, "results/preProcess/doparallelChunked")
# saveFunctionData(preProcessParallelChunked_peakRAM, "results/preProcess/parallelChunked")
# saveFunctionData(preProcessClusterChunked_peakRAM, "results/preProcess/clusterChunked")

#saveFunctionData(preProcessDoparallelChunked_peakRAM, "results/preProcess/doparallelChunked")
#saveFunctionData(preProcessParallelChunked_peakRAM, "results/preProcess/parallelChunked")
#saveFunctionData(preProcessClusterChunked_peakRAM, "results/preProcess/clusterChunked")
#saveFunctionData(createDTMChunked_peakRAM, "test/test")
#saveFunctionData(createDTM_peakRAM, "test/test/test")



# saveFunctionData_doSNOW <- function(f, fpath) {
#   dir.create(fpath, recursive = TRUE, showWarnings = FALSE) # gooi geen error als 
#   start.time <- Sys.time()
#   snowplot <- plot(doSNOW::snow.time(microbenchmarkResult <- microbenchmark(peakRAMResult <- f(), times = 1)))
#   saveRDS(my_data[!(my_data$time < start.time),], file = paste0(fpath, '/resources.rds'))
#   saveRDS(microbenchmarkResult, file = paste0(fpath, '/microbenchmark.rds'))
#   saveRDS(peakRAMResult, file = paste0(fpath, '/peakRAM.rds'))
#   png(paste0(fpath, '/snow_plot.png')); snowplot; dev.off()
# }




