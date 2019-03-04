# Save peakRAM, cpu+ram, microbenchmark en snow.time plot 
#  naar de gespecifieerde folder voor functie f.
# vb. : saveFunctionData(read_sequential_peakRAM, "results/readFiles/sequential")
# Verwacht peakram functie, anders gebruik peakRAM = FALSE
saveFunctionData <- function(f, fpath) {
  dir.create(fpath, recursive = TRUE, showWarnings = FALSE) # gooi geen error als 
  start.time <- Sys.time()
  
  ### voer functie uit
  microbenchmarkResult <- microbenchmark(snowtime <- snow.time(peakRAMResult <- f()), times = 1)
  
  ### save ram/cpu data uit my_data
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

# saveFunctionData_doSNOW <- function(f, fpath) {
#   dir.create(fpath, recursive = TRUE, showWarnings = FALSE) # gooi geen error als 
#   start.time <- Sys.time()
#   snowplot <- plot(doSNOW::snow.time(microbenchmarkResult <- microbenchmark(peakRAMResult <- f(), times = 1)))
#   saveRDS(my_data[!(my_data$time < start.time),], file = paste0(fpath, '/resources.rds'))
#   saveRDS(microbenchmarkResult, file = paste0(fpath, '/microbenchmark.rds'))
#   saveRDS(peakRAMResult, file = paste0(fpath, '/peakRAM.rds'))
#   png(paste0(fpath, '/snow_plot.png')); snowplot; dev.off()
# }


