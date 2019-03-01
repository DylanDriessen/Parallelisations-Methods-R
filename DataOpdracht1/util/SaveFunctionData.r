# Save peakRAM, cpu+ram, microbenchmark en snow.time plot 
#  naar de gespecifieerde folder voor functie f.
# vb. : saveFunctionData(read_sequential_peakRAM, "results/readFiles/sequential")
# Verwacht peakram functie, anders gebruik peakRAM = FALSE
saveFunctionData <- function(f, fpath, peakRAM = TRUE) {
  dir.create(fpath, showWarnings = FALSE) # gooi geen error als 
  start.time <- Sys.time()
  png(paste0(fpath, '/snow_plot.png'))
  if(peakRAM) {
    plot(snow.time(microbenchmarkResult <- microbenchmark(peakRAMResult <- f(), times = 1)))
  } else {
    plot(snow.time(microbenchmarkResult <- microbenchmark(f(), times = 1)))
  }
  dev.off()
  saveRDS(my_data[!(my_data$time < start.time),], file = paste0(fpath, '/resources.rds'))
  saveRDS(microbenchmarkResult, file = paste0(fpath, '/microbenchmark.rds'))
  if (peakRAM) saveRDS(peakRAMResult, file = paste0(fpath, '/peakRAM.rds'))
}