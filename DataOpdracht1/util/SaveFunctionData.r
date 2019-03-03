# Save peakRAM, cpu+ram, microbenchmark en snow.time plot 
#  naar de gespecifieerde folder voor functie f.
# vb. : saveFunctionData(read_sequential_peakRAM, "results/readFiles/sequential")
# Verwacht peakram functie, anders gebruik peakRAM = FALSE
saveFunctionData <- function(f, fpath) {
  dir.create(fpath, recursive = TRUE, showWarnings = FALSE) # gooi geen error als 
  start.time <- Sys.time()

  #png(paste0(fpath, '/snow_plot.png'))
  if(peakRAM) {
    plot(snow.time(microbenchmarkResult <- microbenchmark(peakRAMResult <- f(), times = 1)))
  } else {
    plot(snow.time(microbenchmarkResult <- microbenchmark(f(), times = 1)))
  }
  #dev.off()

  microbenchmarkResult <- microbenchmark(snowtime <- snow.time(peakRAMResult <- f()), times = 1)
  saveRDS(my_data[!(my_data$time < start.time),], file = paste0(fpath, '/resources.rds'))
  saveRDS(microbenchmarkResult, file = paste0(fpath, '/microbenchmark.rds'))
  saveRDS(peakRAMResult, file = paste0(fpath, '/peakRAM.rds'))
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




