source("lib/readFiles.r")
import(c("readr","tibble","data.table", "peakRAM", "foreach", "doParallel", "parallel","snow"))

source("lib/readFiles_peakRAM.r")
import(c("readr","tibble","data.table", "peakRAM", "foreach", "doParallel", "parallel", "microbenchmark"))

#Wegschrijven naar een PNG bestand
read_doparallel_foreach_PNG<- function() {
  png('docs/read_doparallel_foreach_PNG.png')
  plot(snow.time(result <- microbenchmark(peakR <- read_doparallel_foreach_peakRAM(), times = 1)))
  dev.off()
}

readFiles_clusterapply_PNG<- function(){
  png('docs/read_clusterapply.png')
  plot(snow.time(result <- microbenchmark(peakR <- read_clusterapply_peakRAM(), times = 1)))
  dev.off()
}

readFiles_parlapply_PNG <- function(){
  png('docs/read_parLapply.png')
  plot(snow.time(result <- microbenchmark(peakR <- read_parlapply_peakRAM(), times = 1)))
  dev.off()
}
  

readFiles_sequential_PNG <- function(){
  import(c("stringi","parallel","snow")) 
  png('docs/read_Sequential.png')
  plot(snow.time(result <- microbenchmark(read_sequential(), times = 1)))

}

datasize <- "mini"

import(c("stringi","parallel","snow")) 
readFiles_sequential <- function(){
  start.time <- Sys.time()
  fpath <- paste0("results/readFiles/sequential/", datasize, "/")
  png(paste0(fpath, 'snow_plot.png'))
  plot(snow.time({microbenchmarkResult <- microbenchmark({peakRAMResult <- read_sequential_peakRAM()}, times = 1)}))
  dev.off()
  saveRDS(microbenchmarkResult, file = paste0(fpath, 'microbenchmark.rds'))
  saveRDS(peakRAMResult, file = paste0(fpath, 'peakRAM.rds'))
}

readFiles_doparallel_foreach_ffdf_PNG <- function(){
  png('docs/read_doparallel_foreach_ffdf.png')
  plot(snow.time(result <- microbenchmark(read_doparallel_foreach_ffdf(), times = 1)))
  dev.off()
}

excecuteAllReadPNG <- function(){
  
  readFiles_clusterapply_PNG()
  read_doparallel_foreach_PNG()
  readFiles_parlapply_PNG()
  readFiles_sequential_PNG()
  read_files_fo
}
  

# readFiles_doparallel_foreach_ffdf <- function(){
#   png('docs/read_doparallel_foreach_ffdf.png')
#   plot(snow.time(result <- microbenchmark(read_doparallel_foreach_ffdf(), times = 1)))
#   dev.off()
# }
