source("lib/readFiles.r")
import(c("readr","tibble","data.table", "peakRAM", "foreach", "doParallel", "parallel","snow"))

#Wegschrijven naar een PNG bestand
read_doparallel_foreach_PNG<- function() {
  png('docs/read_doparallel_foreach_PNG.png')
  plot(snow.time(result <- microbenchmark(read_doparallel_foreach(), times = 1)))
  dev.off()
}

readFiles_clusterapply_PNG<- function(){
  png('docs/read_clusterapply.png')
  plot(snow.time(result <- microbenchmark(read_clusterapply(), times = 1)))
  dev.off()
}

readFiles_parlapply_PNG <- function(){
  png('docs/read_parLapply.png')
  plot(snow.time(result <- microbenchmark(read_parlapply(), times = 1)))
  dev.off()
}
  
readFiles_sequential_PNG <- function(){
  import(c("stringi","parallel","snow")) 
  png('docs/read_Sequential.png')
  plot(snow.time(result <- microbenchmark(read_sequential(), times = 1)))
  dev.off()
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
  

