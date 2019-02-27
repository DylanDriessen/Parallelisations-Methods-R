source("lib/readFiles.r")
import(c("readr","tibble","data.table", "peakRAM", "foreach", "doParallel", "parallel"))

#Wegschrijven naar een PNG bestand
read_doparallel_foreach_PNG<- function() {
  png('docs/read_doparallel_foreach_PNG.png')
  plot(snow.time(result <- microbenchmark(read_doparallel_foreach(), times = 1)))
  dev.off()
}

readFiles_clusterapply <- function(){
  png('docs/read_clusterapply.png')
  plot(snow.time(result <- microbenchmark(read_clusterapply(), times = 1)))
  dev.off()
}

readFiles_parlapply <- function(){
  png('docs/read_parLapply.png')
  plot(snow.time(result <- microbenchmark(read_parlapply(), times = 1)))
  dev.off()
}
  
readFiles_sequential <- function(){
  import(c("stringi","parallel","snow")) 
  png('docs/read_Sequential.png')
  plot(snow.time(result <- microbenchmark(read_sequential(), times = 1)))
  dev.off()
}

readFiles_doparallel_foreach_ffdf <- function(){
  png('docs/read_doparallel_foreach_ffdf.png')
  plot(snow.time(result <- microbenchmark(read_doparallel_foreach_ffdf(), times = 1)))
  dev.off()
}
