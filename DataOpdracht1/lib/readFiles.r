### LOOPED METHOD : read batch for given path, filename and batch no

read_batch <- function(batch_nr) {
  ## Compile file name to read
  batch_no <- sprintf("%02d", batch_nr)
  fn <- paste0(ifn, batch_no, ".txt.xz")
  fp <- ifp
  ## Read batch file
  print(paste0("Start reading batch <", batch_no, "> (", normalizePath(fp), "/", fn, ")"))
  return(tibble::as_tibble(readr::read_delim(paste0(fp, fn), ",", col_names=TRUE, quote = "\"", 
                                             escape_backslash = FALSE, escape_double = FALSE, na=character(), 
                                             trim_ws= TRUE, skip=0, n_max=Inf, locale = locale(encoding = "UTF-8"))))
}

read_batch_ffdf <- function(batch_nr) {
  batch_no <- sprintf("%02d", batch_nr)
  fn <- paste0(ifn, batch_no, ".txt.xz")
  fp <- ifp
  ## Read batch file
  print(paste0("Start reading batch <", batch_no, "> (", normalizePath(fp), "/", fn, ")"))
  return(read.table.ffdf(file = paste0(fp, fn), sep=",", skip=1, appendLevels = FALSE, levels = NULL))
}

### CREATE CLUSTER : make a cluster for parralel processing, fit to run read_batch
makeReadFileCluster <- function() {
  cl <- makeCluster(min(no_cores, batches), outfile = "")
  clusterExport(cl, c("read_batch", "ifn", "ifp", "ofn"))
  clusterEvalQ(cl, { library("readr"); library("tibble"); library("data.table") })
  return(cl)
}
makeReadFileCluster_ffdf <- function() {
  cl <- makeCluster(min(no_cores, batches), outfile = "")
  clusterExport(cl, c("read_batch_ffdf", "ifn", "ifp", "ofn"))
  clusterEvalQ(cl, { library("ff") })
  return(cl)
}

list_to_df <- function(l){ return(as.data.frame(do.call(rbind, l))) }

### LOOPING METHODS / PARALLEL-SEQUENTIAL : read all batches, return as one combined <dataframe>
read_parlapply <- function() {
  cl <- makeReadFileCluster()
  on.exit(stopCluster(cl))
  return(list_to_df(parLapply(cl, 1:batches, read_batch)))
}
read_clusterapply <- function() {
  cl <- makeReadFileCluster()
  on.exit(stopCluster(cl))
  return(list_to_df(clusterApply(cl, 1:batches, read_batch)))
}
read_doparallel_foreach <- function() {
  cl <- makeReadFileCluster()
  registerDoParallel(cl)
  on.exit(stopCluster(cl))
  return( foreach(batch_nr = 1:batches, .combine = rbind) %dopar% read_batch(batch_nr) )
}
read_sequential <- function() {
  return(list_to_df(lapply(1:batches, read_batch)))
}

read_doparallel_foreach_ffdf <- function() {
  cl <- makeReadFileCluster_ffdf()
  registerDoParallel(cl)
  on.exit(stopCluster(cl))
  return( foreach(batch_nr = 1:batches, .combine = ffdfappend ) %dopar% {
    res <- read_batch_ffdf(batch_nr)
    res
  })
}

### MAIN FUNCTION : read batches and save as dataframe docs
readFiles <- function(f = read_sequential){
  print("Read Files Process started.")
  
  ## Read in files and combine to 1 dataframe
  ps18b_abstr <- f()
  print("Compiling to dataframe 'docs'.")
  
  ## Rename and order columns
  #docs <- ps18b_abstr[,c("appln_id", "appln_abstract", "appln_abstract_lg")]
  docs <- ps18b_abstr[,c(1, 3, 2)]
  names(docs) <- c("doc_id", "text", "language")
  
  print("Finished reading batches.")
  return(docs)
}

### CALL METHODS
readFiles_sequential <- function() {
  return(readFiles(read_sequential))
}
readFiles_doparallel_foreach <- function() {
  return(readFiles(read_doparallel_foreach))
}
readFiles_clusterapply <- function() {
  return(readFiles(read_clusterapply))
}
readFiles_parlapply <- function() {
  return(readFiles(read_parlapply))
}
readFiles_doparallel_foreach_ffdf <- function() {
  return(readFiles(read_doparallel_foreach_ffdf))
}

### BENCHMARK
benchmark_read <- function() {
 benchmarkReadFilesSmall <-  microbenchmark(read_clusterapply(), 
                 read_doparallel_foreach(), 
                 read_parlapply(), 
                 read_sequential(),
                 times = 1)
 saveRDS(benchmarkReadFilesSmall, "~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/benchmarkReadFilesSmall.rds")
}
