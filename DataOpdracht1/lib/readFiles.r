### LOOPED METHOD : read batch for given path, filename and batch no
read_batch <- function(batch_nr, ifn, ifp, ofn) {
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

### CREATE CLUSTER : make a cluster for parralel processing, fit to run read_batch
makeReadFileCluster <- function() {
  cl <- makeCluster(no_cores, outfile = "")
  clusterExport(cl, c("read_batch", "ifn", "ifp", "ofn"))
  clusterEvalQ(cl, { library("readr"); library("tibble"); library("data.table") })
  return(cl)
}

### LOOPING METHODS / PARALLEL-SEQUENTIAL : read all batches, return as one combined <dataframe>
read_parlapply <- function() {
  cl <- makeReadFileCluster()
  res <- parLapply(cl, 1:batches, read_batch)
  stopCluster(cl)
  return(as.data.frame(do.call(rbind, res)))
}
read_clusterapply <- function() {
  cl <- makeReadFileCluster()
  res <- clusterApply(cl, 1:batches, read_batch)
  stopCluster(cl)
  return(as.data.frame(do.call(rbind, res)))
}
read_doparallel_foreach <- function() {
  cl <- makeReadFileCluster()
  registerDoParallel(cl)
  res <- foreach(batch_nr = 1:batches, .combine = rbind) %dopar% {
    read_batch(batch_nr)
  }
  stopCluster(cl)
  return(res)
}
read_sequential <- function() {
  res <- lapply(1:batches, read_batch)
  return(as.data.frame(do.call(rbind, res)))
}

### MAIN FUNCTION : read batches and save as dataframe docs
readFiles <- function(f = read_sequential){
  import(c("readr","tibble","data.table"))
  print("Read Files Process started.")
  
  ## Batches info
  ifn <- "tls203_part"; ifp <- "../../../data/mini/"; ofn <- "ps18b_abstr"; batches <- 5
  
  ## Read in files and combine to 1 dataframe
  ps18b_abstr <- f()
  print("Compiling to dataframe 'docs'.")
  
  ## Rename and order columns
  docs <- ps18b_abstr[,c("appln_id", "appln_abstract", "appln_abstract_lg")]
  names(docs) <- c("doc_id", "text", "language")
  
  ## Save to rds file and return
  saveRDS(docs, file="docs.rds")
  print("Finished reading batches.")
  return(docs)
}

### CALL METHODS
readFiles_sequential <- function() {
  return(readFiles(read_sequential))
}
readFiles_doparallel_foreach <- function() {
  import(c("foreach", "doParallel"))
  return(readFiles(read_doparallel_foreach))
}
readFiles_clusterapply <- function() {
  import("parallel")
  return(readFiles(read_clusterapply))
}
readFiles_parlapply <- function() {
  import("parallel")
  return(readFiles(read_parlapply))
}

### BENCHMARK
benchmark_read <- function() {
  import(c("readr","tibble","data.table", "parallel", "foreach", "doParallel"))
  microbenchmark(read_clusterapply(), 
                 read_doparallel_foreach(), 
                 read_parlapply(), 
                 read_sequential(), 
                 times = 1)
}