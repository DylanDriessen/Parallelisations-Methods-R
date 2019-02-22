gc(verbose = TRUE)

source("util/importPackage.r")
import(c("readr","tibble","data.table","stringi", "microbenchmark"))
import(c("foreach", "doParallel", "parallel"))

ifn <- "tls203_part"
batches <- 5
ifp <- "../../../data/mini/"
ofn <- "ps18b_abstr"
no_cores <- detectCores()

# given a batch number, read batch and save in global env
read_batch <- function(batch_nr) {
  # Compile file name to read
  batch_no <- sprintf("%02d", batch_nr)
  fn <- paste0(ifn, batch_no, ".txt.xz")
  fp <- ifp
  #read batch file
  print(paste0("Start reading batch <", batch_no, "> (", fn, ")"))
  return(tibble::as_tibble(readr::read_delim(paste0(fp, fn), ",", col_names=TRUE, quote = "\"", 
                                             escape_backslash = FALSE, escape_double = FALSE, na=character(), 
                                             trim_ws= TRUE, skip=0, n_max=Inf, locale = locale(encoding = "UTF-8"))))
}

read_and_save_batch <- function(batch_nr) {
  # Compile file name to read
  batch_no <- sprintf("%02d", batch_nr)
  fn <- paste0(ifn, batch_no, ".txt.xz")
  fp <- ifp
  
  #read batch file
  print(paste0("Start reading batch <", batch_no, "> (", fn, ")"))
  v <- paste0(ofn, "_", batch_no)
  assign(v,
         tibble::as_tibble(readr::read_delim(paste0(fp, fn), ",", col_names=TRUE, quote = "\"", 
                                             escape_backslash = FALSE, escape_double = FALSE, na=character(), 
                                             trim_ws= TRUE, skip=0, n_max=Inf, locale = locale(encoding = "UTF-8")))
         , envir = globalenv())
  
  # # Save intermediate batch file
  fn <- paste0(ofn, "_", batch_no, ".xz.RDa")
  print(paste0("Start saving batch <", batch_no, "> to ", fn, "."))
  save(list=v, file=fn, compress="xz", compression_level=2)
  print(paste0("Finished processing batch <", batch_no, ">."))
}

makeReadFileCluster <- function() {
  cl <- makeCluster(no_cores, outfile = "")
  clusterExport(cl, c("read_batch", "ifn", "ifp", "ofn", "import"))
  clusterEvalQ(cl, import(c("readr","tibble","data.table")))
  return(cl)
}

read_parlapply <- function() {
  cl <- makeReadFileCluster()
  res <- parLapply(cl, 1:batches, read_batch)
  stopCluster(cl)
  res_df <- as.data.frame(do.call(rbind, res))
  rm(res)
  gc()
  return(res_df)
}

read_clusterapply <- function() {
  cl <- makeReadFileCluster()
  res <- clusterApply(cl, 1:batches, read_batch)
  stopCluster(cl)
  res_df <- as.data.frame(do.call(rbind, res))
  rm(res)
  gc()
  return(res_df)
}

read_doparallel_foreach <- function() {
  cl <- makeReadFileCluster()
  registerDoParallel(cl)
  res <- foreach(batch_nr = 1:batches, .combine = rbind) %dopar% {
    read_batch(batch_nr)
  }
  stopCluster(cl)
  gc()
  return(res)
}

read_sequential <- function() {
  res <- lapply(1:batches, read_batch)
  res_df <- as.data.frame(do.call(rbind, res))
  rm(res)
  gc()
  return(res_df)
}


readFiles <- function(){
  print("Read Files Process started.")
  # READ AND PROCESS BATCHES
  ps18b_abstr <- read_doparallel_foreach()
  # COMBINE BATCHES
  # # Variable name to use
  # w <- ofn
  # # Pattern to use
  # pt <- paste0(ofn, "_", "[0-9]+")
  # print("Combining batches...")
  # assign(w, as.data.frame(rbindlist(mget(ls(pattern=pt, envir = .GlobalEnv), envir = .GlobalEnv))))
  # COMPILE DATAFRAME
  print("Compiling to dataframe 'docs'.")
  docs <- ps18b_abstr[,c("appln_id", "appln_abstract", "appln_abstract_lg")]
  names(docs) <- c("doc_id", "text", "language")
  
  saveRDS(docs, file="docs.rds")
  
  print("Finished reading batches.")
  return(docs)
}

benchmark_readFiles <- function() {
  microbenchmark(read_clusterapply(), read_doparallel_foreach(), read_parlapply(), read_sequential(), times = 1)
}