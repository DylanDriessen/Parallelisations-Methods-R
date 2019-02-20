gc(verbose = TRUE)

source("util/importPackage.r")
import(c("readr","tibble","data.table","stringi", "microbenchmark"))
import(c("foreach", "doParallel", "parallel"))

ifn <- "tls203_part"
batches <- 5
ifp <- "../../../data/mini/"
ofn <- "ps18b_abstr"
core_no <- detectCores()

# given a batch number, read batch and save in global env
read_and_save_batch <- function(batch_nr) {
  # Compile file name to read
  batch_no <- sprintf("%02d", batch_nr)
  fn <- paste0(ifn, batch_no, ".txt.xz")
  fp <- ifp
  
  #read batch file
  print(paste0("Start reading batch <", batch_no, "> (", fn, ")"))
  v <- paste0(ofn, "_", batch_no)
  assign(v, tibble::as_data_frame(readr::read_delim(paste0(fp, fn), ",", col_names=TRUE, quote = "\"", 
                                                    escape_backslash = FALSE, escape_double = FALSE, na=character(), 
                                                    trim_ws= TRUE, skip=0, n_max=Inf, locale = locale(encoding = "UTF-8")))
         , envir = globalenv())
  # Save intermediate batch file
  fn <- paste0(ofn, "_", batch_no, ".xz.RDa")
  print(paste0("Start saving batch <", batch_no, "> to ", fn, "."))
  save(list=v, file=fn, compress="xz", compression_level=2)
  print(paste0("Finished processing batch <", batch_no, ">."))
}

makeReadFileCluster <- function() {
  cl <- makeCluster(core_no, outfile = "")
  clusterExport(cl, c("read_and_save_batch", "ifn", "ifp", "ofn", "import"))
  clusterEvalQ(cl, import(c("readr","tibble","data.table")))
  return(cl)
}

read_and_save_parlapply <- function() {
  cl <- makeReadFileCluster()
  parLapply(cl, 1:batches, read_and_save_batch)
  stopCluster(cl)
}

read_and_save_clusterapply <- function() {
  cl <- makeReadFileCluster()
  clusterApply(cl, 1:batches, read_and_save_batch)
  stopCluster(cl)
}

read_and_save_doparallel_foreach <- function() {
  cl <- makeReadFileCluster()
  registerDoParallel(cl)
  
  foreach(batch_nr = 1:batches, .combine = rbind) %dopar% {
    read_and_save_batch(batch_nr)
  }
}

read_and_save_sequential <- function() {
  for(batch_nr in 1:batches) {
    read_and_save_batch(batch_nr)
  }
}


readFiles <- function(){
  
  print("Read Files Process started.")
  
  # READ AND PROCESS BATCHES
  
  read_and_save_clusterapply()
  
  # COMBINE BATCHES
  
  # Variable name to use
  w <- ofn
  # Pattern to use
  pt <- paste0(ofn, "_", "[0-9]+")
  print("Combining batches...")
  assign(w, as.data.frame(rbindlist(mget(ls(pattern=pt, envir = globalenv()), envir = globalenv()))))
  
  
  # COMPILE DATAFRAME
  
  print("Compiling to dataframe 'docs'.")
  
  docs <- ps18b_abstr[,c("appln_id", "appln_abstract", "appln_abstract_lg")]
  names(docs) <- c("doc_id", "text", "language")
  
  print("Finished reading batches.")
  return(docs)
}

benchmark_readFiles <- function() {
  microbenchmark(read_and_save_clusterapply(), read_and_save_doparallel_foreach(), read_and_save_parlapply(), read_and_save_sequential(), times = 3)
}
