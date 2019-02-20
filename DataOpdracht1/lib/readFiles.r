<<<<<<< HEAD
gc(verbose = TRUE)
memory.size()
object.size(readFiles())
=======
ifn <- "tls203_part"
batches <- 5
ifp <- "../../../data/mini/"
ofn <- "ps18b_abstr"

import(c("readr","tibble","data.table","stringi"))
import(c("foreach", "doParallel", "parallel"))
core_no <- detectCores()

microbenchmark(read_and_save_clusterapply(), read_and_save_doparallel_foreach(), read_and_save_parlapply(), read_and_save_sequential(), times = 3)

>>>>>>> 5a396f42ca5b8ab3efb8a2af87948cc60dd5c023
readFiles <- function(){
  
  print("Read Files Process started.")
  
  # READ AND PROCESS BATCHES
  
  #microbenchmark(read_and_save_clusterapply(), read_and_save_parlapply(), times = 1)
  read_and_save_doparallel_foreach()
  
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

read_and_save_parlapply <- function() {
  cl <- makeCluster(core_no)
  clusterExport(cl, c("ifn", "ifp", "ofn", "import"))
  clusterEvalQ(cl, import(c("readr","tibble","data.table")))
  parLapply(cl, 1:batches, read_and_save_batch)
  stopCluster(cl)
}

read_and_save_clusterapply <- function() {
  cl <- makeCluster(core_no)
  clusterExport(cl, c("ifn", "ifp", "ofn", "import"))
  clusterEvalQ(cl, import(c("readr","tibble","data.table")))
  clusterApply(cl, 1:batches, read_and_save_batch)
  stopCluster(cl)
}

read_and_save_doparallel_foreach <- function() {
  registerDoParallel(cores = core_no)
  
  foreach(batch_nr = 1:batches, .combine = rbind) %dopar% {
    read_and_save_batch(batch_nr)
  }
}

read_and_save_sequential <- function() {
  for(batch_nr in 1:batches) {
    read_and_save_batch(batch_nr)
  }
}

read_and_save_batch <- function(batch_nr) {
  # Compile file name to read
  batch_no <- sprintf("%02d", batch_nr)
  fn <- paste0(ifn, batch_no, ".txt.xz")
  fp <- ifp
  
<<<<<<< HEAD
}
=======
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
>>>>>>> 5a396f42ca5b8ab3efb8a2af87948cc60dd5c023
