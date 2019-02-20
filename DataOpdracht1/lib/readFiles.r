readFiles <- function(){
  
  print("Read Files Process started.")
  
  ifn <- "tls203_part"
  batches <- 5
  ifp <- "../../../data/mini/"
  ofn <- "ps18b_abstr"
  
  
  # READ AND PROCESS BATCHES
  
  #Read and process batches
  for(batch_nr in 1:batches) {
    
    # Compile file name to read
    batch_no <- sprintf("%02d", batch_nr)
    fn <- paste0(ifn, batch_no, ".txt.xz")
    fp <- ifp
    
    print(paste0("Start reading batch <", batch_nr, "> (", fn, ")"))
    
    # Read batch file
    v <- paste0(ofn, "_", batch_no)
    assign(v, tibble::as_data_frame(readr::read_delim(paste0(fp, fn), ",", col_names=TRUE, quote = "\"", 
                                                      escape_backslash = FALSE, escape_double = FALSE, na=character(), 
                                                      trim_ws= TRUE, skip=0, n_max=Inf, locale = locale(encoding = "UTF-8"))))
    
    # Save intermediate batch file
    fn <- paste0(ofn, "_", batch_no, ".xz.RDa")
    print(paste0("Start saving batch <", batch_nr, "> to ", fn, "."))
    save(list=v, file=fn, compress="xz", compression_level=2)
    print(paste0("Finished processing batch <", batch_nr, ">."))
    
  } # Read and process batches
  
  
  # COMBINE BATCHES
  
  # Variable name to use
  v <- ofn
  
  # Pattern to use
  pt <- paste0(ofn, "_", "[0-9]+")
  print("Combining batches...")
  assign(v, as.data.frame(rbindlist(mget(ls(pattern=pt)))))
  
  
  # COMPILE DATAFRAME
  
  print("Compiling to dataframe 'docs'.")
  docs <- ps18b_abstr[,c("appln_id", "appln_abstract", "appln_abstract_lg")]
  names(docs) <- c("doc_id", "text", "language")
  
  print("Finished reading batches.")
  return(docs)
  
}