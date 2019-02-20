readFiles <- function(){
  ifn <- "tls203_part"
  batches <- 3
  #ifp <- "D:/DATA_Active/Data/PATSTAT_2018_B/1_Source_Data/source_zip_files/"
  ifp <- "../../../data/"
  ofn <- "ps18b_abstr"
  print("Var toegewezen")
  
  # READ AND PROCESS BATCHES
  
  
  #Read and process batches
  for(batch_nr in 3:batches) {
    print("for loop")
    # Compile file name to read
    batch_no <- sprintf("%02d", batch_nr)
    fn <- paste0(ifn, batch_no, ".txt.xz")
    fp <- ifp
    
    # Read batch file
    print("Read batch file")
    v <- paste0(ofn, "_", batch_no)
    assign(v, tibble::as_data_frame(readr::read_delim(paste0(fp, fn),
                                                      ",",
                                                      col_names=TRUE,
                                                      #col_types="icc",
                                                      # c:char; i:int; n:num; d:dbl
                                                      # l:log; D:date; T:date time; t:time
                                                      # ?:guess _/-:skip
                                                      #guess_max=min(1000, n_max),
                                                      quote = "\"",
                                                      escape_backslash = FALSE,
                                                      escape_double = FALSE,
                                                      na=character(),
                                                      #na=c("", "NA"),
                                                      #quoted_na = FALSE,
                                                      trim_ws= TRUE,
                                                      skip=0,
                                                      #n_max=smplsize,
                                                      n_max=Inf,
                                                      locale = locale(encoding = "UTF-8")
    ))
    )
    print("assignd")
    
    # Save intermediate batch file
    fn <- paste0(ofn, "_", batch_no, ".xz.RDa")
    save(list=v, file=fn, compress="xz", compression_level=2)
    print("saved")
    
  } # Read and process batches
  
  
  # COMBINE BATCHES
  
  
  # Variable name to use
  v <- ofn
  
  # Pattern to use
  pt <- paste0(ofn, "_", "[0-9]+")
  assign(v, as.data.frame(rbindlist(mget(ls(pattern=pt)))))
  
  # COMPILE DATAFRAME
  
  docs <- ps18b_abstr[,c("appln_id", "appln_abstract", "appln_abstract_lg")]
  names(docs) <- c("doc_id", "text", "language")
  return(docs)
}