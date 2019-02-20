readFiles <- function(){
  
  print("Read Files Process started.")
  
  ifn <- "tls203_part"
  batches <- 1
  ifp <- "../../../data/"
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
print("Finished reading batches.")
}