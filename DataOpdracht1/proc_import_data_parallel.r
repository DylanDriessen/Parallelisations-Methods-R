
source("importPackage.r")
import(c("readr","tibble","data.table","stringi", "R.utils", "microbenchmark"))

ifn <- "tls203_part"
batches <- 3
#ifp <- "D:/DATA_Active/Data/PATSTAT_2018_B/1_Source_Data/source_zip_files/"
ifp <- "../../../data/"
ofn <- "ps18b_abstr"

# READ AND PROCESS BATCHES



# Read and process batches
par <- function() {
foreach(batch_nr = 1:batches) %dopar% {
  
  # Compile file name to read
  batch_no <- sprintf("%02d", batch_nr)
  fn <- paste0(ifn, batch_no, ".txt.xz")
  fp <- ifp
  
  ## read file and put in object ofn_<batch_no>
  v <- paste0(ofn, "_", batch_no)
  assign(v, tibble::as_data_frame(readr::read_delim(paste0(fp, fn), ",", col_names=TRUE, quote = "\"", 
                                                    escape_backslash = FALSE, escape_double = FALSE, na=character(), 
                                                    trim_ws= TRUE, skip=0, n_max=Inf, locale = locale(encoding = "UTF-8"))))
  
  ## Save intermediate batch file
  fn <- paste0(ofn, "_", batch_no, "par.xz.RDa")
  #save(list=v, file=fn, compress="xz", compression_level=2)
  
  }# Read and process batches
}



seq <- function() {
  for(batch_nr in 1:batches) {
    
    # Compile file name to read
    batch_no <- sprintf("%02d", batch_nr)
    fn <- paste0(ifn, batch_no, ".txt.xz")
    fp <- ifp
    
    ## read file and put in object ofn_<batch_no>
    v <- paste0(ofn, "_", batch_no)
    assign(v, tibble::as_data_frame(readr::read_delim(paste0(fp, fn), ",", col_names=TRUE, quote = "\"", 
                                                      escape_backslash = FALSE, escape_double = FALSE, na=character(), 
                                                      trim_ws= TRUE, skip=0, n_max=Inf, locale = locale(encoding = "UTF-8"))))
    
    ## Save intermediate batch file
    fn <- paste0(ofn, "_", batch_no, "seq.xz.RDa")
    #save(list=v, file=fn, compress="xz", compression_level=2)
    
  }# Read and process batches
}
microbenchmark(par(), seq(), times = 1)
  
  
  
  
  
  
  
  ## parallel batch read tryout
  
  # estimate how many lines there are in the file
  #estimate_lines <- as.numeric(stri_extract_first_regex(
  #  system(paste0("wc -l ", paste0(fp, fn)), intern = TRUE), "[0-9]+"))
  # calculate the amount of lines each core will process
  #estimate_chunk <- floor(estimate_lines/core_no)
  #system.time(nrow(fread(file=paste0(fp, fn))))
  #system.time(res <- countLines(paste0(fp,fn)))
  #res
  #chunks <- fread(skip estimate chunk)
  #}
  
  # Read batch file
  
  # w <- paste0(ofn, "_", batch_no, "2")
  
  # par <- function() {
  # assign(v, foreach(start = c(1, 400001, 800001, 1200001), .combine = rbind) %dopar%
  #        {
  #          tibble::as_data_frame(readr::read_delim(paste0(fp, fn),
  #                                                ",",
  #                                                col_names=FALSE,
  #                                                quote = "\"",
  #                                                escape_backslash = FALSE,
  #                                                escape_double = FALSE,
  #                                                na=character(),
  #                                                trim_ws= TRUE,
  #                                                skip=start,
  #                                                n_max=400000,
  #                                                locale = locale(encoding = "UTF-8")
  #        ))}
  # )}