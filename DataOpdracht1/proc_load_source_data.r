(.packages())


library(readr)
library(tibble)
library(data.table)
library(stringi)


################################################################################
#
# PATSTAT SOURRCE DATA STRUCTURE
#
################################################################################


# Character encoding: UTF-8
# Line structure: DOS (CR/LF)
# Field structure: CSV
#  Text delimiter: double quote
#  Embedded double quote: none
#  Embedded line break: \n
#  Escape character: none (except \n for line breaks)
#  Escape escape character: none
# First line contains field names: YES (also for part files)
# Decimal separator: point


################################################################################
#
# IMPORT PATSTAT TABLE ABSTRACT (TLS203, x PARTS)
#
################################################################################


ifn <- "tls203_part"
batches <- 1
#ifp <- "D:/DATA_Active/Data/PATSTAT_2018_B/1_Source_Data/source_zip_files/"
ifp <- ""
ofn <- "PS18B_ABSTR"


# READ AND PROCESS BATCHES


# Read and process batches
for(b in 1:batches) {

  # Compile file name to read
  batch_no <- sprintf("%02d", b)
  fn <- paste0(ifn, batch_no, ".txt.xz")
  fp <- ifp
  
  # Read batch file
  v <- paste0(ofn, "_", batch_no)
  assign(v,
         tibble::as_data_frame(readr::read_delim(paste0(fp, fn), 
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
  v <- paste0(ofn, "_", batch_no)
  fn <- paste0(ofn, "_", batch_no, ".RDa")
  cmp="xz"
  save(list=v, file=fn, compress=cmp, compression_level=2)

} # Read and process batches
