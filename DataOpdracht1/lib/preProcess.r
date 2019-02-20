preProcess <- function() {
  import("stringi")   # String manipulation
  # TRANSLITERATE TO LATIN
  docs$text <- stringi::stri_trans_general(docs$text, 'Latin-ASCII')
}

parLPreProcess <- function() {
  import(c("stringi","parallel"))   # String manipulation
  no_cores = detectCores()-1
  cluster <- makeCluster(no_cores)
  
  # TRANSLITERATE TO LATIN
  parLapply(cl,docs$text,stringi::stri_trans_general,id="Latin-ASCII")
  stopCluster(cluster)
}

fePreProcess <- function() {
  foreach(str = docs$text) %do%
    stringi::stri_trans_general(str=str,id="Latin_ASCII")
}

feParProcess <- function() {
  import("doParallel")
  registerDoPar(detectCores()-1)
  
  foreach(str = docs$text) %do%
    stringi::stri_trans_general(str=str,id="Latin_ASCII")
}