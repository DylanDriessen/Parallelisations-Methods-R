preProcess <- function() {
  import("stringi")
  result <- stringi::stri_trans_general(docs$text, 'Latin-ASCII')
  return(result)
}

parLPreProcess <- function() {
  import(c("stringi","parallel"))  
  no_cores = detectCores()-1
  cluster <- makeCluster(no_cores)
  
  result <- parLapply(cluster,docs$text,stringi::stri_trans_general,id="Latin-ASCII")
  stopCluster(cluster)
  return(result)
}

fePreProcess <- function() {
  result <- foreach(str = docs$text) %do%
    stringi::stri_trans_general(str=str,id="Latin-ASCII")
  return(result)
}

feParProcess <- function() {
  import("doParallel")
  registerDoPar(detectCores()-1)
  
  result <- foreach(str = docs$text) %do%
    stringi::stri_trans_general(str=str,id="Latin-ASCII")
  
  return(result)
}