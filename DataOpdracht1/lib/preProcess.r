preProcess_seq <- function() {
  #process every line sequentially
  
  import("stringi")
  result <- stringi::stri_trans_general(docs$text, 'Latin-ASCII')
  return(result)
  
}

prePorcess_parLapply <- function() {
  #process every line in parallel with lapply
  
  import(c("stringi","parallel"))  
  no_cores = detectCores()-1
  cluster <- makeCluster(no_cores)
  
  result <- parLapply(cluster,docs$text,stringi::stri_trans_general,id="Latin-ASCII")
  stopCluster(cluster)
  return(result)
}

preProcess_foreachPar <- function() {
  #process every line sequentially with foreach
  
  import(c("stringi","doParallel"))
  registerDoParallel(detectCores()-1)
  
  result <- foreach(str = docs$text) %dopar%
    stringi::stri_trans_general(str=str,id="Latin-ASCII")
  
  return(result)
}

preProcess_DevidedInChunks_doparallel <- function(){
  #Devide descriptions into a number of chunks equal to the number of cores and process the chunks in parallel
  
  import(c("stringi","doParallel"))
  
  #split id's into chunks
  #https://code.i-harness.com/en/q/32a23d
  ids <- 1: length(docs$text)
  no_cores = detectCores()-1
  chunks <- split(ids,factor(sort(rank(ids)%%no_cores)))
  
  #process
  registerDoParallel(detectCores()-1)
  res <- foreach(chunk = chunks,
                 .combine = c) %dopar%
    stringi::stri_trans_general(docs$text[chunk], 'Latin-ASCII')
  
  stopImplicitCluster()
  
  return(res)
}

preProcess_DevidedInChunks_parallel <- function(){
  #Devide descriptions into a number of chunks equal to the number of cores and process the chunks in parallel
  
  import(c("stringi","parallel"))
  
  #split id's into chunks
  #https://code.i-harness.com/en/q/32a23d
  ids <- 1: length(docs$text)
  no_cores = detectCores()-1
  chunks <- split(ids,factor(sort(rank(ids)%%no_cores)))
  
  cluster <- makeCluster(no_cores)
  res <- parLapply(cluster,chunks,function(chunk,doc){stringi::stri_trans_general(doc$text[chunk], 'Latin-ASCII')},doc=docs)
  stopCluster(cluster)
  
  return(res)
}

benchmark_preProcess <- function(times = 3,display=true,save=false){
  benchmarkResult <- microbenchmark(preProcess_seq(),prePorcess_parLapply(),preProcess_DevidedInChunks_doparallel(),times = 1)
  
  if(save){
    save(benchmakrResult,file="doc/preProcessBenchmarkResult.rda")
  }
  
  if(display){
    return(benchmakrResult)  
  }
  
  
}


