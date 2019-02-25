if (!exists("docs")){
  load("docs.rds")
}

preProcess_seq <- function() {
  #process every line sequentially
  
  import("stringi")
  result <- stringi::stri_trans_general(docs$text, 'Latin-ASCII')
  return(result)
  
}

preProcess_parallel <- function(createPlot=FALSE) {
  #process every line in parallel with lapply
  plot=TRUE
  import(c("stringi","parallel","snow"))  
  no_cores = detectCores()
  cluster <- makeCluster(no_cores)
  
  if(plot){
    svg('docs/plot_preProcess_parallel.svg')
    plot(snow.time(result <- parLapply(cluster,docs$text,stringi::stri_trans_general,id="Latin-ASCII")))
    dev.off()
  }else{
    result <- parLapply(cluster,docs$text,stringi::stri_trans_general,id="Latin-ASCII")  
  }
  
  
  stopCluster(cluster)
  return(result)
}

preProcess_doparallel <- function(createPlot=FALSE) {
  #process every line sequentially with foreach
  
  import(c("stringi","doParallel"))
  registerDoParallel(detectCores())
  
  if(plot){
    svg('docs/plot_preProcess_doparallel.svg')
    plot(
      snow.time({
        result <- foreach(str = docs$text) %dopar%
          stringi::stri_trans_general(str=str,id="Latin-ASCII")  
      })
    )
    dev.off()
  }else{
    result <- foreach(str = docs$text) %dopar%
      stringi::stri_trans_general(str=str,id="Latin-ASCII")  
  }
  
  return(result)
}

preProcess_DevidedInChunks_doparallel <- function(createPlot=FALSE){
  #Devide descriptions into a number of chunks equal to the number of cores and process the chunks in parallel
  
  import(c("stringi","doParallel","doSNOW"))
  
  #split id's into chunks
  #https://code.i-harness.com/en/q/32a23d
  ids <- 1: length(docs$text)
  no_cores = detectCores()
  
  chunks <- split(ids,factor(sort(rank(ids)%%no_cores)))
  cluster <- makeCluster(detectCores()-1,outfile="")
  registerDoSNOW(cluster)
    
  #process
  if(plot){
    svg('docs/plot_preProcess_DevidedInChunks_doparallel.svg')
    plot(
      snow.time({
        res <- foreach(chunk = chunks,
                      .combine = c,
                      .export = "docs") %dopar%
          stringi::stri_trans_general(docs$text[chunk], 'Latin-ASCII')
      })
    )
    dev.off()
  }else{
    res <- foreach(chunk = chunks,
                   .combine = c,
                   .export = "docs") %dopar%
      stringi::stri_trans_general(docs$text[chunk], 'Latin-ASCII')
  }

  
  stopCluster(cluster)
  return(res)
}

preProcess_DevidedInChunks_parallel <- function(createPlot=FALSE){
  #Devide descriptions into a number of chunks equal to the number of cores and process the chunks in parallel
  
  import(c("stringi","parallel","snow"))
  
  #split id's into chunks
  #https://code.i-harness.com/en/q/32a23d
  ids <- 1: length(docs$text)
  no_cores = detectCores()
  chunks <- split(ids,factor(sort(rank(ids)%%no_cores)))
  
  cluster <- makeCluster(no_cores,outfile="")
  
  if(plot){
    svg('docs/plot_preProcess_DevidedInChunks_parallel.svg')
    plot(snow.time(res <- parLapply(cluster,chunks,function(chunk,doc){stringi::stri_trans_general(doc$text[chunk], 'Latin-ASCII')},doc=docs)))
    dev.off()
  }else{
    res <- parLapply(cluster,chunks,function(chunk,doc){stringi::stri_trans_general(doc$text[chunk], 'Latin-ASCII')},doc=docs)
  }
  
  stopCluster(cluster)
  
  return(res)
}

benchmark_preProcess <- function(times = 1,display=TRUE,save=FALSE,createPlot=FALSE){
  import("microbenchmark")
  
  benchmarkResult <- microbenchmark(preProcess_seq(createPlot=createPlot),
                                    preProcess_parallel(createPlot=createPlot),
                                    preProcess_doparallel(createPlot=createPlot),
                                    preProcess_DevidedInChunks_parallel(createPlot=createPlot),
                                    preProcess_DevidedInChunks_doparallel(createPlot=createPlot),
                                    times=times)
  if(save){
    save(benchmarkResult,file="doc/preProcessBenchmarkResult.rda")
  }
  
  if(display){
    return(benchmarkResult)  
  }
}
