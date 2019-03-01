# if (!exists("docs")){
#   load("docs.rds")
# }

preProcessSequential <- function() {
  #process every line sequentially
  print("#####################preProcess_seq")
  
  import("stringi")
  result <- stringi::stri_trans_general(docs$text, 'Latin-ASCII')
  return(result)
  
}

preProcessParallel <- function(createPlot=FALSE,no_cores=detectCores()-1) {
  #process every line in parallel with lapply
  print("#####################preProcess_parallel")
  
  import(c("stringi","parallel","snow"))  
  cluster <- makeCluster(no_cores)
  
  if(createPlot){
    svg('docs/plot_preProcess_parallel.svg')
    plot(snow.time(result <- parLapply(cluster,docs$text,stringi::stri_trans_general,id="Latin-ASCII")))
    dev.off()
  }else{
    result <- parLapply(cluster,docs$text,stringi::stri_trans_general,id="Latin-ASCII")  
  }
  
  stopCluster(cluster)
  return(result)
}

preProcessDoparallel <- function(createPlot=FALSE,no_cores=detectCores()-1) {
  #process every line sequentially with foreach
  print("#####################preProcess_doparallel")
  
  import(c("stringi","doParallel"))
  cluster <- makeCluster(no_cores)
  registerDoParallel(cluster)
  
  if(createPlot){
    png('docs/plot_preProcess_doparallel.png')
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
  
  stopCluster(cluster)
  return(result)
}

preProcessCluster <- function(createPlot=FALSE,no_cores = detectCores()-1) {
  #process every line in parallel with lapply
  print("#####################preProcess_cluster")
  
  import(c("stringi","parallel","snow"))  
  cluster <- makeCluster(no_cores)
  
  if(createPlot){
    svg('docs/plot_preProcess_cluster.svg')
    plot(snow.time(result <- clusterApply(cl = cluster,x=docs$text,stringi::stri_trans_general,id="Latin-ASCII")))
    dev.off()
  }else{
    result <- unlist(clusterApply(cl = cluster,x=docs$text,stringi::stri_trans_general,id="Latin-ASCII"))
  }
  
  stopCluster(cluster)
  return(result)
}

preProcessDoparallelChunked <- function(createPlot=FALSE,no_cores = detectCores()-1){
  #Devide descriptions into a number of chunks equal to the number of cores and process the chunks in parallel
  print("#####################preProcess_DevidedInChunks_doparallel")
  
  
  import(c("stringi","doParallel","doSNOW"))
  
  #split id's into chunks
  #https://code.i-harness.com/en/q/32a23d
  ids <- 1: length(docs$text)
  
  chunks <- split(ids,factor(sort(rank(ids)%%no_cores)))
  cluster <- makeCluster(no_cores,outfile="")
  registerDoSNOW(cluster)
    
  #process
  if(createPlot){
    png('docs/plot_preProcess_DevidedInChunks_doparallel.png')
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

preProcessParallelChunked <- function(createPlot=FALSE,no_cores=detectCores()-1){
  #Devide descriptions into a number of chunks equal to the number of cores and process the chunks in parallel
  print("#####################preProcess_DevidedInChunks_parallel")
  
  
  import(c("stringi","parallel","snow"))
  
  #split id's into chunks
  #https://code.i-harness.com/en/q/32a23d
  ids <- 1: length(docs$text)
  chunks <- split(ids,factor(sort(rank(ids)%%no_cores)))
  
  cluster <- makeCluster(no_cores,outfile="")
  
  if(createPlot){
    png('docs/plot_preProcess_DevidedInChunks_parallel.png')
    plot(snow.time(res <- parLapply(cluster,chunks,function(chunk,doc){stringi::stri_trans_general(doc$text[chunk], 'Latin-ASCII')},doc=docs)))
    dev.off()
  }else{
    res <- parLapply(cluster,chunks,function(chunk,doc){stringi::stri_trans_general(doc$text[chunk], 'Latin-ASCII')},doc=docs)
  }
  
  stopCluster(cluster)
  
  return(res)
}

preProcessClusterChunked <- function(createPlot=FALSE,no_cores=detectCores()-1) {
  #process every line in parallel with lapply
  
  import(c("stringi","parallel","snow"))
  print("Devide docs into chunks")
  chunks <- createChunksObjects(no_cores)
  print("Create Clusters")
  cluster <- makeCluster(no_cores,outfile="")
  
  
  if(createPlot){
    png('docs/plot_preProcess_devidedInChunks_cluster.svg')
    plot(snow.time(result <- unlist(clusterApply(cluster,
                                                 chunks,
                                                 function(chunk){
                                                   print("Creating chunk")
                                                   result <- stringi::stri_trans_general(chunk, 'Latin-ASCII')
                                                   print("preProcess chunk done")
                                                   return(result)
                                                 }))))
    dev.off()
  }else{ 
    
    ###################################################
    #
    #                 TODO 
    #     Check effect tussen export
    #     Check effect van mcapply
    #
    ###################################################
    
    result <- unlist(clusterApply(cluster,
                                  chunks,
                                  preProcessChunk)) #functie niet anoniem maken heeft een groot effect op ram gebruik. Ca 58% minder ram 
  }
  
  stopCluster(cluster)
  return(result)
}

preProcessChunk <- function(chunk){
  print("PreProcessing chunk")
  chunk <- stringi::stri_trans_general(chunk, 'Latin-ASCII')
  print("preProcess chunk done")
  return(chunk)
}

preProcessClusterChunked2 <- function(createPlot=FALSE,no_cores=detectCores()-1) {
  #process every line in parallel with lapply
  import(c("stringi","parallel","snow"))
  
  chunks <- createChunksIds(no_cores)
  cluster <- makeCluster(no_cores,outfile="")
  
  if(createPlot){
    png('docs/plot_preProcess_devidedInChunks_cluster.svg')
    plot(snow.time(result <- clusterApply(cluster,
                                          chunks,
                                          function(chunk,doc){
                                            stringi::stri_trans_general(doc$text[chunk], 'Latin-ASCII')},
                                          doc=docs)))
    dev.off()
  }else{
    result <- unlist(clusterApply(cluster,
                                  chunks,
                                  function(chunk,doc){
                                    stringi::stri_trans_general(doc[chunk], 'Latin-ASCII')},
                                  doc=docs))
  }
  
  stopCluster(cluster)
  return(result)
}

createChunksIds <- function(noChunks){
  ids <- 1: length(docs$text)
  split(ids,factor(sort(rank(ids)%%noChunks)))
}

createChunksObjects <- function(noChunks){
  docsList <- list()
  for(i in 1:noChunks){
    og <- round((i-1)*nrow(docs)/noChunks)+1
    bg <- round(nrow(docs)/noChunks*i)
    print(paste(og," --> ",bg))
    docsList[[i]] <- unlist(docs[og:bg,"text"])
  }
  
  return(docsList)
}


benchmarkPreProcess <- function(times = 1,display=TRUE,save=FALSE,createPlot=FALSE){
  import("microbenchmark")
  
  benchmarkResult <- microbenchmark(preProcessSequential(),
                                    preProcessParallel(createPlot=createPlot),
                                    preProcessParallelChunked(createPlot=createPlot),
                                    preProcessDoparallel(createPlot=createPlot),
                                    preProcessDoparallelChunked(createPlot=createPlot),
                                    preProcessCluster(createPlot = createPlot),
                                    preProcessClusterChunked(createPlot = createPlot),
                                    times=times)
  if(save){
    save(benchmarkResult,file="doc/preProcessBenchmarkResult.rda")
  }
  
  if(display){
    return(benchmarkResult)  
  }
}
