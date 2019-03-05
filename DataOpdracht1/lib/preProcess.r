# if (!exists("docs")){
#   load("docs.rds")
# }

preProcess <- function(){
  return(preProcessClusterChunked())
}

preProcessSequential <- function() {
  #process every line sequentially
  #print("#####################preProcess_seq")
  return(preProcessChunk(docs$text))
}

preProcessParallel <- function() {
  #process every line in parallel with lapply
  #print("#####################preProcess_parallel")
  cluster <- makeCluster(no_cores)
  result <- parLapply(cluster,docs$text,preProcessChunk)
  stopCluster(cluster)
  return(unlist(result))
}

preProcessDoparallel <- function() {
  #process every line sequentially with foreach
  #print("#####################preProcess_doparallel")
  cluster <- makeCluster(no_cores)
  registerDoParallel(cluster)
  result <- foreach(doc = docs$text, 
                    .combine = c,
                    .export="preProcessChunk") %dopar%
    preProcessChunk(doc)
  stopCluster(cluster)
  return(result)
}

preProcessCluster <- function() {
  #process every line in parallel with lapply
  #print("#####################preProcess_cluster")
  cluster <- makeCluster(no_cores)
  result <- clusterApply(cl = cluster,x=docs$text,preProcessChunk)
  stopCluster(cluster)
  return(unlist(result))
}

preProcessDoparallelChunked <- function(){
  #Devide descriptions into a number of chunks equal to the number of cores and process the chunks in parallel
  #print("#####################preProcess_DevidedInChunks_doparallel")
  cluster <- makeCluster(no_cores)
  registerDoSNOW(cluster)
  res <- foreach(chunk = createChunksObjects(no_cores), .combine = c,.export = "preProcessChunk") %dopar% 
    preProcessChunk(chunk)
  stopCluster(cluster)
  return(res)
}

preProcessParallelChunked <- function(){
  #Devide descriptions into a number of chunks equal to the number of cores and process the chunks in parallel
  #print("#####################preProcess_DevidedInChunks_parallel")
  
  cluster <- makeCluster(no_cores)
  res <- parLapply(cluster,createChunksObjects(no_cores),preProcessChunk)
  stopCluster(cluster)
  return(unlist(res))
}


preProcessClusterChunked <- function() {
  ###################################################
  #
  #                 TODO 
  #     Check effect tussen export
  #     Check effect van mcapply
  #
  ###################################################
  #print("preProcess_DevidedInChunks_cluster")
  cluster <- makeCluster(no_cores)
  result <- unlist(clusterApply(cluster, createChunksObjects(no_cores),preProcessChunk)) #Named function gebruikt ca 58% minder ram
  stopCluster(cluster)
  return(result)
}

preProcessChunk <- function(chunk){
  chunk <- stringi::stri_trans_general(chunk, 'Latin-ASCII')
  return(chunk)
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
    #print(paste(og," --> ",bg))
    docsList[[i]] <- unlist(docs[og:bg,"text"])
  }
  
  return(docsList)
}


benchmarkPreProcess <- function(times = 1,display=TRUE,save=TRUE){
  
  benchmarkResult <- microbenchmark(preProcessSequential(),
                                    #preProcessParallel(),
                                    preProcessParallelChunked(),
                                    #preProcessDoparallel(),
                                    preProcessDoparallelChunked(),
                                    #preProcessCluster(),
                                    preProcessClusterChunked(),
                                    times=times)
  if(save){save(benchmarkResult,file="result.Rda")}
  if(display){return(benchmarkResult)  
  }
}
