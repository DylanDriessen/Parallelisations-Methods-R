# if (!exists("docs")){
#   load("docs.rds")
# }

import(c("peakRAM","stringi","parallel", "doParallel"))
list_to_df <- function(l){ return(as.data.frame(do.call(rbind, l))) }
no_cores <- detectCores()-1

preProcessSequential_peakRAM <- function() {
  return(peakRAM(stringi::stri_trans_general(docs$text, 'Latin-ASCII')))
}

preProcessParallel_peakRAM <- function() {
  cluster <- makeCluster(no_cores)
  clusterEvalQ(cluster, library("peakRAM"))
  result <- parLapply(cluster,docs$text,function(x) { peakRAM(stringi::stri_trans_general(x, id="Latin-ASCII")) })
  stopCluster(cluster)
  return(list_to_df(result))
}

preProcessDoparallel_peakRAM <- function(createPlot=FALSE,no_cores=detectCores()-1) {
  cluster <- makeCluster(no_cores)
  clusterEvalQ(cluster, library("peakRAM"))
  registerDoParallel(cluster)
  result <- foreach(str = docs$text, .combine = rbind) %dopar%
      peakRAM(stringi::stri_trans_general(str=str,id="Latin-ASCII"))
  stopCluster(cluster)
  return(result)
}

preProcessCluster_peakRAM <- function() {
  cluster <- makeCluster(no_cores)
  clusterEvalQ(cluster, library("peakRAM"))
  result <- clusterApply(cl = cluster,x=docs$text,function(x) { peakRAM(stringi::stri_trans_general(x, id="Latin-ASCII"))})
  stopCluster(cluster)
  return(list_to_df(result))
}

preProcessDoparallelChunked <- function(){
  ids <- 1: length(docs$text)
  chunks <- split(ids,factor(sort(rank(ids)%%no_cores)))
  cluster <- makeCluster(no_cores,outfile="")
  clusterEvalQ(cluster, library("peakRAM"))
  registerDoParallel(cluster)
  res <- foreach(chunk = chunks,
                  .combine = rbind,
                  .export = "docs") %dopar%
    peakRAM(stringi::stri_trans_general(docs$text[chunk], 'Latin-ASCII'))
  stopCluster(cluster)
  return(res)
}

preProcessParallelChunked <- function(){
  ids <- 1: length(docs$text)
  chunks <- split(ids,factor(sort(rank(ids)%%no_cores)))
  cluster <- makeCluster(no_cores,outfile="")
  clusterEvalQ(cluster, library("peakRAM"))
  res <- parLapply(cluster,chunks,function(chunk,doc){peakRAM(stringi::stri_trans_general(doc$text[chunk], 'Latin-ASCII'))},doc=docs)
  stopCluster(cluster)
  return(list_to_df(res))
}

preProcessClusterChunked <- function() {
  chunks <- createChunksObjects(no_cores)
  cluster <- makeCluster(no_cores,outfile="")
  clusterEvalQ(cluster, library("peakRAM"))
    result <- clusterApply(cluster,
                                  chunks,
                                  function(chunk){
                                    peakRAM(stringi::stri_trans_general(chunk, 'Latin-ASCII'))
                                  })
  stopCluster(cluster)
  return(list_to_df(result))
}

preProcessClusterChunked2 <- function() {
  chunks <- createChunksIds(no_cores)
  cluster <- makeCluster(no_cores,outfile="")
  clusterEvalQ(cluster, library("peakRAM"))
    result <- clusterApply(cluster,
                                  chunks,
                                  function(chunk,doc){
                                    peakRAM(stringi::stri_trans_general(doc[chunk], 'Latin-ASCII'))
                                    }, doc=docs)
  stopCluster(cluster)
  return(list_to_df(result))
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
