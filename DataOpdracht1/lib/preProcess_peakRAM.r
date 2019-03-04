# if (!exists("docs")){
#   load("docs.rds")
# }

import(c("peakRAM","stringi","parallel", "doSNOW" ))

list_to_df <- function(l){ return(as.data.frame(do.call(rbind, l))) }

no_cores <- detectCores()-1

### help functies
stringi_peakRAM <- function (x){
  t <- Sys.time()
  pram <- peakRAM(res <- stringi::stri_trans_general(x, id="Latin-ASCII"))
  list(cbind(Process_Id = Sys.getpid(), pram[,2:4], Start_Time = t, End_Time = Sys.time()), res)
}
get_pram_from_list <- function (l){
  pram <- l[[1]][[1]]
  for (i in 2:length(l)){ pram <- rbind(pram, l[[i]][[1]]) }
  return(pram)
}
get_result_as_vector_from_list <- function (l) {
  res <- NULL
  for (i in 2:length(l)){ res <- c(res, l[[i]][[2]]) }
  return(res)
}


### call functies
preProcessSequential_peakRAM <- function() {
  print("CHECK")
  return(stringi_peakRAM(docs$text)[[1]])
}

preProcessParallel_peakRAM <- function() {
  print("CHECK")
  
  cluster <- makeCluster(no_cores)
  clusterEvalQ(cluster, library("peakRAM"))
  result <- parLapply(cluster,docs$text,stringi_peakRAM)
  stopCluster(cluster)
  return(get_pram_from_list(result))
}

preProcessDoparallel_peakRAM <- function(createPlot=FALSE,no_cores=detectCores()-1) {
  cluster <- makeCluster(no_cores)
  clusterEvalQ(cluster, library("peakRAM"))
  clusterExport(cluster, c("stringi_peakRAM"))
  
  registerDoSNOW(cluster)
  result <- foreach(str = docs$text, .combine = rbind) %dopar% stringi_peakRAM(str)
  stopCluster(cluster)
  return(get_pram_from_list(result))
}

preProcessCluster_peakRAM <- function() {
  cluster <- makeCluster(no_cores)
  clusterEvalQ(cluster, library("peakRAM"))
  clusterExport(cluster, c("stringi_peakRAM"))
  
  result <- clusterApply(cl = cluster,x=docs$text,stringi_peakRAM)
  stopCluster(cluster)
  return(get_pram_from_list(result))
}

preProcessDoparallelChunked_peakRAM <- function(){
  cluster <- makeCluster(no_cores,outfile="")
  clusterEvalQ(cluster, library("peakRAM"))
  clusterExport(cluster, c("stringi_peakRAM"))
  registerDoSNOW(cluster)
  res <- foreach(chunk = createChunksObjects(no_cores)) %dopar% stringi_peakRAM(chunk)
  stopCluster(cluster)
  return(get_pram_from_list(res))
}

preProcessParallelChunked_peakRAM <- function(){
  cluster <- makeCluster(no_cores,outfile="")
  clusterEvalQ(cluster, library("peakRAM"))
  clusterExport(cluster, c("stringi_peakRAM"))
  res <- parLapply(cluster,createChunksObjects(no_cores),stringi_peakRAM)
  stopCluster(cluster)
  return(get_pram_from_list(res))
}

preProcessClusterChunked_peakRAM <- function() {
  cluster <- makeCluster(no_cores,outfile="")
  clusterEvalQ(cluster, library("peakRAM"))
  clusterExport(cluster, c("stringi_peakRAM"))
  result <- clusterApply(cluster,createChunksObjects(no_cores),stringi_peakRAM)
  stopCluster(cluster)
  return(get_pram_from_list(result))
}


#Hulpfuncties

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