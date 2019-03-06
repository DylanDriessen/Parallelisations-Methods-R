source("lib/createDTM.r")

#####################################################################
##
##             Document-Feature Matrix Parallel Chunks
##
#####################################################################

createDfmChunks_peakRAM <- function() {
  print("createCluster")
  cl <- makeCreateDTMCluster()
  clusterEvalQ(cl, library("peakRAM"))
  clusterExport(cl, c("no_cores"))
  registerDoSNOW(cl)
  print("create List")
  dfmList <- list()
  print("checking limits & writing dfm's to list")
  docrows <- nrow(docs)
  dc <- docsCorpus
  print("test")
  dfmList <-
    foreach(i = 1:no_cores) %dopar% {
      t <- Sys.time()
      pram <- peakRAM(res <- {
      og <- round((i - 1) * docrows / no_cores) + 1
      bg <- round(docrows / no_cores * i)
      sub <- tokens_subset(dc, id >= og & id <= bg)
      dfm(sub)
      })
      list(cbind(Process_Id = Sys.getpid(), pram[,2:4], Start_Time = t, End_Time = Sys.time()), res)
    }
  
  stopCluster(cl)
  
  #get peakram from result list
  pram <- dfmList[[1]][[1]]
  for (i in 2:length(dfmList)){
    pram <- rbind(pram, dfmList[[i]][[1]])
  }
  
  #get dfm from result list
  dfmTotal <- dfmList[[1]][[2]]
  for (i in 2:length(dfmList)) {
    dfmTotal <- rbind(dfmTotal, dfmList[[i]][[2]])
  }
  return(pram)
}

#####################################################################
##
##                    Document-Feature Matrix
##
#####################################################################

createDFMnormal_peakRAM <- function() {
  t <- Sys.time()
  df <- peakRAM(createDFMnormal())
  cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
}

#####################################################################
##
##          Document-Feature Matrix to Document-Term Matrix
##
#####################################################################

createDFMasDTM_peakRAM <- function() {
  t <- Sys.time()
  df <- peakRAM(createDFMasDTM())
  cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
}


#####################################################################
##
##                     Document-Term Matrix
##
#####################################################################

createDTM_peakRAM <- function() {
  t <- Sys.time()
  df <- peakRAM(createDTM())
  cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
}

#####################################################################
##
##                     Document-Term Matrix Chunked
##
#####################################################################

createDTMChunked_peakRAM <- function() {
  # CREATE DTM (RAW AND WEIGHTED)
  
  print("create a DTM")
  dtm_ctrl <- list(
    tokenize = "words",
    tolower = FALSE,
    removePunctuation = FALSE,
    removeNumbers = FALSE,
    stopwords = FALSE,
    stemming = FALSE,
    dictionary = NULL,
    bounds = list(global = c(1, Inf)),
    weighting = weightTf,
    wordLengths = c(1, Inf)
  )
  
  chunks <- createCorpusChunks(no_chunks = no_cores)
  
  cluster <- makeCluster(no_cores,outfile="")
  clusterEvalQ(cluster, library("peakRAM"))
  registerDoSnow(cluster)
  
  dtmpramList <- 
    foreach(chunk = chunks,
            .packages = "tm") %dopar% {
              t <- Sys.time()
              pram <- peakRAM(res <-DocumentTermMatrix(chunk,control=dtm_ctrl))
              list(cbind(Process_Id = Sys.getpid(), pram[,2:4], Start_Time = t, End_Time = Sys.time()), res)
            }
  stopCluster(cluster)
  
  #get peakram from result list
  pram <- dtmpramList[[1]][[1]]
  for (i in 2:length(dtmpramList))
    pram <- rbind(pram, dtmpramList[[i]][[1]])
  
  #get dfm from result list
  dtmList <- list()
  for (i in 1:length(dtmpramList)) 
    dtmList[[i]] <- dtmpramList[[i]][[2]]
  
  dtm <- do.call(tm:::c.DocumentTermMatrix,dtmList)
  
  return(pram)
}
