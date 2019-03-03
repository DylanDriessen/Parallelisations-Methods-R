source("lib/createDTM.r")

makeCreateDTMCluster <- function() {
  cl <- makeCluster(no_cores, outfile = "")
  print("clusterEvalQ")
  clusterEvalQ(cl, { library("quanteda") })
  return(cl)
}

#####################################################################
##
##             Document-Feature Matrix Parallel Chunks
##
#####################################################################

createDfmChunks_peakRAM <- function() {
  print("createCluster")
  cl <- makeCreateDTMCluster()
  no_cores <- detectCores()
  registerDoParallel(cl)
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

createDFM_peakRAM <- function() {
  t <- Sys.time()
  df <- peakRAM(createDFM())
  cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
}

#####################################################################
##
##          Document-Feature Matrix to Document-Term Matrix
##
#####################################################################

createDFMasDTM <- function() {
  t <- Sys.time()
  df <- peakRAM(createDFMasDTM())
  cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
}

#####################################################################
##
##                     Document-Term Matrix
##
#####################################################################

createDTMC <- function() {
  t <- Sys.time()
  df <- peakRAM(createDTMC())
  cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
}