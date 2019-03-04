createDTM <- function() {
  # CREATE
  createDFMasDTM()
  #createDfmChunks()
  #createDTMC()
  #createDTMCChunked()
  #createDFM()
  #createDfmChunksBind()
}

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

createDfmChunks <- function() {
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
      print("in foreach loop")
      og <- round((i - 1) * docrows / no_cores) + 1
      print(paste(no_cores, docrows))
      bg <- round(docrows / no_cores * i)
      print(paste(og, bg))
      sub <- tokens_subset(dc, id >= og & id <= bg)
      dfm(sub)
    }
  
  stopCluster(cl)
  print("cluster stopt")
  
  print("remove big Corpus")
  #rm(docsCorpus)
  
  print("write first dfm to total")
  dfmTotal <- dfmList[[1]]
  
  print("binding DFM's")
  for (i in 2:length(dfmList)) {
    dfmTotal <- rbind(dfmTotal, dfmList[[i]])
  }
  print("done binding")
  return(dfmTotal)
  print("returnd result")
  
}

#####################################################################
##
##          Document-Feature Matrix Parallel Chunks RBIND
##
#####################################################################


createDfmChunksBind <- function() {
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
    foreach(i = 1:no_cores, .combine = rbind) %dopar% {
      print("in foreach loop")
      og <- round((i - 1) * docrows / no_cores) + 1
      print(paste(no_cores, docrows))
      bg <- round(docrows / no_cores * i)
      print(paste(og, bg))
      sub <- tokens_subset(dc, id >= og & id <= bg)
      dfm(sub)
    }
  
  stopCluster(cl)
  print("cluster stopt")
  
  print("remove big Corpus")
  #rm(docsCorpus)
  
  return(dfmList)
  print("returnd result")
  
}

#####################################################################
##
##                    Document-Feature Matrix
##
#####################################################################

createDFM <- function() {
  # CREATE DFM
  print("create a DFM")
  dtm_raw <- dfm(docsCorpus)
  rowSums(dtm_raw, na.rm = FALSE)
  # dtm_tfidf  <- dfm_weight(dtm_raw)
  dtm_raw <- dtm_raw[rowSums(dtm_raw[,-1]) != 0,]
  return(dtm_raw)
}

#####################################################################
##
##          Document-Feature Matrix to Document-Term Matrix
##
#####################################################################

createDFMasDTM <- function() {
  # CREATE DFM
  print("create a DFM")
  dtm_raw <- dfm(docsCorpus)
  #as dtm now
  print("convert to DTM")
  dtm_raw <- convert(dtm_raw, to = "tm")
  dtm_tfidf  <- weightTfIdf(dtm_raw, normalize = FALSE)
  return(dtm_raw)
}

#####################################################################
##
##                     Document-Term Matrix
##
#####################################################################

createDTMC <- function() {
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
  print("dtm_raw")
  dtm_raw <- DocumentTermMatrix(docsCorpus, control = dtm_ctrl)
  #dtm_tfidf  <- weightTfIdf(dtm_raw, normalize = FALSE)
  #dtm <- as.matrix(dtm_raw[1:50,1:50])
  
  # SAVE RESULTS
  #save(dtm_raw, dtm_ctrl, file = "dtm_raw.RDa")
  #save(dtm_tfidf, dtm_ctrl, file = "dtm_tfidf.RDa")
  
  return(dtm_raw)
}

#####################################################################
##
##                     Document-Term Matrix Chunked
##
#####################################################################

createDTMCChunked <- function() {
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
  registerDoParallel(cluster)
  
  dtmList <- 
    foreach(chunk = chunks,
            .packages = "tm") %dopar% {
              DocumentTermMatrix(chunk,control=dtm_ctrl)
            }
  
  stopCluster(cluster)
  
  dtm <- do.call(tm:::c.DocumentTermMatrix,dtmList)
  
  return(dtm)
}

createCorpusChunks <- function(no_chunks){
  corpusLenght <- length(docsCorpus)
  
  return(foreach(i=1:no_chunks ) %do% {
            og <- round((i -1) * corpusLenght / no_chunks) + 1
            bg <- round(corpusLenght / no_chunks * i) 
            print(paste0(og," ---> ",bg))
            docsCorpus[og:bg]
          })
  
}