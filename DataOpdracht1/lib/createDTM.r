createDFM <- function() {
  # CREATE
  #createDFMasDTM()
  #createDFMChunks()
  createDFMnormal()
  #createDFMChunksBind()
  
  # docrows <- nrow(docs)
  # rm(docs)
}

createDTM <- function() {
  # CREATE
  createDTM()
  #createDTMChunked()
}

makeCreateDFMCluster <- function() {
  # no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterEvalQ(cl, { library("quanteda") })
  return(cl)
}

#####################################################################
##
##             Document-Feature Matrix Parallel Chunks
##
#####################################################################
##HEEL FOUT
createDFMChunks <- function() {
  cl <- makeCreateDFMCluster()
  # no_cores <- detectCores()
  registerDoParallel(cl)
  dfmList <- list()
  docrows <- length(docsCorpusQuan)
  dc <- docsCorpusQuan
  dfmList <-
    foreach(i = 1:no_cores, .export = "no_cores") %dopar% {
      og <- round((i - 1) * docrows / no_cores) + 1
      print(paste(no_cores, docrows))
      bg <- round(docrows / no_cores * i)
      print(paste(og, bg))
      sub <- tokens_subset(dc, id >= og & id <= bg)
      dfm(sub)
    }
  
  stopCluster(cl)
  dfmTotal <- dfmList[[1]]

  for (i in 2:length(dfmList)) {
    dfmTotal <- rbind(dfmTotal, dfmList[[i]])
  }
  
  tryCatch(
    {
      print("try1")
      dfmTotal <- dfmTotal[rowSums(dfmTotal[,-1]) != 0,]
      print("try")
      return(dfmTotal)
    },
    error = function(condition) {
      print("catch")
      return(dfmTotal)
    }
  )
}

#####################################################################
##
##          Document-Feature Matrix Parallel Chunks RBIND
##
#####################################################################


createDFMChunksBind <- function() {
  cl <- makeCreateDFMCluster()
  # no_cores <- detectCores() - 1
  registerDoParallel(cl)
  dfmList <- list()
  docrows <- nrow(docsCorpusQuan)
  dc <- docsCorpusQuan
  dfmList <-
    foreach(i = 1:no_cores*2, .combine = rbind, .export = "no_cores") %dopar% {
      og <- round((i - 1) * docrows / no_cores) + 1
      print(paste(no_cores, docrows))
      bg <- round(docrows / no_cores * i)
      print(paste(og, bg))
      sub <- tokens_subset(dc, id >= og & id <= bg)
      dfm(sub)
    }
  
  stopCluster(cl)
  tryCatch(
    {
      print("try1")
      dfmList <- dfmList[rowSums(dfmList[,-1]) != 0,]
      print("try")
      return(dfmList)
    },
    error = function(condition) {
      print("catch")
      return(dfmList)
    }
  )
}

#####################################################################
##
##                    Document-Feature Matrix
##
#####################################################################

createDFMnormal <- function() {
  dtm_raw <- dfm(docsCorpusQuan)
  rowSums(dtm_raw, na.rm = FALSE)
  # dtm_tfidf  <- dfm_weight(dtm_raw)
  tryCatch(
    {
      print("try1")
      dtm_raw <- dtm_raw[rowSums(dtm_raw[,-1]) != 0,]
      print("try")
      return(dtm_raw)
    },
    error = function(condition) {
    print("catch")
    return(dtm_raw)
    }
  )
}

#####################################################################
##
##          Document-Feature Matrix to Document-Term Matrix
##
#####################################################################

createDFMasDTM <- function() {
  dtm_raw <- dfm(docsCorpusQuan)
  tryCatch(
    {
      print("try1")
      dtm_raw <- dtm_raw[rowSums(dtm_raw[,-1]) != 0,]
      print("try")
      return(dtm_raw)
    },
    error = function(condition) {
      print("catch")
      return(dtm_raw)
    }
  )
  dtm_raw <- convert(dtm_raw, to = "tm")
  # dtm_tfidf  <- weightTfIdf(dtm_raw, normalize = FALSE)
  # 
  return(dtm_raw)
}

#####################################################################
##
##                     Document-Term Matrix
##
#####################################################################

createDTM <- function() {
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
  dtm_raw <- DocumentTermMatrix(docsCorpus, control = dtm_ctrl)
  #dtm_tfidf  <- weightTfIdf(dtm_raw, normalize = FALSE)
  
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

createDTMChunked <- function() {
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