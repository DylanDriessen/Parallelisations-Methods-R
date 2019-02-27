createDTM <- function() {
  # CREATE
  import(c("glmnet", "quanteda"))
  
  createDFMasDTM()
  #createDfmChunks()
  #createDTMC()
  #createDFM()
}

#####################################################################
##
##             Document-Feature Matrix Parallel Chunks
##
#####################################################################

createDfmChunks <- function() {
  print("detectCores")
  no_cores = detectCores()
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  print("create List")
  dfmList <- list()
  print("checking limits & writing dfm's to list")
  docrows <- nrow(docs)
  print("test")
  dfmList <-
    foreach(i = 1:no_cores, .packages = "quanteda", .export = c("docs", "docsCorpus")) %dopar% {
      og <- round((i - 1) * docrows / no_cores) + 1
      bg <- round(docrows / no_cores * i)
      sub <- tokens_subset(docsCorpus, id >= og & id <= bg)
      dfm(sub)
    }
  print("remove big Corpus")
  #rm(docsCorpus)
  
  print("write first dfm to total")
  dfmTotal <- dfmList[[1]]
  
  print("binding DFM's")
  for (i in 2:length(dfmList)) {
    dfmTotal <- rbind(dfmTotal, dfmList[[i]])
  }
  
  stopCluster(cl)
  
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
  #dtm_tfidf  <- weightTfIdf(dtm_raw, normalize = FALSE)
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
  dtm_raw <- DocumentTermMatrix(docsCorpus, control = dtm_ctrl)
  dtm_tfidf  <- weightTfIdf(dtm_raw, normalize = FALSE)
  #dtm <- as.matrix(dtm_raw[1:50,1:50])
  
  # SAVE RESULTS
  save(dtm_raw, dtm_ctrl, file = "dtm_raw.RDa")
  save(dtm_tfidf, dtm_ctrl, file = "dtm_tfidf.RDa")
  
  return(dtm_raw)
}