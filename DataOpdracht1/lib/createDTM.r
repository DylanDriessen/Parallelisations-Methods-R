createDTM <- function() {
  # CREATE
  import(c("glmnet", "quanteda"))
  
  createDFMasDTM()
  #createDTMC()
  #createDFM()
}

#####################################################################
##
##             Document-Feature Matrix Parallel Chunks
##
#####################################################################

createDfmChunks <- function(){
  no_cores=detectCores()
  list <- list()
  for(i in 1:no_cores){
    og <- round((i-1)*nrow(docs)/no_cores)+1
    bg <- round(nrow(docs)/no_cores*i)
    list[[i]] = tokens_subset(docsCorpus, id >= og & id <= bg)
  }
  rm(docsCorpus)
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