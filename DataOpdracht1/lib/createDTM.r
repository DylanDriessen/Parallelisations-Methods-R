createDTM <- function(){
  # CREATE
  import(c("glmnet"))
  
  createDFM()
  
}

createDFM <- function(){
  # CREATE DFM
  print("create a DFM")
  dtm_rawQuan <- dfm(docsCorpus)
  #as dtm now
  tmdfm <- convert(dtm_rawQuan, to = "tm")
  dtm_raw <- tmdfm
  dtm_tfidf  <- weightTfIdf(dtm_raw, normalize = FALSE)
  return(rawQuanM)
}

createDTMC <- function(){
  # CREATE DTM (RAW AND WEIGHTED)
  print("create a DTM")
  dtm_ctrl <- list(tokenize = "words",
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
  save(dtm_raw, dtm_ctrl, file="dtm_raw.RDa")
  save(dtm_tfidf, dtm_ctrl, file="dtm_tfidf.RDa")
  
  return(dtm_tfidf)
}