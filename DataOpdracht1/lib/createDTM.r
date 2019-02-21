createDTM <- function(){
  # CREATE DTM (RAW AND WEIGHTED)
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
  
  
  dtm_raw    <- DocumentTermMatrix(crp, control = dtm_ctrl)
  dtm <- as.matrix(dtm_raw[1:50,1:50])
  dtm_rawQuan <- dfm(crpT)
  rawQuanM <- as.matrix(dtm_rawQuan)
  dtm_tfidf  <- weightTfIdf(dtm_raw, normalize = FALSE)
  
  
  # SAVE RESULTS
  save(dtm_raw, dtm_ctrl, file="dtm_raw.RDa")
  save(dtm_tfidf, dtm_ctrl, file="dtm_tfidf.RDa")
}