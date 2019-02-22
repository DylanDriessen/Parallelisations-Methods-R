deriveVoc <- function(){
  deriveVocabularyDFM()
}

deriveVocabularyDFM <- function() {
  # DERIVE VOCABULARY
  voc <-
    data.frame(
      trm = as.character(names(col_sums(DocumentTermMatrix))),
      cFrqs = col_sums(DocumentTermMatrix),
      #dFrqs = col_sums(weightBin(DocumentTermMatrix)),
      dFrqs = col_sums(dfm_weight(DocumentTermMatrix, scheme = "prop")),
      stringsAsFactors = FALSE
    )
  voc <- voc[with (voc, order(-cFrqs, -dFrqs, trm)), ]
  voc <- cbind(trm_id = seq(1, nrow(voc)), voc)
  rownames(voc) <- voc$trm_id
  return(voc)
}

deriveVocabulary <- function() {
  # DERIVE VOCABULARY QUAN
  voc2 <-
    data.frame(
      trm = as.character(names(col_sums(DocumentTermMatrix))),
      cFrqs = col_sums(DocumentTermMatrix),
      dFrqs = col_sums(weightBin(DocumentTermMatrix)),
      stringsAsFactors = FALSE
    )
  voc2 <- voc2[with (voc2, order(-cFrqs, -dFrqs, trm)), ]
  voc2 <- cbind(trm_id = seq(1, nrow(voc2)), voc2)
  rownames(voc2) <- voc2$trm_id
  return(voc2)
}