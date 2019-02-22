deriveVoc <- function() {
  deriveVocabularyDFM()
}

#####################################################################
##
##                    Derive Vocabulary On DFM
##
#####################################################################

deriveVocabularyDFM <- function() {
  # DERIVE VOCABULARY QUAN
  voc <-
    data.frame(
      trm = as.character(names(col_sums(
        DocumentTermMatrix
      ))),
      cFrqs = col_sums(DocumentTermMatrix),
      #dFrqs = col_sums(weightBin(DocumentTermMatrix)),
      dFrqs = col_sums(dfm_weight(DocumentTermMatrix, scheme = "prop")),
      stringsAsFactors = FALSE
    )
  voc <- voc[with (voc, order(-cFrqs,-dFrqs, trm)),]
  voc <- cbind(trm_id = seq(1, nrow(voc)), voc)
  rownames(voc) <- voc$trm_id
  return(voc)
}

#####################################################################
##
##                    Derive Vocabulary On DTM
##
#####################################################################

deriveVocabulary <- function() {
  # DERIVE VOCABULARY
  voc <-
    data.frame(
      trm = as.character(names(col_sums(
        DocumentTermMatrix
      ))),
      cFrqs = col_sums(DocumentTermMatrix),
      dFrqs = col_sums(weightBin(DocumentTermMatrix)),
      stringsAsFactors = FALSE
    )
  voc <- voc2[with (voc2, order(-cFrqs,-dFrqs, trm)),]
  voc <- cbind(trm_id = seq(1, nrow(voc)), voc)
  rownames(voc) <- voc$trm_id
  return(voc)
}