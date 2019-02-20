deriveVocabulary <- function() {
  # DERIVE VOCABULARY
  voc <-
    data.frame(
      trm = as.character(names(col_sums(dtm_raw))),
      cFrqs = col_sums(dtm_raw),
      dFrqs = col_sums(weightBin(dtm_raw)),
      stringsAsFactors = FALSE
    )
  voc <- voc[with (voc, order(-cFrqs, -dFrqs, trm)), ]
  voc <- cbind(trm_id = seq(1, nrow(voc)), voc)
  rownames(voc) <- voc$trm_id
}