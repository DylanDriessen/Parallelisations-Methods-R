################################################################################
#
# 1 COMPILE DOCUMENT COLLECTION
#
# 18/02/2019 Tom Magerman
#
################################################################################

compileDocumentCollectionSeq <- function(){
  appln_id <- c(1,"qwe","be")
  appln_abstract <- c(1,"qwe","be")
  appln_abstract_lg <- c(1,"qwe","be")
  l <- data.frame(appln_id, appln_abstract, appln_abstract_lg)
  l
  
  #for (l in ps18b_abstr)
  docs <- l[,c("appln_id", "appln_abstract", "appln_abstract_lg")]
  names(docs) <- c("doc_id", "text", "language")
  str(docs)
}

