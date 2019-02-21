createCorpus <- function() {
  import(
    c(
      "tm",
      "SnowballC",
      "slam",
      "stringi",
      "wordcloud",
      "corrplot",
      "NLP",
      "foreach",
      "doParallel",
      "microbenchmark",
      "text2vec",
      "doMC",
      "quanteda",
      "textmineR"
    )
  )
  
  ##### Create corpus (and define default language)
  
  VCorp()
}

#####################################################################
##
## TM Package
##
#####################################################################

VCorp <- function() {
  crp <-
    VCorpus(DataframeSource(docs), readerControl = list(language = "en"))
  
  
  ##### Set metadata based on fields in source data
  
  # seqClus <- function() {
  #   for (i in 1:length(crp2)) {
  #     metadoc(crp2, i, "language") <- docs$language[i]
  #   }
  # }
  # 
  # parClus <- function() {
  #   no_cores <- detectCores()
  #   #cl <- makeCluster(no_cores)
  #   registerDoMC(no_cores)
  #   foreach(
  #     i = 1:length(crp),
  #     .export = c("docs", "crp"),
  #     .packages = c("tm")
  #   ) %dopar% {
  #     meta(crp[[i]], "language") <- docs$language[i]
  #   }
  #   #stopCluster(cl)
  # }
  
  ##### Define general function to replace strings in corpus
  (crp.replacePattern <-
      content_transformer(function(x, pattern, replace)
        gsub(pattern, replace, x)))
  
  ##### Clean unicode characters
  ##### Remove graphical characters
  crp <- tm_map(crp, crp.replacePattern, "[^[:graph:]]", " ")
  
  ##### To lower
  crp <- tm_map(crp, content_transformer(tolower))
  
  ##### Stopword removal
  crp <- tm_map(crp, removeWords, stopwords(source ="smart"))
  
  ##### Stemming
  crp <- tm_map(crp, stemDocument, language = "porter")
  
  ##### Numbers
  
  ##### All numbers (including numbers as part of a alphanumerical term)
  crp <- tm_map(crp, removeNumbers)
  
  ##### Punctuation
  crp <-
    tm_map(crp, removePunctuation, preserve_intra_word_dashes = TRUE)
  
  ##### Whitespace
  crp <- tm_map(crp, stripWhitespace)
  
  # SAVE RESULTS
  save(crp, file = "crp.RDa")
}

#####################################################################
##
## Quanteda
##
#####################################################################

Quan <- function() {
  crp2 <- corpus(docs)
  
  #Quanteda tokens
  crpT <- tokens(crp2, remove_punct = TRUE)
  
  #metadoc(crp2, "language") <- "english"
  
  ##### Clean unicode characters
  ##### Remove graphical characters
  crpT <- tokens_replace(crpT, "[^[:graph:]]", " ")
  
  ##### To lower
  crpT <- tokens_tolower(crpT)
  
  ##### Stopword removal
  crpT <- tokens_remove(crpT, stopwords(source = "smart"))
  
  ##### Stemming
  crpT <- tokens_wordstem(crpT, language = "porter")
  
  ##### Numbers
  
  ##### All numbers (including numbers as part of a alphanumerical term) + punction + symbols
  crpT <-
    tokens(
      crpT,
      remove_numbers = TRUE,
      remove_symbols = TRUE
    )
  
  # SAVE RESULTS
  save(crpT, file = "crpT.RDa")
}


microbenchmark(Quan(), VCorp(), times = 1)

