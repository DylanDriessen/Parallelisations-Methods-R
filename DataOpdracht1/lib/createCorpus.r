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
  
  Quan()
}

#####################################################################
##
##                           TM Package Parallel
##
#####################################################################

VCorpChunk <- function() {
  print("Creating VCorpus Chunk")
  crp <-
    VCorpus(DataframeSource(docs), readerControl = list(language = "en"))
  
  ##### Define general function to replace strings in corpus
  print("Define general function to replace strings in corpus")
  (crp.replacePattern <-
      content_transformer(function(x, pattern, replace)
        gsub(pattern, replace, x)))
  
  ##### Clean unicode characters
  ##### Remove graphical characters
  print("Remove graphical characters")
  ids <- 1: length(crp)
  no_cores = detectCores()
  chunks <- split(ids,factor(sort(rank(ids)%%no_cores)))
  
  registerDoParallel(no_cores)
  crp <- foreach(chunk = chunks,
                 .combine = c) %dopar%
    tm_map(crp[chunk], crp.replacePattern, "[^[:graph:]]", " ")
  
  #stopImplicitCluster()
  
  ##### To lower
  print("To lower")
  #registerDoParallel(no_cores)
  crp <- foreach(chunk = chunks,
                 .combine = c) %dopar%
    tm_map(crp[chunk], content_transformer(tolower))
  
  #stopImplicitCluster()
  
  ##### Stopword removal
  print("Stopword removal")
  #registerDoParallel(no_cores)
  crp <- foreach(chunk = chunks,
                 .combine = c) %dopar%
    tm_map(crp[chunk], removeWords, stopwords(source = "snowball"))
  
  #stopImplicitCluster()
  
  ##### Stemming
  print("Stemming")
  #registerDoParallel(no_cores)
  crp <- foreach(chunk = chunks,
                 .combine = c) %dopar%
    tm_map(crp[chunk], stemDocument, language = "porter")
  
  #stopImplicitCluster()
  
  ##### Numbers
  
  ##### All numbers (including numbers as part of a alphanumerical term)
  print("Removing all numbers")
  #registerDoParallel(no_cores)
  crp <- foreach(chunk = chunks,
                 .combine = c) %dopar%
    tm_map(crp[chunk], removeNumbers)
  
  #stopImplicitCluster()
  
  ##### Punctuation
  print("remove Puncuation")
  #registerDoParallel(no_cores)
  crp <- foreach(chunk = chunks,
                 .combine = c) %dopar%
    tm_map(crp[chunk], removePunctuation, preserve_intra_word_dashes = TRUE)
  
  #stopImplicitCluster()

  ##### Whitespace
  print("Remove whitespace")
  #registerDoParallel(no_cores)
  crp <- foreach(chunk = chunks,
                 .combine = c) %dopar%
    tm_map(crp[chunk], stripWhitespace)
  
  stopImplicitCluster()
  
  # SAVE RESULTS
  print("Saving results")
  save(crp, file = "crp.RDa")
  return(crp)
}


#####################################################################
##
##                           TM Package
##
#####################################################################

VCorp <- function() {
  print("Creating VCorpus")
  crp <-
    VCorpus(DataframeSource(docs), readerControl = list(language = "en"))
  
  ##### Define general function to replace strings in corpus
  print("Define general function to replace strings in corpus")
  (crp.replacePattern <-
      content_transformer(function(x, pattern, replace)
        gsub(pattern, replace, x)))
  
  ##### Clean unicode characters
  ##### Remove graphical characters
  print("Remove graphical characters")
  crp <- tm_map(crp, crp.replacePattern, "[^[:graph:]]", " ")
  
  ##### To lower
  print("To lower")
  crp <- tm_map(crp, content_transformer(tolower))
  
  ##### Stopword removal
  crp <- tm_map(crp, removeWords, stopwords(source = "smart"))
  
  ##### Stemming
  print("Stemming")
  crp <- tm_map(crp, stemDocument, language = "porter")
  
  ##### Numbers
  
  ##### All numbers (including numbers as part of a alphanumerical term)
  print("Removing all numbers")
  crp <- tm_map(crp, removeNumbers)
  
  ##### Punctuation
  print("remove Puncuation")
  crp <-
    tm_map(crp, removePunctuation, preserve_intra_word_dashes = TRUE)
  
  ##### Whitespace
  print("Remove whitespace")
  crp <- tm_map(crp, stripWhitespace)
  
  # SAVE RESULTS
  print("Saving results")
  save(crp, file = "crp.RDa")
  return(crp)
}

#####################################################################
##
##                            Quanteda
##
#####################################################################
#https://cran.r-project.org/web/packages/quanteda/quanteda.pdf

Quan <- function() {
  print("Creating Quanteda Corpus")
  crpT <- corpus(docs)
  
  #Quanteda tokens
  print("Creating tokens and removing punctuation")
  crpT <- tokens(crpT, remove_punct = TRUE)
  
  ##### Clean unicode characters
  ##### Remove graphical characters
  print("Remove graphical characters")
  crpT <- tokens_replace(crpT, "[^[:graph:]]", " ")
  
  ##### To lower
  print("To lower")
  crpT <- tokens_tolower(crpT)
  
  ##### Stopword removal
  print("Stopword removal")
  crpT <- tokens_remove(crpT, stopwords(source = "smart"))
  
  ##### Stemming
  print("Stemming")
  crpT <- tokens_wordstem(crpT, language = "porter")
  
  ##### Numbers
  
  ##### All numbers (including numbers as part of a alphanumerical term) + punction + symbols
  print("Removing all numbers and symbols")
  crpT <-
    tokens(crpT,
           remove_numbers = TRUE,
           remove_symbols = TRUE)
  
  # SAVE RESULTS
  print("Saving")
  save(crpT, file = "crpT.RDa")
  print("Return result")
  return(crpT)
}




