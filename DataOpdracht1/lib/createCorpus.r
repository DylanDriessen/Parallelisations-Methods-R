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
## TM Package
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
  print("Stopword removal")
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
## Quanteda
##
#####################################################################

Quan <- function() {
  print("Creating Quanteda Corpus")
  crp2 <- corpus(docs)
  
  #Quanteda tokens
  print("Creating tokens and removing punctuation")
  crpT <- tokens(crp2, remove_punct = TRUE)
  
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


#microbenchmark(Quan(), VCorp(), times = 1)

