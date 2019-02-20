createCorpus <- function(){
  import(c("tm","SnowballC","slam","stringi","wordcloud", "corrplot", "NLP"))
  
  # Create corpus (and define default language)
  
  crp <- VCorpus(DataframeSource(docs), readerControl=list(language="en"))
  
  # Set metadata based on fields in source data
  
  for(i in 1:length(crp)) {
    meta(crp[[i]], "language") <- docs$language[i]
  }
  
  # Define general function to replace strings in corpus
  (crp.replacePattern <- content_transformer(function(x, pattern, replace) gsub(pattern, replace, x)))
  
  ## Clean unicode characters
  
  # Remove graphical characters
  crp <- tm_map(crp, crp.replacePattern, "[^[:graph:]]", " ")
  
  # To lower
  crp <- tm_map(crp, content_transformer(tolower))
  
  # Stopword removal
  crp <- tm_map(crp, removeWords, stopwords("SMART"))
  
  # Stemming
  crp <- tm_map(crp, stemDocument, language = "porter")
  
  # Numbers
  
  # All numbers (including numbers as part of a alphanumerical term)
  crp <- tm_map(crp, removeNumbers)
  
  # Punctuation
  crp <- tm_map(crp, removePunctuation, preserve_intra_word_dashes = TRUE)
  
  # Whitespace
  crp <- tm_map(crp, stripWhitespace)
  
  # SAVE RESULTS
  save(crp, file="crp.RDa")
}