createCorpus <- function() {
  
  ##### Create corpus (and define default language)
  
  #Quan()
  #VCorpChunk()
  #VCorp()
  VCorpChunk1Loop()
}

createCorpusCluster <- function() {
  cl <- makeCluster(7, outfile = "")
  print("clusterEvalQ")
  clusterEvalQ(cl, { library("tm") })
  return(cl)
}

#####################################################################
##
##              TM Package Parallel 1 Loop
##
#####################################################################

createCRPChunks <- function(noChunks, crp){
  crpList <- list()
  for(i in 1:noChunks){
    og <- round((i-1)*length(crp)/noChunks)+1
    bg <- round(length(crp)/noChunks*i)
    crpList[[i]] <- crp[og:bg]
  }
  return(crpList)
}

createDocsChunks <- function(noChunks){
  docsList <- list()
  for(i in 1:noChunks){
    og <- round((i-1)*nrow(docs)/noChunks)+1
    bg <- round(nrow(docs)/noChunks*i)
    print(paste(og," --> ",bg))
    docsList[[i]] <- docs[og:bg,]
  }
  
  return(docsList)
}



VCorpChunk1Loop <- function() {
  library(parallel)
  
  print("Define general function to replace strings in corpus")
  crp.replacePattern <-
      content_transformer(function(x, pattern, replace)
        gsub(pattern, replace, x))
  
  print("Create docsChunks")
  docsChunks <- createDocsChunks(no_cores)
  cl <- createCorpusCluster()
  registerDoParallel(cl)
  
  crp <- foreach(docsChunk = docsChunks,
                 .combine = c) %dopar%{
                   crpChunk <- VCorpus(DataframeSource(docsChunk), readerControl = list(language = "en"))
                   
                   print("Remove graphical")
                   tm_map(crpChunk, crp.replacePattern, "[^[:graph:]]", " ")
                   print("To lower")
                   tm_map(crpChunk, content_transformer(tolower))
                   print("Remove stopwords")
                   tm_map(crpChunk, removeWords, c(stopwords("SMART")))
                   print("Stem document")
                   tm_map(crpChunk, stemDocument, language = "porter")
                   print("Remove numbers")
                   tm_map(crpChunk, removeNumbers)
                   print("Remove punctuation")
                   tm_map(crpChunk, removePunctuation, preserve_intra_word_dashes = TRUE)
                   print("Strip whitespace")
                   tm_map(crpChunk, stripWhitespace)
                 }
  stopCluster(cl)
  
  # SAVE RESULTS
  print("Saving results")
  save(crp, file = "crp.RDa")
  return(crp)
  
  ##### Define general function to replace strings in corpus
  print("Define general function to replace strings in corpus")
  (crp.replacePattern <-
      content_transformer(function(x, pattern, replace)
        gsub(pattern, replace, x)))
  
  ##### Clean unicode characters
  ##### Remove graphical characters
  # ids <- 1:length(crp)
  # print("split chunks")
  # chunks <- split(ids, factor(sort(rank(ids) %% no_cores)))
  chunks <- createCRPChunks(no_cores, crp)
  crp <- foreach(chunk = chunks,
                 .combine = c) %dopar% {
                   print("Remove graphical")
                   tm_map(chunk, crp.replacePattern, "[^[:graph:]]", " ")
                   print("To lower")
                   tm_map(chunk, content_transformer(tolower))
                   print("Remove stopwords")
                   tm_map(chunk, removeWords, c(stopwords("SMART")))
                   print("Stem document")
                   tm_map(chunk, stemDocument, language = "porter")
                   print("Remove numbers")
                   tm_map(chunk, removeNumbers)
                   print("Remove punctuation")
                   tm_map(chunk, removePunctuation, preserve_intra_word_dashes = TRUE)
                   print("Strip whitespace")
                   tm_map(chunk, stripWhitespace)
                 }
  print("stopCluster")
  stopCluster(cl)
  
  # SAVE RESULTS
  print("Saving results")
  save(crp, file = "crp.RDa")
  return(crp)
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
  cl <- createCorpusCluster()
  ##### Define general function to replace strings in corpus
  print("Define general function to replace strings in corpus")
  (crp.replacePattern <-
      content_transformer(function(x, pattern, replace)
        gsub(pattern, replace, x)))
  
  ##### Clean unicode characters
  ##### Remove graphical characters
  ids <- 1:length(crp)
  library(parallel)
  no_cores <- 7
  print("split chunks")
  chunks <- split(ids, factor(sort(rank(ids) %% no_cores)))
  
  registerDoParallel(cl)
  print("Remove graphical characters")
  crp <- foreach(chunk = chunks,
                 .combine = c) %dopar% {
    print("test")
    tm_map(crp[chunk], crp.replacePattern, "[^[:graph:]]", " ")
                 }
  
  print("stopCluster")
  # stopCluster(cl)
  
  ##### To lower
  print("To lower")
  # registerDoParallel(cl)
  crp <- foreach(chunk = chunks,
                 .combine = c) %dopar%
    tm_map(crp[chunk], content_transformer(tolower))
  
  # stopCluster(cl)
  
  ##### Stopword removal
  print("Stopword removal")
  # registerDoParallel(cl)
  crp <- foreach(chunk = chunks,
                 .combine = c) %dopar%
    tm_map(crp[chunk], removeWords, c(stopwords("SMART")))
  
  # stopCluster(cl)
  
  ##### Stemming
  print("Stemming")
  # registerDoParallel(cl)
  crp <- foreach(chunk = chunks,
                 .combine = c) %dopar%
    tm_map(crp[chunk], stemDocument, language = "porter")
  
  # stopCluster(cl)
  
  ##### Numbers
  
  ##### All numbers (including numbers as part of a alphanumerical term)
  print("Removing all numbers")
  # registerDoParallel(cl)
  crp <- foreach(chunk = chunks,
                 .combine = c) %dopar%
    tm_map(crp[chunk], removeNumbers)
  
  # stopCluster(cl)
  
  ##### Punctuation
  print("remove Puncuation")
  # registerDoParallel(no_cores)
  crp <- foreach(chunk = chunks,
                 .combine = c) %dopar%
    tm_map(crp[chunk], removePunctuation, preserve_intra_word_dashes = TRUE)
  
  # stopCluster(cl)
  
  ##### Whitespace
  print("Remove whitespace")
  # registerDoParallel(cl)
  crp <- foreach(chunk = chunks,
                 .combine = c) %dopar%
    tm_map(crp[chunk], stripWhitespace)
  
  stopCluster(cl)
  
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
  print("Stopword")
  crp <- tm_map(crp, removeWords, c(stopwords("SMART")))
  
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
  
  quanteda_options(threads = 8)
  
  print("Creating Quanteda Corpus")
  crpT <- corpus(docs)
  
  #Quanteda tokens
  print("Creating tokens, removing punctuation & numbers")
  crpT <- tokens(crpT, remove_punct = TRUE, remove_numbers = TRUE)
  # 
  # ids <- 1:length(crpT)
  # no_cores = detectCores()
  # chunks <- split(ids, factor(sort(rank(ids) %% no_cores)))
  # 
  # registerDoParallel(no_cores)
  
  #Remove symbols
  print("Remove regex")
  crpT <- tokens_remove(crpT, "\\p{Z}", valuetype = "regex")
  
  
  print("Remove symbols")
  crpT <- tokens(crpT, remove_symbols = TRUE)
  
  ##### Stemming
  print("Stemming")
  #crpT <- tokens_wordstem(crpT, language = "porter")
  crpT <- tokens_wordstem(crpT)
  
  ##### Stopword removal
  print("Stopword removal")
  crpT <- tokens_select(crpT, stopwords(source = "smart"), selection = 'remove')
  
  ##### To lower
  print("To lower")
  crpT <- tokens_tolower(crpT)
  
  ##### Clean unicode characters
  ##### Remove graphical characters
  print("Remove graphical characters")
  crpT <- tokens_remove(crpT, "*#*")
  crpT <- tokens_remove(crpT, "*-*")
  crpT <- tokens_remove(crpT, "*.*")
  crpT <- tokens_remove(crpT, "*,*")
  crpT <- tokens_remove(crpT, "*\\d*")
  # stopImplicitCluster()
  
  # SAVE RESULTS
  print("Saving")
  save(crpT, file = "crpT.RDa")
  print("Return result")
  return(crpT)
}

#####################################################################
##
##                            text2vec
##
#####################################################################

textvec <- function() {
  # preprocessor = function(x) {
  #   gsub("[^[:alnum:]\\s]", replacement = " ", tolower(x))
  # }
  # sample_ind = 1:100
  # #tokens = word_tokenizer(preprocessor(movie_review$review[sample_ind]))
  # tokens = word_tokenizer(preprocessor(docs$text[sample_ind]))
  # it = itoken(tokens, ids = docs$doc_id[sample_ind])
  # system.time(v <- create_vocabulary(it))
  # v = prune_vocabulary(v, term_count_min = 5)
  # model = Collocations$new(collocation_count_min = 5, pmi_min = 5)
  # model$fit(it, n_iter = 2)
  # model$collocation_stat
  # it2 = model$transform(it)
  # v2 = create_vocabulary(it2)
  # v2 = prune_vocabulary(v2, term_count_min = 5)
  # # check what phrases model has learned
  # setdiff(v2$term, v$term)
  # # and same way we can create document-term matrix which contains
  # # words and phrases!
  # dtm = create_dtm(it, vocab_vectorizer(v2))
  
  setDT(docs)
  setkey(docs, doc_id)
  set.seed(2017L)
  all_ids = docs$doc_id
  train_ids = sample(all_ids, 4000)
  test_ids = setdiff(all_ids, train_ids)
  train = docs[J(train_ids)]
  test = docs[J(test_ids)]
  
  # define preprocessing function and tokenization function
  prep_fun = tolower
  tok_fun = word_tokenizer
  
  it_train = itoken(train$text, 
                    preprocessor = prep_fun, 
                    tokenizer = tok_fun, 
                    ids = train$doc_id, 
                    progressbar = FALSE)
  vocab = create_vocabulary(it_train)
  
  train_tokens = train$text %>% 
    prep_fun %>% 
    tok_fun
  it_train = itoken(train_tokens, 
                    ids = train$doc_id,
                    # turn off progressbar because it won't look nice in rmd
                    progressbar = FALSE)
  
  vocab = create_vocabulary(it_train)
  vocab
  
  vectorizer = vocab_vectorizer(vocab)
  t1 = Sys.time()
  dtm_train = create_dtm(it_train, vectorizer)
  print(difftime(Sys.time(), t1, units = 'sec'))
  dim(dtm_train)
  identical(rownames(dtm_train), train$doc_id)
  
  t1 = Sys.time()
  vocab = create_vocabulary(it_train, stopwords = stopwords(source = "snowball"))
  print(difftime(Sys.time(), t1, units = 'sec'))
  
  dim(dtm_train)
  
}