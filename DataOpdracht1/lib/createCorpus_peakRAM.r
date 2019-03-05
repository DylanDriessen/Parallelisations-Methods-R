source("lib/createCorpus.r")

#####################################################################
##
##              TM Package Parallel 1 Loop
##
#####################################################################

TMCorpusChunk1Loop_peakRAM <- function() {
  # print("Define general function to replace strings in corpus")
  crp.replacePattern <-
    content_transformer(function(x, pattern, replace)
      gsub(pattern, replace, x))
  
  # print("Create docsChunks")
  docsChunks <- createDocsChunks(no_cores)
  cl <- createCorpusCluster()
  clusterEvalQ(cl, library("peakRAM"))
  registerDoParallel(cl)
  
  crp <- foreach(docsChunk = docsChunks,
                 .combine = rbind) %dopar% {
                   t <- Sys.time()
                   df <- peakRAM({
                   
                   
                   crpChunk <-
                     VCorpus(DataframeSource(docsChunk),
                             readerControl = list(language = "en"))
                   pid <- Sys.getpid()
                   
                   # print(paste(pid, "   Remove graphical"))
                   tm_map(crpChunk, crp.replacePattern, "[^[:graph:]]", " ")
                   # print(paste(pid, "   To lower"))
                   tm_map(crpChunk, content_transformer(tolower))
                   # print(paste(pid, "   Remove stopwords"))
                   tm_map(crpChunk, removeWords, c(stopwords("SMART")))
                   # print(paste(pid, "   Stem document"))
                   tm_map(crpChunk, stemDocument, language = "porter")
                   # print(paste(pid, "   Remove numbers"))
                   tm_map(crpChunk, removeNumbers)
                   # print(paste(pid, "   Remove punctuation"))
                   tm_map(crpChunk, removePunctuation, preserve_intra_word_dashes = TRUE)
                   # print(paste(pid, "   Strip whitespace"))
                   tm_map(crpChunk, stripWhitespace)
                   })
                   cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
                 }
  stopCluster(cl)
  return(crp)
}


#####################################################################
##
##                           TM Package Parallel
##
#####################################################################

TMCorpusChunk_peakRAM <- function() {
  t <- Sys.time()
  df <- peakRAM(TMCorpusChunk())
  cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
}


#####################################################################
##
##                           TM Package
##
#####################################################################

TMCorpus_peakRAM <- function() {
  t <- Sys.time()
  df <- peakRAM(TMCorpus())
  cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
}

#####################################################################
##
##                            Quanteda
##
#####################################################################
#https://cran.r-project.org/web/packages/quanteda/quanteda.pdf

QuantedaCorpus_peakRAM <- function() {
  t <- Sys.time()
  df <- peakRAM(QuantedaCorpus())
  cbind(Process_Id = Sys.getpid(), df[,2:4], Start_Time = t, End_Time = Sys.time())
}