(.packages())


# Issues: 
#   Language dependent stop word removal
#   Language dependent stemming
#   Stem completion


################################################################################
#
# IMPORT SOURCE DATA
#
################################################################################


source("importPackage.r")

import(c("readr","tibble","data.table","stringi"))


ifn <- "tls203_part"
batches <- 1
#ifp <- "D:/DATA_Active/Data/PATSTAT_2018_B/1_Source_Data/source_zip_files/"
ifp <- "../../../data/"
ofn <- "ps18b_abstr"


# READ AND PROCESS BATCHES


# Read and process batches
for(batch_nr in 1:batches) {

  # Compile file name to read
  batch_no <- sprintf("%02d", batch_nr)
  fn <- paste0(ifn, batch_no, ".txt.xz")
  fp <- ifp

  # Read batch file
  v <- paste0(ofn, "_", batch_no)
  assign(v, tibble::as_data_frame(readr::read_delim(paste0(fp, fn), 
                                                 ",",
                                                 col_names=TRUE,
                                                 #col_types="icc",
                                                 # c:char; i:int; n:num; d:dbl
                                                 # l:log; D:date; T:date time; t:time
                                                 # ?:guess _/-:skip
                                                 #guess_max=min(1000, n_max),
                                                 quote = "\"",
                                                 escape_backslash = FALSE,
                                                 escape_double = FALSE,
                                                 na=character(),
                                                 #na=c("", "NA"),
                                                 #quoted_na = FALSE,
                                                 trim_ws= TRUE,
                                                 skip=0,
                                                 #n_max=smplsize,
                                                 n_max=Inf,
                                                 locale = locale(encoding = "UTF-8")
         ))
  )

  # Save intermediate batch file
  fn <- paste0(ofn, "_", batch_no, ".xz.RDa")
  save(list=v, file=fn, compress="xz", compression_level=2)

} # Read and process batches


# COMBINE BATCHES


# Variable name to use
v <- ofn

# Pattern to use
pt <- paste0(ofn, "_", "[0-9]+")
assign(v, as.data.frame(rbindlist(mget(ls(pattern=pt)))))


################################################################################
#
# 1 COMPILE DOCUMENT COLLECTION
#
# 18/02/2019 Tom Magerman
#
################################################################################


docs <- ps18b_abstr_smpl[,c("appln_id", "appln_abstract", "appln_abstract_lg")]
names(docs) <- c("doc_id", "text", "language")
str(docs)


################################################################################
#
# 2 PREPROCESS
#
# 18/02/2019 Tom Magerman
#
################################################################################


library(stringi)   # String manipulation


# TRANSLITERATE TO LATIN


docs$text <- stringi::stri_trans_general(docs$text, 'Latin-ASCII')


# ==============================================================================
#
# 3 CREATE AND CLEAN CORPUS
#
# 18/02/2019 Tom Magerman
#
# ==============================================================================


library(tm)        # Basic text mining
library(SnowballC) # Additional stemmers
library(stringi)   # String manipulation
library(slam)      # Sparse matrices
library(wordcloud) # Word clouds
library(corrplot)  # Correlation plots
#library(RWeka)    # Additional tokenizers
library(NLP)       # Additional tokenizers


# COMPILE CORPUS (FROM DATAFRAME, PREVIOUS TO VERSION 0.7-6)


# Define reader to map id, content and metadata

#myReader <- readTabular(mapping=list(id="id", content=as.character("txt"), language="lg", mime="mime"))

# Language setting: fixed language or language set by metadata

# OR Fixed language
#crp <- VCorpus(DataframeSource(docs), readerControl=list(reader=myReader, language="en"))
# OR Language in metadata
#crp <- VCorpus(DataframeSource(docs), readerControl=list(reader=myReader))


# COMPILE CORPUS (FROM DATAFRAME, AS OF VERSION 0.7-6)


# Create corpus (and define default language)

crp <- VCorpus(DataframeSource(docs), readerControl=list(language="en"))

# Set metadata based on fields in source data

for(i in 1:length(crp)) {
  meta(crp[[i]], "language") <- docs$language[i]
  #meta(crp[[i]], "author") <- docs$author[i]
}


# INSPECT CORPUS

# Check number of documents in corpus
#length(crp)

# Inspect corpus document
#inspect(crp[1])

# View document text contents
#str(crp[[1]]$content)
#strwrap(crp[[1]]$content)
#as.character(crp[[1]])

# View metadata document
#meta(crp[[1]])
#meta(crp[[1]],"id")
#meta(crp[[1]],"language")
#meta(crp[[1]],"author")

# Inspect multiple documents
#lapply(crp[1:10], as.character)


# ANALYSE CORPUS


# CLEAN CORPUS


# Define general function to replace strings in corpus

(crp.replacePattern <- content_transformer(function(x, pattern, replace) gsub(pattern, replace, x)))

# Clean unicode characters

# Remove graphical characters
crp <- tm_map(crp, crp.replacePattern, "[^[:graph:]]", " ")

# Remove unicode chars
#crp <- tm_map(crp, content_transformer (function(x) iconv(x,"UTF-8","ASCII",sub="")))

# To lower

crp <- tm_map(crp, content_transformer(tolower))

# Stopword removal

# Check stopword lists
#stopwords("SMART")
#stopwords("english")
#stopwords("en")
# Supported languages: danish, dutch, english, finnish, french, german, 
# hungarian, italian, norwegian, portugese, russion, spanish, swedish

#crp <- tm_map(crp, removeWords, c("the", "a", "is"))
crp <- tm_map(crp, removeWords, stopwords("SMART"))
#crp <- tm_map(crp, removeWords, stopwords("english"))
#crp <- tm_map(crp, removeWords, stopwords("en"))
#crp <- tm_map(crp, removeWords, c(stopwords("english"), "method", "process"))
#crp <- tm_map(crp, removeWords, c(stopwords("english"), stopwords("dutch")))

#crp <- tm_map(crp, removeWords, stopwords(meta(corp, "language")))

# Stemming

crp <- tm_map(crp, stemDocument, language = "porter")
#crp <- tm_map(crp, stemDocument, language = "english")

#crp <- tm_map(crp, stemDocument, language = meta(crp, "language"))

# Stem completion

# Numbers

# All numbers (including numbers as part of a alphanumerical term)
crp <- tm_map(crp, removeNumbers)

# Pure numbers
#crp <- tm_map(crp, crp.replacePattern, " [[:digit:]]+ ", " ")
#crp <- tm_map(crp, crp.replacePattern, " [[:digit:]]+ ", " ")
#crp <- tm_map(crp, crp.replacePattern, " [[:digit:]]+ ", " ")
#crp <- tm_map(crp, crp.replacePattern, " [[:digit:]]+ ", " ")
#crp <- tm_map(crp, crp.replacePattern, " [[:digit:]]+ ", " ")
#crp <- tm_map(crp, crp.replacePattern, " [[:digit:]]+$", "")

# Punctuation

crp <- tm_map(crp, removePunctuation, preserve_intra_word_dashes = TRUE)

# Whitespace

crp <- tm_map(crp, stripWhitespace)

# Remove special characters (or replace by space)

#crp <- tm_map(crp, crp.replacePattern, "-", " ")
#crp <- tm_map(crp, crp.replacePattern, "'", "")
#crp <- tm_map(crp, crp.replacePattern, """, "")

# Remove final punctuation

#crp <- tm_map(crp, removePunctuation, preserve_intra_word_dashes = TRUE)
#crp <- tm_map(crp, crp.replacePattern, "[[:punct:]] +", " ")

# Remove final whitespace

#crp <- tm_map(crp, stripWhitespace)


# SAVE RESULTS


save(crp, file="crp.RDa")


# ==============================================================================
#
# 4 CREATE DTM
#
# 18/02/2019 Tom Magerman
#
# ==============================================================================


# CREATE DTM (RAW AND WEIGHTED)


dtm_ctrl <- list(tokenize = "words",
                 tolower = FALSE, 
                 removePunctuation = FALSE,
                 removeNumbers = FALSE,
                 stopwords = FALSE,
                 stemming = FALSE,
                 dictionary = NULL,
                 bounds = list(global = c(1, Inf)),
                 weighting = weightTf,
                 wordLengths = c(1, Inf) 
)


dtm_raw    <- DocumentTermMatrix(crp, control = dtm_ctrl)
dtm_tfidf  <- weightTfIdf(dtm_raw, normalize = FALSE)


# SAVE RESULTS


save(dtm_raw, dtm_ctrl, file="dtm_raw.RDa")
save(dtm_tfidf, dtm_ctrl, file="dtm_tfidf.RDa")


# ==============================================================================
#
# 5 DERIVE VOCABULARY
#
# 18/02/2019 Tom Magerman
#
# ==============================================================================


# DERIVE VOCABULARY


voc <- data.frame(trm = as.character(names(col_sums(dtm_raw))), cFrqs = col_sums(dtm_raw), dFrqs = col_sums(weightBin(dtm_raw)),stringsAsFactors=FALSE)
#voc <- voc[with (voc, order(trm)),]
voc <- voc[with (voc, order(-cFrqs,-dFrqs,trm)),]
voc <- cbind(trm_id=seq(1, nrow(voc)),voc)
rownames(voc) <- voc$trm_id
#voc$stopword.nl <- sapply(voc$trm, function(x) {as.integer(x %in% stopwords("nl"))})
#voc$stopword.en <- sapply(voc$trm, function(x) {as.integer(x %in% stopwords("en"))})
#voc$isCapitalCase <- sapply(voc$trm, function(x) as.integer(isCapitalCase(x)))


# SAVE RESULTS


save(voc, file="voc.RDa")
