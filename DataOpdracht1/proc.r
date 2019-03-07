(.packages())
source("util/importPackage.r")

import(c("readr","tibble","data.table", "parallel", "foreach", "doSNOW", "snow", 
         "stringi", "ff", "ffbase", "tm","SnowballC","slam","stringi","data.table",
         "magrittr","corrplot","NLP", "foreach","doParallel","microbenchmark",
         "text2vec","doMC","quanteda","textmineR", "parallel", "peakRAM",
         "microbenchmark","glmnet", "quanteda", "tcltk2", "pryr", "dplyr", "stopwords", "skmeans"))


## Batches info
ifn <- "tls203_part"; ifp <- "../../../data/mini/"; ofn <- "ps18b_abstr"; batches <- 1

no_cores <- detectCores()

# Issues: 
#   Language dependent stop word removal
#   Language dependent stemming
#   Stem completion

# source("lib/readFiles_peakRAM.r")
# read_peakRAM_to_rds()

################################################################################
#
# IMPORT SOURCE DATA
#
################################################################################

source("lib/readFiles.r")
#docs <- read_doparallel_foreach_ffdf()     ### Ik denk dat ffdf aleen goed werk voor data die wederkerend is zoals bv de data in de taal col. dit zijn 4 verschillende waardes die steeds herbruikt worden. in de text col staat telkens een andere waarde
docs <- readFiles_doparallel_foreach()
docs$id <- 1:nrow(docs)
#benchmark_read()

################################################################################
#
# 2 PREPROCESS
#
# 18/02/2019 Tom Magerman
#
################################################################################

source("lib/preProcess.r")
docs$text <- preProcessClusterChunked()

#benchmark_preProcess(createPlot = TRUE,times = 1)


# ==============================================================================
#
# 3 CREATE AND CLEAN CORPUS
#
# 18/02/2019 Tom Magerman
#
# ==============================================================================

source("lib/createCorpus.r")
docsCorpus <- createCorpus()
docsCorpusQuan <- createCorpusQuan()

# ==============================================================================
#
# 4 CREATE DTM
#
# 18/02/2019 Tom Magerman
#
# ==============================================================================

source("lib/createDTM.r")
DFM <- createDFM()
DTM <- createDTM()
#microbenchmark(createDFM(), createDfmChunks(), createDfmChunksBind(), times = 5)

# ==============================================================================
#
# 5 DERIVE VOCABULARY
#
# 18/02/2019 Tom Magerman
#
# ==============================================================================

source("lib/deriveVocabulary.r")
Voca <- deriveVoc()

# ==============================================================================
#
# 6 CREATE CLUSTER
#
# ==============================================================================

source("lib/cluster.R")
cluster <- clusterMatrix()
#microbenchmark(skmeansCluster(), skmeansClusterPar10(), skmeansClusterPar100(), times = 1)

# SAVE RESULTS
save(voc, file="voc.RDa")
