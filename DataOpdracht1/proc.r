(.packages())
source("util/importPackage.r")

import(c("readr","tibble","data.table", "parallel", "foreach", "doSNOW", "snow", 
         "stringi", "ff", "ffbase", "tm","SnowballC","slam","stringi","data.table",
         "magrittr","corrplot","NLP", "foreach","doParallel","microbenchmark",
         "text2vec","doMC","quanteda","textmineR", "parallel", "peakRAM",
         "microbenchmark","glmnet", "quanteda", "tcltk2", "pryr", "dplyr"))


## Batches info
ifn <- "tls203_part"; ifp <- "../../../data/mini/"; ofn <- "ps18b_abstr"; batches <- 5

no_cores <- 8#detectCores()

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
#docs <- readFiles_doparallel_foreach_ffdf()
docs <- readFiles_doparallel_foreach()
docs$id <- 1:nrow(docs)
#benchmark_read()

################################################################################
#p
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
#docsCorpus2 <- createCorpus()
#microbenchmark(VCorp(), VCorpChunk(), Quan(), times = 1)
#microbenchmark_data <- microbenchmark(VCorpChunk = VCorpChunk(), Quan = Quan(), times = 1)[,2]*10^-9
#microbenchmark_data <- rbind(vcorpFunction = microbenchmark(VCorp(), times = 1)[,2]*10^-9, 
#                             quanFunction = microbenchmark(Quan() ,times = 1)[,2]*10^-9, 
#                             vcorpchunkFunction = microbenchmark(VCorpChunk() ,times = 1)[,2]*10^-9)

#saveRDS(microbenchmark_data, file = "~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/microbenchmark_data.rds")

# ==============================================================================
#
# 4 CREATE DTM
#
# 18/02/2019 Tom Magerman
#
# ==============================================================================

source("lib/createDTM.r")
DFM <- createDTM()
microbenchmark(createDFM(), createDfmChunks(), createDfmChunksBind(), times = 5)

# ==============================================================================
#
# 5 DERIVE VOCABULARY
#
# 18/02/2019 Tom Magerman
#
# ==============================================================================

source("lib/deriveVocabulary.r")
Voca <- deriveVoc()

# SAVE RESULTS


save(voc, file="voc.RDa")
