(.packages())
source("util/importPackage.r")
source("lib/realtime_sysinfo.r")

## Batches info
ifn <- "tls203_part"; ifp <- "../../../data/mini/"; ofn <- "ps18b_abstr"; batches <- 5

import("parallel")
no_cores <- 2#detectCores()

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
docs <- readFiles_doparallel_foreach_ffdf()
docs2 <- readFiles_doparallel_foreach()
#benchmark_read()

################################################################################
#
# 2 PREPROCESS
#
# 18/02/2019 Tom Magerman
#
################################################################################

source("lib/preProcess.r")
docs$text <- preProcess_DevidedInChunks_doparallel()
#benchmark_preProcess()


# ==============================================================================
#
# 3 CREATE AND CLEAN CORPUS
#
# 18/02/2019 Tom Magerman
#
# ==============================================================================

source("lib/createCorpus.r")
docsCorpus <- createCorpus()
#microbenchmark(VCorp(), VCorpChunk(), Quan(), times = 1)

# ==============================================================================
#
# 4 CREATE DTM
#
# 18/02/2019 Tom Magerman
#
# ==============================================================================

source("lib/createDTM.r")
DocumentTermMatrix <- createDTM()

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
