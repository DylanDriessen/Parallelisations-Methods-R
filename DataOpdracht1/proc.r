(.packages())
source("util/importPackage.r")
no_cores <- detectCores()

# Issues: 
#   Language dependent stop word removal
#   Language dependent stemming
#   Stem completion


################################################################################
#
# IMPORT SOURCE DATA
#
################################################################################

#library(peakRAM)
source("lib/readFiles.r")
docs <- readFiles_doparallel_foreach()
#call_functions_for_ram()
#benchmark_readFiles()

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
