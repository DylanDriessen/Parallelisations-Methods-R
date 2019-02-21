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



source("lib/readFiles.r")
docs <- readFiles()           
#benchmark_readFiles()

################################################################################
#
# 2 PREPROCESS
#
# 18/02/2019 Tom Magerman
#
################################################################################

source("lib/preProcess.r")
preProcess()

# ==============================================================================
#
# 3 CREATE AND CLEAN CORPUS
#
# 18/02/2019 Tom Magerman
#
# ==============================================================================

source("lib/createCorpus.r")
docsCorpus <- createCorpus()

# ==============================================================================
#
# 4 CREATE DTM
#
# 18/02/2019 Tom Magerman
#
# ==============================================================================

source("lib/createDTM.r")
createDTM()

# ==============================================================================
#
# 5 DERIVE VOCABULARY
#
# 18/02/2019 Tom Magerman
#
# ==============================================================================

source("lib/deriveVocabulary.r")
deriveVocabulary()

# SAVE RESULTS


save(voc, file="voc.RDa")
