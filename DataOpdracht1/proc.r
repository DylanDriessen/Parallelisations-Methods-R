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

source("lib/readFiles.r")
readFiles()

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
createCorpus()

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
