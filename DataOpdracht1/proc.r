(.packages())

source("loadPackages.R")

no_cores <- detectCores()
save <- TRUE # save to data files

## Batches info
ifn <- "tls203_part"; ifp <- "../../../data/mini/"; batches <- 1

if (save) dir.create("data", showWarnings = FALSE)

# Issues: 
#   Language dependent stop word removal
#   Language dependent stemming
#   Stem completion

################################################################################
# IMPORT SOURCE DATA
################################################################################

# read batches and add ids
source("lib/readFiles.r")
docs <- readFiles_doparallel_foreach()
docs$id <- 1:nrow(docs)

# save to RDS
if (save) saveRDS(docs, file="data/docs.rds")

################################################################################
# 2 PREPROCESS
################################################################################

# copy docs into new object
docspp <- docs

# preprocess
source("lib/preProcess.r")
docspp$text <- preProcessClusterChunked()

# save to RDS
if (save) saveRDS(docspp, "data/docspp.rds")

# remove docs
rm(docs); gc()

# ==============================================================================
# 3 CREATE AND CLEAN CORPUS
# ==============================================================================

# create corpus
source("lib/createCorpus.r")
docsCorpus <- createCorpus()
docsCorpusQuan <- createCorpusQuan()

# save to RDS
if (save) { saveRDS(docsCorpus, "data/docsCorpus.rds"); 
  saveRDS(docsCorpusQuan, "data/docsCorpusQuan.rds") }

# remove docspp
rm(docspp); gc()

#docsCorpus2 <- createCorpus()
#microbenchmark(VCorpChunk(), Quan(), VCorpChunk1Loop(), times = 2)
#microbenchmark_data <- microbenchmark(VCorpChunk = VCorpChunk(), Quan = Quan(), times = 1)[,2]*10^-9
#microbenchmark_data <- rbind(vcorpFunction = microbenchmark(VCorp(), times = 1)[,2]*10^-9, 
#                             quanFunction = microbenchmark(Quan() ,times = 1)[,2]*10^-9, 
#                             vcorpchunkFunction = microbenchmark(VCorpChunk() ,times = 1)[,2]*10^-9)

#saveRDS(microbenchmark_data, file = "~/R/Afstudeerwerk/DataOpdracht1/RShinyDashboardAfstudeer/data/microbenchmark_data.rds")

# ==============================================================================
# 4 CREATE DTM
# ==============================================================================

# create DFM & DTM
source("lib/createDTM.r")
DFM <- createDFM()
DTM <- createDTM()

# save to RDS
if (save) { saveRDS(DFM, "data/DFM.rds"); 
  saveRDS(DTM, "data/DTM.rds") }

# remove corpus
rm(docsCorpusQuan); rm(docsCorpus); gc()

#microbenchmark(createDFM(), createDfmChunks(), createDfmChunksBind(), times = 5)

# ==============================================================================
# 5 DERIVE VOCABULARY
# ==============================================================================

source("lib/deriveVocabulary.r")
voc <- deriveVoc()
saveRDS(voc, "data/voc.rds")
rm(voc); gc()

# ==============================================================================
# 6 CREATE CLUSTER
# ==============================================================================

source("lib/cluster.R")
cluster <- clusterMatrix()

#microbenchmark(skmeansCluster(), skmeansClusterPar10(), skmeansClusterPar100(), times = 1)

if (save) saveRDS(cluster, "data/cluster.rds")
rm(cluster); gc()
