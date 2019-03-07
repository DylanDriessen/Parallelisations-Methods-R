source("loadPackages.R")
# Shiny specific packages
import(c("shiny", "ggplot2", "plotly", "DT", "future", "promises", "markdown"))

# Batches info
ifn <- "tls203_part"; ifp <- "../../../data/small/"; ofn <- "ps18b_abstr"; batches <- 5

# Load intermediate files into memory
docs <- readRDS("data/docs.rds")
docspp <- readRDS("data/docspp.rds")
docsCorpus <- readRDS("data/docsCorpus.rds")
docsCorpusQuan <- readRDS("data/docsCorpusQuan.rds")
DTM <- readRDS("data/DTM.rds")
DFM <- readRDS("data/DFM.rds")

# niet nodig voor dashboard
# voc <- readRDS("data/voc.rds")
# cluster <- readRDS("data/cluster.rds")

source("lib/realtime_sysinfo.r")
source("util/SaveFunctionData.r")

source("lib/readFiles_peakRAM.r")
source("lib/preProcess_peakRAM.r")
source("lib/createCorpus_peakRAM.r")
source("lib/createDTM_peakRAM.r")
source("lib/cluster_peakRAM.r")