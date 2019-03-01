import(c("tm","SnowballC","slam","stringi","data.table","magrittr","corrplot","NLP",
    "foreach","doParallel","microbenchmark","text2vec","doMC","quanteda","textmineR",
    "parallel", "peakRAM"))

source("lib/createCorpus.r")

createCorpusCluster_peakRAM <- function() {
  cl <- makeCluster(4, outfile = "")
  clusterEvalQ(cl, { library("tm"); library("peakRAM") })
  return(cl)
}

VCorpChunk_peakRAM <- function() {
  #TODO?
}

VCorp_peakRAM <- function() {
  return(peakRAM(VCorp()))
}

Quan_peakRAM <- function() {
  return(peakRAM(Quan()))
}