setwd("~/R/Afstudeerwerk/DataOpdracht1")
source("loadPackages.R")

no_cores <- detectCores()

source("lib/readFiles.r")

ifn <- "tls203_part"; ifp <- "../../../data/mini/"; batches <- 5
saveRDS(summary(benchmark_read(), unit="s"), "docs/readFiles/resources/benchmark_mini.rds")

ifn <- "tls203_part"; ifp <- "../../../data/small/"; batches <- 5
saveRDS(summary(benchmark_read(), unit="s"), "docs/readFiles/resources/benchmark_small.rds")

ifn <- "tls203_part"; ifp <- "../../../data/medium/"; batches <- 5
saveRDS(summary(benchmark_read(), unit="s"), "docs/readFiles/resources/benchmark_medium.rds")

ifn <- "tls203_part"; ifp <- "../../../data/"; batches <- 5
saveRDS(summary(benchmark_read(), unit="s"), "docs/readFiles/resources/benchmark_large.rds")


