library(stringi)
library(parallel)

ps18b_abstr_cln <- PS18B_ABSTR_01

no_cores <- detectCores()-1

parLapplyParallel <- function() {
  cl <- makeCluster(no_cores)
  ps18b_abstr_cln$appln_abstract <- parLapply(cl, PS18B_ABSTR_01$appln_abstract, stringi::stri_trans_general, 'Latin-ASCII')
  stopCluster(cl)
}

parApplyParallel <- function() {
  cl <- makeCluster(no_cores)
  ps18b_abstr_cln$appln_abstract <- parApply(cl, as.array(PS18B_ABSTR_01$appln_abstract), 1, stringi::stri_trans_general, 'Latin-ASCII')
  stopCluster(cl)
}

clusterApplyParallel <- function() {
  cl <- makeCluster(no_cores)
  
  #put in groups
  groups <- sample(no_cores, length(PS18B_ABSTR_01$appln_abstract), replace = TRUE)
  split_groups <- split(PS18B_ABSTR_01$appln_abstract, groups)
  split_groups
  
  res <- clusterApply(cl, split_groups, stringi::stri_trans_general,'Latin-ASCII')
  ps18b_abstr_cln$appln_abstract <- unlist(res)
  
  stopCluster(cl)
}

library(microbenchmark)
sequential <- function(){
  ps18b_abstr_cln$appln_abstract <- stringi::stri_trans_general(PS18B_ABSTR_01$appln_abstract, 'Latin-ASCII') 
}

micro <- microbenchmark(sequential(), parLapplyParallel(), parApplyParallel(), clusterApplyParallel(), times = 3)
micro
plot(micro)