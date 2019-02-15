library(stringi)
library(parallel)

ps18b_abstr_cln <- PS18B_ABSTR_01


no_cores <- detectCores()-1

parLapplyFunc <- function() {
  cl <- makeCluster(no_cores)
  ps18b_abstr_cln$appln_abstract <- parLapply(cl, PS18B_ABSTR_01$appln_abstract, stringi::stri_trans_general, 'Latin-ASCII')
  stopCluster(cl)
}



parApplyFunc <- function() {
  cl <- makeCluster(no_cores)
  ps18b_abstr_cln$appln_abstract <- parApply(cl, as.array(PS18B_ABSTR_01$appln_abstract), 1, stringi::stri_trans_general, 'Latin-ASCII')
  stopCluster(cl)
}

library(microbenchmark)
serial <- function(){
  ps18b_abstr_cln$appln_abstract <- stringi::stri_trans_general(PS18B_ABSTR_01$appln_abstract, 'Latin-ASCII') 
}

micro <- microbenchmark(serial(), parLapplyFunc(), parApplyFunc(), times = 3)
micro
plot(micro)

