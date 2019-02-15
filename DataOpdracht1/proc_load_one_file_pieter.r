library(stringi)
library(parallel)

ps18b_abstr_cln <- PS18B_ABSTR_01

serialPreProcessing <-function(){
  ps18b_abstr_cln$appln_abstract <- lapply(PS18B_ABSTR_01$appln_abstract,stringi::stri_trans_general,id='Latin-ASCII')
}

parallelPreProcessing <- function(){
  no_cores <- detectCores()-1
  cluster<-makeCluster(no_cores)
  ps18b_abstr_cln$appln_abstract <- parLapply(cl,PS18B_ABSTR_01$appln_abstract,stringi::stri_trans_general,id='Latin-ASCII')
  stopCluster(cluster)
}


library(microbenchmark)


micro <- microbenchmark(serialPreProcessing(),parallelPreProcessing(), times = 3)
micro
plot(micro)

