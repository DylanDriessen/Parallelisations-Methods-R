library(stringi)
library(parallel)

ps18b_abstr_cln <- PS18B_ABSTR_01


no_cores <- detectCores()-1
cl <- makeCluster(no_cores)
parApply(cl, PS18B_ABSTR_01$appln_abstract, function(i){
  #stringi::stri_trans_general(i, 'Latin-ASCII')
  i
})

library(microbenchmark)
serial <- function(){
  ps18b_abstr_cln$appln_abtract <- stringi::stri_trans_general(PS18B_ABSTR_01$appln_abstract, 'Latin-ASCII') 
}

micro <- microbenchmark(serial(), times = 3)
micro
plot(micro)

