library(ff)
library(doParallel)
library(parallel)
library(ffbase)

no_batches <- 5

cluster <- makeCluster(no_batches,outfile="")
registerDoParallel(cluster)

ffdfdataframe <- foreach (i = 1:no_batches,
                     .combine = ffdfappend,
                     .packages = "ff")%dopar%{
                       return(read.table.ffdf(file=paste0("/home/pieter/data/mini/tls203_part0",i,".txt.xz"),sep=",",skip=1,appendLevels = TRUE))
                       }

stopCluster(cluster)


dataFrame <- as.data.frame(ffdfdataframe)

res.text.dataFrame <- dataFrame$V2
res.text.ffdfdataframe <- ffdfdataframe$V2

res.lang.dataFrame <- dataFrame$V3
res.lang.ffdfdataframe <- ffdfdataframe$V3

remove(cluster)
remove(no_batches)



############################################################################################
#
# ffdf werkt goed voor data die wederkerend is zoals de talen in de 2e col 
# ffdf werkt niet goed voor data die allemaal verschillend is zoals de texten in de 3e col
#
#
############################################################################################