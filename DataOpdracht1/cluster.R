import(c("biganalytics"))

kfitDFM <- kmeans(DFM, 2, nstart = 1)

big <- bigkmeans(DFMasDTM6.1, centers = 5)

m <- as.matrix(DFM)

M <- as.big.matrix(x = as.matrix(DFMasDTM))

