import(c("biganalytics", "cluster"))

kfitDFM <- kmeans(DFM, 1, nstart = 1)

big <- bigkmeans(DFM, centers = 1)

m <- as.matrix(DFM)

M <- as.big.matrix(x = as.matrix(DFMasDTM))

CLARA <- clara(DFM, 2, metric = "euclidean", stand = FALSE, 
               samples = 1, pamLike = FALSE)
