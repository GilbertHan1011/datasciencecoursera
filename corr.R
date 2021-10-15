#== R programming program week2
#== Gilbert
#== 10.16.2021

corr <- function(directory, threshold=0){
  files <- list.files(directory,full.names = T)
  fileList <- map(files,read.csv)
  fileNew <- map(fileList, na.omit)
  fileNew<- fileNew[unlist(map(fileNew, function(x) dim(x)[1]>threshold))]
  corData <-unlist(lapply(fileNew, function(x) cor(x[2],x[3])))
  return(corData)
}

