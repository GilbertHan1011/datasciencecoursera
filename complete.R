#== R programming program week2
#== Gilbert
#== 10.16.2021

complete <- function(directory, id = 1:332){
  require(tidyverse)
  files <- list.files(directory,full.names = T)
  fileSelect <- files[id]
  fileList <- map(fileSelect,read.csv)
  fileBind <- do.call(rbind,fileList)
  fileTable <- na.omit(fileBind)
  fileOb <- data.frame(table(id=fileTable$ID))
  colnames(fileOb) <- c("id", "nob")
  return(fileOb)
}

