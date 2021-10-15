#== R programming program week2
#== Gilbert
#== 10.16.2021

pollutantmean <- function(directory,pollutant, id = 1:332){
  require(tidyverse)
  files <- list.files(directory,full.names = T)
  fileSelect <- files[id]
  fileList <- map(fileSelect,read.csv)
  fileBind <- do.call(rbind,fileList)
  choosen <- fileBind[[pollutant]]
  choosen <- na.omit(choosen)
  mean(choosen)
}

  