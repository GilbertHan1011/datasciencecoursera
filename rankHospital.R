
library(dplyr)
library(stringr)
best <- function(state, outcomeName) {
     tOutcomeName <- outcomeName%>% strsplit(.,"\\s+")%>%.[[1]]%>%str_to_sentence()%>%paste(.,collapse = ".")
     header <- paste("Hospital.30.Day.Death..Mortality..Rates.from",tOutcomeName,sep = ".")
     conditions <- outcomeDf[c(header,"Hospital.Name","State")]
     sortDf <- subset(conditions, State==state )
     sortDf <- sortDf[sortDf[,1] != "Not Available",]
     sortDf[,1] <- as.numeric(sortDf[,1])
     result <- sortDf%>%
         arrange(.,.[,1],Hospital.Name)
     result[1,2]
   }

rankhospital <- function(state, outcomeName,num) {
  tOutcomeName <- outcomeName%>% strsplit(.,"\\s+")%>%.[[1]]%>%str_to_sentence()%>%paste(.,collapse = ".")
  header <- paste("Hospital.30.Day.Death..Mortality..Rates.from",tOutcomeName,sep = ".")
  conditions <- outcomeDf[c(header,"Hospital.Name","State")]
  sortDf <- subset(conditions, State==state )
  sortDf <- sortDf[sortDf[,1] != "Not Available",]
  sortDf[,1] <- as.numeric(sortDf[,1])
  result <- sortDf%>%
    arrange(.,.[,1],Hospital.Name)%>% 
    data.frame(., "Rank"=1:nrow(sortDf))
  if (num == "best"){
    return(result[1,2])
  }
  else if(num == "worst"){
    return(result[nrow(result),2])
  }
  else {
    return(result[num,2])
  }
}


rankall <- function(outcomeName,num="best") {
  tOutcomeName <- outcomeName%>% strsplit(.,"\\s+")%>%.[[1]]%>%str_to_sentence()%>%paste(.,collapse = ".")
  header <- paste("Hospital.30.Day.Death..Mortality..Rates.from",tOutcomeName,sep = ".")
  conditions <- outcomeDf[c(header,"Hospital.Name","State")]
  sortDf <- conditions[conditions[,1] != "Not Available",]
  sortDf[,1] <- as.numeric(sortDf[,1])
  findOrder <- function(df,state,num=num){
    df <- subset(df, State==state )
    result <- df%>%
      arrange(.,.[,1],Hospital.Name)%>% 
      data.frame(., "Rank"=1:nrow(df))
    if (num == "best"){
      resultone <- result[1,2]
    }
    else if(num == "worst"){
      resultone<-result[nrow(result),2]
    }
    else {
      resultone<-result[num,2]
    }
  }
  resultDf <- sapply(unique(sortDf$State),function(x) findOrder(df=sortDf, state = x,num = num))
  return(as.data.frame(resultDf))
}
