
dialectTable <- function(file = file.choose(), zscore = 2.326){
  #Produces a data frame from csv file with the Word, Frequency, Dictionary Website, 
  #Proportion, Margin of Error, and Confidence Intervals
  
  #The Confidence Intervals are produced from the following formula:
  #p +- zscore * sqrt(p(1-p)/n)
  
  #the default zscore is 2.326 at 99%
  #p is the proportion
  
  library(dplyr)
  library(sqldf)  
  
  data <- read.csv(file)
  n <- sum(data[,2])
  for (i in 1:length(data[,1])) {
    data[i,4] <- data[i,2] / sum(data[,2])
    data[i,5] <- zscore * sqrt(data[i,4] * (1-data[i,4]) / n)
    data[i,6] <- data[i,4] - data[i,5]
    data[i,7] <- data[i,4] + data[i,5]
  }
  colnames(data) <- c("Word", "Frequency", "DictionaryWebsite", "Proportion", "MarginOfError", "ConfidenceLow", "ConfidenceHigh")
  data <- sqldf("SELECT Word, Frequency, DictionaryWebsite, Proportion, MarginOfError, ConfidenceLow, ConfidenceHigh FROM data ORDER BY Proportion DESC")
  data
}

write.csv(dialectTable(), file ="C://Users//willc//Desktop//New folder//CenterForHumanRights//AlzheimersNetworkwordsDict.csv")

count <- 1
for (i in dialectTable(file = 'VietnameseVoicewordsDict.csv')){
  if (i[6] > 0){
    total[count, ] <- i
    count <- count + 1
  }
}