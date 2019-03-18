
dialectTable <- function(file = file.choose(), zscore = 2.32635){
  #Acquires proportion of individual word usage, margin of error, and confidence low and high interval with a default probability of 
  #99% with the formula p +-z*squareroot(p(1-p)/n)
  
  library(dplyr)
  
  data <- read.csv(file)
  n <- sum(data[,2])
  for (i in 1:length(data[,1])) {
    data[i,4] <- data[i,2] / sum(data[,2])
    data[i,5] <- zscore * sqrt(data[i,4] * (1-data[i,4]) / n)
    data[i,6] <- data[i,4] - data[i,5]
    data[i,7] <- data[i,4] + data[i,5]
  }
  colnames(data) <- c("Word", "Frequency", "DictionaryWebsite", "Proportion", "MarginOfError", "ConfidenceLow", "ConfidenceHigh")
  data
}

differentWordUsage <- function (entity1 = file.choose(), entity2 = file.choose()) {  
  #Compares 2 data sets with the words and the frequencies of those words and produces a similarity percentage based 
  #on whether the low confidence interval is below 0 and whether the confidence intervals overlap
  
  library(dplyr)
  library(sqldf)
  
  print(entity1)
  print(entity2)
  
  data1 <- dialectTable(entity1)
  data2 <- dialectTable(entity2)
  print(paste0("There are ", sum(data1[,2]), " words"))
  print(paste0("There are ", sum(data2[,2]), " words"))
  
  
  #Produces warning if either data set is under 1,000 words  
  if (sum(data1[,2]) < 1000) {
    print("Warning data1 has less than 1,000 words!")
    print(paste0("There are ", sum(data1[,2]), " words!"))
  }
  if (sum(data2[,2]) < 1000) {
    print("Warning data2 has less than 1,000 words!")
    print(paste0("There are ", sum(data2[,2]), " words!"))
  }
  
  x <- data.frame(matrix(ncol = 4))
  
  #Evaluates datasets to see if the words are listed in both datasets, also filters out words in data1 
  #if confidence low is below zero
  for (i in 1:length(data1[,1])) {
    if (data1[i,1] %in% data2[,1] && data1[i,6] > 0) {
      x[i,] <- data.frame(as.character(data1[i,1]), data1[i,2], data1[i,6], data1[i,7], stringsAsFactors = FALSE)
      
      #returns nothing but filters out rest of common words
    } else if (data1[i,1] %in% data2[,1]) {
    } 
      else if (data1[i,6] > 0) {
      x[i,] <- data.frame(as.character(data1[i,1]), data1[i,2], data1[i,6], data1[i,7], stringsAsFactors = FALSE)
    }
  }
  
  #Removes the NAs from data.frame
  x <- x[complete.cases(x),1:4]
  
  #Uses SQL to produce 2 tables one, table z, produces table with each word in both datasets with word
  #, frequency, confidence intervals, porportion, etc. Table q, produces table with each word only in 
  #data1 and the confidence low above 0
  z <-sqldf("SELECT DISTINCT * FROM data1, data2 WHERE data1.Word = data2.Word AND data1.ConfidenceLow > 0")
  q <- sqldf("SELECT DISTINCT * FROM data1 WHERE Word NOT IN (SELECT Word FROM data2) AND ConfidenceLow > 0")

  different <- data.frame(matrix(ncol = 14))
  same <- data.frame(matrix(ncol = 14))
  #Produces difference of word usage from words in both datasets
  for (i in 1:length(z[,1])) {
    if (z[i,6] <= z[i,14]) {
      same[i,] <- z[i,]     
    } else {
      different[i,] <- z[i,]
    }
  }
  
  for (i in 1:length(q[,1])) {
    different[i + length(z[,1]),] <- c(q[i,],0,0,0,0,0,0,0)
  }
  
  #Orders different by greatest difference in confidence interval
  different <- sqldf("SELECT X1 AS D1Word, (X6 - X14)*1000 AS differenceStrength, X2 AS D1Frequency, X4 AS D1Proportion, X5 AS D1MarginOfError, X6 AS D1ConfidenceLow, X7 AS D1ConfidenceHigh, X9 AS D2Frequency, X11 AS D2Proportion, X12 AS D2MarginOfError, X13 AS D2ConfidenceLow, X14 AS D2ConfidenceHigh   FROM different WHERE NOT D1Word = 'NA' ORDER BY (X6 - X14) DESC")
  return(different)
}
write.csv(differentWordUsage(), file = "C://Users//willc//Desktop//New folder//CenterForHumanRights//VietnameseVoicvHRC.csv")
