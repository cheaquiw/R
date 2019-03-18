
dialectComparison <- function (entity1 = file.choose(), entity2 = file.choose()) {  
  #Compares 2 data sets with the words and the frequencies of those words and produces a similarity percentage based 
  #on whether the low confidence interval is below 0 and whether the confidence intervals overlap
  
  library(dplyr)
  library(sqldf)
  
  print(entity1)
  print(entity2)
  
  data1 <- dialectTable(entity1)
  data2 <- dialectTable(entity2)

  print(paste0("Data1 has ", sum(data1[,2]), " words"))
  print(paste0("Data2 has ", sum(data2[,2]), " words"))
  
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
      
      #Does nothing but filters out rest of common words
    } #else if (data1[i,1] %in% data2[,1]) {
    #} 
      #Adds the remaining words from data1 with confidence level above 0
      else if (data1[i,6] > 0) {
      x[i,] <- data.frame(as.character(data1[i,1]), data1[i,2], data1[i,6], data1[i,7], stringsAsFactors = FALSE)
    }
  }
  
  #Removes the NAs from data.frame
  x <- x[complete.cases(x),1:4]
  
  #Initializes the similarity counter
  sim <- 0

  #Uses SQL to produce 2 tables. Table z, produces table with each word in both datasets with word,
  #frequency, confidence intervals, porportion, etc. Table q, produces table with each word only in 
  #data1 and the confidence low above 0
  z <-sqldf("SELECT DISTINCT * FROM data1, data2 WHERE data1.Word = data2.Word AND data1.ConfidenceLow > 0")
  q <- sqldf("SELECT DISTINCT * FROM data1 WHERE Word NOT IN (SELECT Word FROM data2) AND ConfidenceLow > 0")

  #Produces a similarity counter if the confidence low of data1 is below the confidence high of data2
  for (i in 1:length(z[,1])) {
    if (z[i,6] <= z[i,14]) {
      sim <- sim + 1    
    } 
  }
  
  #Produces total significant words (words where the confidence low is above 0)
  total <- length(z[,1]) + length(q[,1])
  
  #Produces final result with similarity percentage with a confidence level of 99%
  result <- round(sim/total, digits = 4)
  print(result)
  return(result)
}
  
compareAllGroups <- function() {
  library(dplyr)
  
  #Acquires all datasets from current directory
  files <- list()
  allfiles <- list.files()
  
  #Filters through the files and only adds the files with the correct format and over 1,000 words.
  count <- 1
  for (file in allfiles) tryCatch({
    if (length(dialectTable(file)[,1]) > 1000 ) {
      files[count] <- file
      count <- count + 1
    }
  }, error=function(e){})
  files
  View(groupsCompare)
  
  #Initiates empty dataframe with group x group matrix
  groupsCompare <- data.frame(matrix(ncol = length(files), nrow = length(files), dimnames = list(files, files)))
  
  #iterates through files and runs compare function on each file and than compares that file with every file
  for (file in files) {
    for (i in files) tryCatch({
      groupsCompare[i,file] <- compare(file,i) 
    }, error=function(e){})
  }
  groupsCompare <- groupsCompare[-c(1,1),]
  groupsCompare <- groupsCompare[,-c(1,1)]
  
  for (i in 1:length(groupsCompare[,1])) {
    groupsCompare[i,i] <- NA
  }
  
  groupsCompare
}