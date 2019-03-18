correlations <- function() {
  #Produces a correlation table with each group by each group
  
  library(dplyr)
  
  b <- colnames(groupsCompare)
  
  name <- gsub("words.*","",b)
  
  correlations <- data.frame(matrix(ncol = length(files), nrow = length(files), dimnames = list(name, name)))
  
  for (x in 1:length(groupsCompare)) {
    for (i in 1:length(groupsCompare)) {
      correlations[i,x] <- cor(groupsCompare[,x], groupsCompare[,i])
    }
  }
  
  correlations
}
View(correlations())