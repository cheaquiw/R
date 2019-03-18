
gretchen <- "gretchenwordsDict.csv"

comparisonTable <- function(entity = "HRC_WordswordsDict.csv", groups = groupsCompare){
  individualComparison <- data.frame(matrix(nrow = length(groups)))
  
  for (i in colnames(groups)) {
    individualComparison[i,] <- dialectComparison(entity1 = paste0("C://Users//willc//Desktop//New folder//", entity), entity2 = paste0("C://Users//willc//Desktop//New folder//",i))
  }
  individualComparison <- individualComparison[-(1:29),]
  return(individualComparison)
}

comparisonTable <- function(entity = "HRC_WordswordsDict.csv", groups = groupsCompare){
  individualComparison <- data.frame(matrix(nrow = length(groups)))
  
  for (i in colnames(groups)) {
    individualComparison[i,] <- dialectComparison(entity1 = paste0("C://Users//willc//Desktop//New folder//", i), entity2 = paste0("C://Users//willc//Desktop//New folder//", entity))
  }
  individualComparison <- individualComparison[-(1:29),]
  return(individualComparison)
}
write.csv(differentWordUsage(), file = "C://Users//willc//Desktop//New folder//CenterForHumanRights//HRC_WordsVOregonBlackPioneers.csv")

write.csv(differentWordUsage(), file = "C://Users//willc//Desktop//New folder//CenterForHumanRights//GretchenVwordsDict.csv")
