library(WHO)
library(dplyr)
library(sqldf)
library(cluster)
library(RColorBrewer)
library(plotrix)

#available datasets for mortality and death
  mort <- glob2rx("*mortality*")
  death <- glob2rx("*death*")
  codes <- get_codes()
  
  indexes <- c(grep(mort, codes$display), grep(death, codes$display))
  mortDeathRows <- codes[indexes, ]
  mortDeathLabels <- codes[indexes,1]

#available datasets for causes of death  
  causes <- glob2rx("*auses*")
  index <- grep(causes, codes$display)
  causesRows <- codes[index, ]
  causesLabels <- codes[index, 1]

  
  i <- glob2rx("*opulation*")
  p<- grep(i,  codes$display)
  population <- codes[p,]
  populationData <- get_data("WHS9_86")


#Adult mortality rate (probability of dying between 15 and 60 years per 1000 population)
  mortRatebyRegion <- get_data("WHOSIS_000004")
  mortRatebyRegion <- mortRatebyRegion %>%
    group_by(region) %>%
    summarise(value = mean(value))
  View(mortRatebyRegion)

  mortRate <- get_data("WHOSIS_000004")
  mortRate <- filter(mortRate, country != '')
  
  mortRatebyCountry <- mortRate %>%
    group_by(country) %>%
    summarise(value = mean(value))
  View(mortRatebyCountry)

#Gross national income per capita (PPP int. $) Data is for 20 countries
  income <- get_data("WHS9_93")
  incomebyCountry <- filter(income, country != "")
  incomebyCountry <- filter(incomebyCountry, value != "")
  incomebyCountry$value <- as.numeric(incomebyCountry$value)
  incomebyCountry$value
  sum(incomebyCountry$value)
  incomebyCountry <- incomebyCountry %>%
    group_by(country) %>%
    summarise(value = mean(value))
  View(incomebyCountry)

#Population median age (years)
  sqldf("SELECT display FROM codes WHERE label = 'WHS9_88'")
  
  ageMedian <- get_data("WHS9_88")

#Acquire data in 2 parts
  Mort_part1 <- read.table("Morticd10_part1/Morticd10_part1", header = TRUE, sep = ',')
  Mort_part2 <- read.table("Morticd10_part2/Morticd10_part2", header = TRUE, sep = ',')

#Rename to Mort
  Mort1 <- Mort_part1
  Mort2 <- Mort_part2

#import country codes data
  countryCodes <- read.csv("countryCodes.csv", header = TRUE, sep = ',')
  colnames(countryCodes) <- c('country', 'name')
  
#filter through
  test <- sqldf("SELECT Country FROM Mort1 GROUP BY Country")
  countryList <- sqldf("SELECT name, countryCodes.country FROM countryCodes, test WHERE countryCodes.country = test.Country ")
  

  mortRatebyAvailableCountry <- sqldf("SELECT * FROM countryList AS c, mortRatebyCountry AS m WHERE c.name = m.country ORDER BY value DESC")
  View(mortRatebyAvailableCountry)
  
#Get Cause Codes
  CauseCodes <- read.csv("causeCodes.csv", header = FALSE)
  colnames(CauseCodes) <- c("cause", "name")
  
#Data for South Africa  

  causeperYear <- sqldf("SELECT Country, Year, Cause, Deaths1 FROM SouthAfrica GROUP BY Year, Cause")
  
  #group by year and cause
    CauseperYear <- sqldf("SELECT Name, Year, Cause, Deaths1, Deaths2, Deaths3, Deaths4, Deaths5, Deaths6, Deaths7, Deaths8, Deaths9, Deaths10, Deaths11, Deaths12, Deaths13, Deaths14, Deaths15, Deaths16, Deaths17, Deaths18, Deaths19, Deaths20, Deaths21, Deaths22, Deaths23, Deaths24, Deaths25, Deaths26 FROM SouthAfrica AS s")  
    
    pneumoniaperYearAll <-sqldf("SELECT Year - 1995 AS Year, SUM(Deaths1) AS TotalDeaths, SUM(Deaths2) AS [0 Year], SUM(Deaths3) AS [1 Year], SUM(Deaths4) AS [2 Years], SUM(Deaths5) AS [3 Years], SUM(Deaths6) AS [4 Years], SUM(Deaths7) AS [5-9 Years], SUM(Deaths8) AS [10-14 Years], SUM(Deaths9) AS [15-19 Years], SUM(Deaths10) AS [20-24 Years], SUM(Deaths11) AS [25-29 Years], SUM(Deaths12) AS [30-34 Years], SUM(Deaths13) AS [35-39 Years], SUM(Deaths14) AS [40-44 Years], SUM(Deaths15) AS [45-49 Years], SUM(Deaths16) AS [50-54 Years], SUM(Deaths17) AS [55-59 Years], SUM(Deaths18) AS [60-64 Years], SUM(Deaths19) AS [65-69 Years], SUM(Deaths20) AS [70-74 Years], SUM(Deaths21) AS [75-79 Years], SUM(Deaths22) AS [80-84 Years], SUM(Deaths23) AS [85-89 Years], SUM(Deaths24) AS [90-94 Years], SUM(Deaths25) AS [95+ Years], SUM(Deaths26) AS Unspecified FROM CauseperYear WHERE name LIKE '%neumonia%' GROUP BY Year")
    pneumoniaperYearTotals <- sqldf("SELECT Cause, name, Year - 1995 AS Year, SUM(Deaths1) AS TotalDeathsperYear FROM CauseperYear WHERE name LIKE '%neumon%' GROUP BY Year")

    gastroenteritisperYearAll <-sqldf("SELECT Year, SUM(Deaths1) AS TotalDeaths, SUM(Deaths2) AS [0 Year], SUM(Deaths3) AS [1 Year], SUM(Deaths4) AS [2 Years], SUM(Deaths5) AS [3 Years], SUM(Deaths6) AS [4 Years], SUM(Deaths7) AS [5-9 Years], SUM(Deaths8) AS [10-14 Years], SUM(Deaths9) AS [15-19 Years], SUM(Deaths10) AS [20-24 Years], SUM(Deaths11) AS [25-29 Years], SUM(Deaths12) AS [30-34 Years], SUM(Deaths13) AS [35-39 Years], SUM(Deaths14) AS [40-44 Years], SUM(Deaths15) AS [45-49 Years], SUM(Deaths16) AS [50-54 Years], SUM(Deaths17) AS [55-59 Years], SUM(Deaths18) AS [60-64 Years], SUM(Deaths19) AS [65-69 Years], SUM(Deaths20) AS [70-74 Years], SUM(Deaths21) AS [75-79 Years], SUM(Deaths22) AS [80-84 Years], SUM(Deaths23) AS [85-89 Years], SUM(Deaths24) AS [90-94 Years], SUM(Deaths25) AS [95+ Years], SUM(Deaths26) AS Unspecified FROM CauseperYear WHERE name LIKE '%astroenteriti%' GROUP BY Year")
    gastroenteritisYearTotals <- sqldf("SELECT Cause, name, Year - 1995 AS Year, SUM(Deaths1) AS TotalDeathsperYear FROM CauseperYear WHERE name LIKE '%astroenteriti%' GROUP BY Year")
    
    heartProblemsYearAll <-sqldf("SELECT Year, SUM(Deaths1) AS TotalDeaths, SUM(Deaths2) AS [0 Year], SUM(Deaths3) AS [1 Year], SUM(Deaths4) AS [2 Years], SUM(Deaths5) AS [3 Years], SUM(Deaths6) AS [4 Years], SUM(Deaths7) AS [5-9 Years], SUM(Deaths8) AS [10-14 Years], SUM(Deaths9) AS [15-19 Years], SUM(Deaths10) AS [20-24 Years], SUM(Deaths11) AS [25-29 Years], SUM(Deaths12) AS [30-34 Years], SUM(Deaths13) AS [35-39 Years], SUM(Deaths14) AS [40-44 Years], SUM(Deaths15) AS [45-49 Years], SUM(Deaths16) AS [50-54 Years], SUM(Deaths17) AS [55-59 Years], SUM(Deaths18) AS [60-64 Years], SUM(Deaths19) AS [65-69 Years], SUM(Deaths20) AS [70-74 Years], SUM(Deaths21) AS [75-79 Years], SUM(Deaths22) AS [80-84 Years], SUM(Deaths23) AS [85-89 Years], SUM(Deaths24) AS [90-94 Years], SUM(Deaths25) AS [95+ Years], SUM(Deaths26) AS Unspecified FROM CauseperYear WHERE name LIKE '%eart%' OR name LIKE '%ardio%' GROUP BY Year")
    heartProblemsYearTotals <- sqldf("SELECT Cause, name, Year - 1995 AS Year, SUM(Deaths1) AS TotalDeathsperYear FROM CauseperYear WHERE name LIKE '%Heart%' OR name LIKE '%heart%' OR name LIKE '%ardi%' GROUP BY Year")
    
    neoplasmYearTotals <- sqldf("SELECT Cause, name, Year - 1995 AS Year, SUM(Deaths1) AS TotalDeathsperYear FROM CauseperYear WHERE name LIKE '%eoplas%' GROUP BY Year")
    
  #Median Age
    medianAgeSouthAfrica <- sqldf("SELECT year, value AS Age FROM ageMedian WHERE country = 'South Africa'") 
    View(medianAgeSouthAfrica)
    
  #Adult mortality rate (probability of dying between 15 and 60 years per 100 population)
    mortRateSouthAfrica <- sqldf("SELECT value/10 as [Mortality Rate Percentage] FROM mortRatebyAvailableCountry WHERE name = 'South Africa'")
    View(mortRateSouthAfrica)
    
  #Get Cause occurances for total years
    causeFreqCum <- sqldf("SELECT COUNT(Cause) AS Count, Cause FROM SouthAfrica GROUP BY Cause HAVING Cause IN (SELECT Cause from SouthAfrica where cause in (select cause from CauseCodes)) ORDER BY Cause")
    causeFreqCum[,3] <- sqldf("SELECT cc.name FROM CauseCodes as cc, causeFreqCum AS cfm WHERE cc.cause = cfm.Cause ORDER BY cc.cause")
    View(causeFreqCum)
    
  #Proportion for Cause Occurances as percentage
    totalDeaths <- sum(causeFreqCum[,1])
    
    #group by year and cause
    CauseperYear <- sqldf("SELECT Name, Year, Cause, Deaths1, Deaths2, Deaths3, Deaths4, Deaths5, Deaths6, Deaths7, Deaths8, Deaths9, Deaths10, Deaths11, Deaths12, Deaths13, Deaths14, Deaths15, Deaths16, Deaths17, Deaths18, Deaths19, Deaths20, Deaths21, Deaths22, Deaths23, Deaths24, Deaths25, Deaths26 FROM SouthAfrica AS s")  
    
    pneumoniaperYearAll <-sqldf("SELECT Year - 1995 AS Year, SUM(Deaths1) AS TotalDeaths, SUM(Deaths2) AS [0 Year], SUM(Deaths3) AS [1 Year], SUM(Deaths4) AS [2 Years], SUM(Deaths5) AS [3 Years], SUM(Deaths6) AS [4 Years], SUM(Deaths7) AS [5-9 Years], SUM(Deaths8) AS [10-14 Years], SUM(Deaths9) AS [15-19 Years], SUM(Deaths10) AS [20-24 Years], SUM(Deaths11) AS [25-29 Years], SUM(Deaths12) AS [30-34 Years], SUM(Deaths13) AS [35-39 Years], SUM(Deaths14) AS [40-44 Years], SUM(Deaths15) AS [45-49 Years], SUM(Deaths16) AS [50-54 Years], SUM(Deaths17) AS [55-59 Years], SUM(Deaths18) AS [60-64 Years], SUM(Deaths19) AS [65-69 Years], SUM(Deaths20) AS [70-74 Years], SUM(Deaths21) AS [75-79 Years], SUM(Deaths22) AS [80-84 Years], SUM(Deaths23) AS [85-89 Years], SUM(Deaths24) AS [90-94 Years], SUM(Deaths25) AS [95+ Years], SUM(Deaths26) AS Unspecified FROM CauseperYear WHERE name LIKE '%neumonia%' GROUP BY Year")
    pneumoniaperYearTotals <- sqldf("SELECT Cause, name, Year - 1995 AS Year, SUM(Deaths1) AS TotalDeathsperYear FROM CauseperYear WHERE name LIKE '%neumon%' GROUP BY Year")
    
    gastroenteritisperYearAll <-sqldf("SELECT Year, SUM(Deaths1) AS TotalDeaths, SUM(Deaths2) AS [0 Year], SUM(Deaths3) AS [1 Year], SUM(Deaths4) AS [2 Years], SUM(Deaths5) AS [3 Years], SUM(Deaths6) AS [4 Years], SUM(Deaths7) AS [5-9 Years], SUM(Deaths8) AS [10-14 Years], SUM(Deaths9) AS [15-19 Years], SUM(Deaths10) AS [20-24 Years], SUM(Deaths11) AS [25-29 Years], SUM(Deaths12) AS [30-34 Years], SUM(Deaths13) AS [35-39 Years], SUM(Deaths14) AS [40-44 Years], SUM(Deaths15) AS [45-49 Years], SUM(Deaths16) AS [50-54 Years], SUM(Deaths17) AS [55-59 Years], SUM(Deaths18) AS [60-64 Years], SUM(Deaths19) AS [65-69 Years], SUM(Deaths20) AS [70-74 Years], SUM(Deaths21) AS [75-79 Years], SUM(Deaths22) AS [80-84 Years], SUM(Deaths23) AS [85-89 Years], SUM(Deaths24) AS [90-94 Years], SUM(Deaths25) AS [95+ Years], SUM(Deaths26) AS Unspecified FROM CauseperYear WHERE name LIKE '%astroenteriti%' GROUP BY Year")
    gastroenteritisYearTotals <- sqldf("SELECT Cause, name, Year - 1995 AS Year, SUM(Deaths1) AS TotalDeathsperYear FROM CauseperYear WHERE name LIKE '%astroenteriti%' GROUP BY Year")
    
    heartProblemsYearAll <-sqldf("SELECT Year, SUM(Deaths1) AS TotalDeaths, SUM(Deaths2) AS [0 Year], SUM(Deaths3) AS [1 Year], SUM(Deaths4) AS [2 Years], SUM(Deaths5) AS [3 Years], SUM(Deaths6) AS [4 Years], SUM(Deaths7) AS [5-9 Years], SUM(Deaths8) AS [10-14 Years], SUM(Deaths9) AS [15-19 Years], SUM(Deaths10) AS [20-24 Years], SUM(Deaths11) AS [25-29 Years], SUM(Deaths12) AS [30-34 Years], SUM(Deaths13) AS [35-39 Years], SUM(Deaths14) AS [40-44 Years], SUM(Deaths15) AS [45-49 Years], SUM(Deaths16) AS [50-54 Years], SUM(Deaths17) AS [55-59 Years], SUM(Deaths18) AS [60-64 Years], SUM(Deaths19) AS [65-69 Years], SUM(Deaths20) AS [70-74 Years], SUM(Deaths21) AS [75-79 Years], SUM(Deaths22) AS [80-84 Years], SUM(Deaths23) AS [85-89 Years], SUM(Deaths24) AS [90-94 Years], SUM(Deaths25) AS [95+ Years], SUM(Deaths26) AS Unspecified FROM CauseperYear WHERE name LIKE '%eart%' OR name LIKE '%ardio%' GROUP BY Year")
    heartProblemsYearTotals <- sqldf("SELECT Cause, name, Year - 1995 AS Year, SUM(Deaths1) AS TotalDeathsperYear FROM CauseperYear WHERE name LIKE '%Heart%' OR name LIKE '%heart%' OR name LIKE '%ardi%' GROUP BY Year")
    
    neoplasmYearTotals <- sqldf("SELECT Cause, name, Year - 1995 AS Year, SUM(Deaths1) AS TotalDeathsperYear FROM CauseperYear WHERE name LIKE '%eoplas%' GROUP BY Year")
    
    rnames <- c('Year 1', 'Year 2', 'Year 3', 'Year 4', 'Year 5', 'Year 6', 'Year 7', 'Year 8', 'Year 9')
    
    top4 <- data.frame(Year = c(1,2,3,4,5,6,7,8,9), totalDeath = totalDeathsperYearSouthAfrica$DeathsPerYear, Pneumonia = pneumoniaperYearTotals$TotalDeathsperYear, HeartProblems = heartProblemsYearTotals$TotalDeathsperYear, Neoplasm = neoplasmYearTotals$TotalDeathsperYear, Gastroenteritis = gastroenteritisYearTotals$TotalDeathsperYear, row.names = rnames)
  
    top4.lm <- lm(totalDeath ~ Pneumonia,data = top4)
    summary(top4.lm)
    add1(top4.lm, scope = top4)
    top4.lm <- lm(totalDeath ~ Pneumonia + Neoplasm, data = top4)
    summary(top4.lm)    
    add1(top4.lm, scope = top4)
    top4.lm <- lm(totalDeath ~ Pneumonia + Neoplasm + Gastroenteritis, data = top4)    
    summary(top4.lm)
    
    top4.pneumonia.lm <- lm(Pneumonia ~ Year, data = top4)
    summary(top4.pneumonia.lm)
    predict.lm(top4.pneumonia.lm)
    
    
    top4.pneumonia.lm$coefficients
    
    pneumoniaregression <- data.frame(Years = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18), Deaths = c(3591.806,9279.023,14966.24,20653.46,26340.67,32027.89,37715.11,43402.33,49089.54,54776.76,60463.98,66151.19,71838.41,77525.63,83212.84,88900.06,94587.28,100274.5,105961.7))
    
    theoreticalpneumoniaregression <- data.frame(Years = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18), Deaths = c(6591.806,9279.023,14966.24,20653.46,26340.67,32027.89,37715.11,43402.33,49089.54,54776.76,49089.54,43402.33,37715.11,32027.89,26340.67,20653.46,14966.24,9279.023,3591.806))
    
    ggplot() +
      geom_point(mapping = aes(x = pneumoniaperYearTotals$Year, y = pneumoniaperYearTotals$TotalDeathsperYear)) +
      geom_line(mapping = aes(pneumoniaregression$Years,pneumoniaregression$Deaths), col = 'red') +
      geom_smooth(method = "lm", x = pneumoniaperYearTotals$Year, y = pneumoniaperYearTotals$TotalDeathsperYear) +
      geom_point(mapping = aes(pneumoniaregression$Years,pneumoniaregression$Deaths), col = 'red') +
      geom_smooth(mapping = aes(x = theoreticalpneumoniaregression$Years, y = theoreticalpneumoniaregression$Deaths)) +
      geom_point(mapping = aes(theoreticalpneumoniaregression$Years, y = theoreticalpneumoniaregression$Deaths), col= 'green3')  +
      theme_minimal()
        
    ggplot(fw, mapping = aes(x = speed, y = count)) +
      
      geom_point() +
      
      geom_smooth(method = "lm")
    
  #Deaths per year
    totalDeathsperYearSouthAfrica <- sqldf("SELECT Year, SUM(Deaths2) + SUM(Deaths3)+SUM(Deaths4)+SUM(Deaths5)+SUM(Deaths6)+SUM(Deaths7)+SUM(Deaths8)+SUM(Deaths9) +  SUM(Deaths10)+SUM(Deaths11)+SUM(Deaths12)+SUM(Deaths13)+SUM(Deaths14)+SUM(Deaths15)+SUM(Deaths16)+SUM(Deaths17)+SUM(Deaths18)+SUM(Deaths19)+SUM(Deaths20)+SUM(Deaths21)+SUM(Deaths22)+SUM(Deaths23)+SUM(Deaths24)+SUM(Deaths25)+SUM(Deaths26) AS DeathsPerYear FROM SouthAfrica GROUP BY Year ORDER BY Year")
    View(totalDeathsperYearSouthAfrica)
    plot(totalDeathsperYearSouthAfrica, type = 'l', ylab = 'Mortality Occurances', col = 'red')
    title(main = 'South Africa Mortality')
    
    #Deaths per year by age range
      ageRangeDeathsperYearSouthAfrica <- sqldf("SELECT Year, SUM(Deaths1) AS Total, SUM(Deaths2) AS [0 Year], SUM(Deaths3) AS [1 Year], SUM(Deaths4) AS [2 Years], SUM(Deaths5) AS [3 Years], SUM(Deaths6) AS [4 Years], SUM(Deaths7) AS [5-9 Years], SUM(Deaths8) AS [10-14 Years], SUM(Deaths9) AS [15-19 Years], SUM(Deaths10) AS [20-24 Years], SUM(Deaths11) AS [25-29 Years], SUM(Deaths12) AS [30-34 Years], SUM(Deaths13) AS [35-39 Years], SUM(Deaths14) AS [40-44 Years], SUM(Deaths15) AS [45-49 Years], SUM(Deaths16) AS [50-54 Years], SUM(Deaths17) AS [55-59 Years], SUM(Deaths18) AS [60-64 Years], SUM(Deaths19) AS [65-69 Years], SUM(Deaths20) AS [70-74 Years], SUM(Deaths21) AS [75-79 Years], SUM(Deaths22) AS [80-84 Years], SUM(Deaths23) AS [85-89 Years], SUM(Deaths24) AS [90-94 Years], SUM(Deaths25) AS [95+ Years], SUM(Deaths26) AS Unspecified FROM SouthAfrica GROUP BY Year")    
      barplot(ageRangeDeathsperYearSouthAfrica$`25-29 Years`,main = '25-29 Year Olds', xlab = 'Year', ylab = 'Number of Deaths', names.arg = ageRangeDeathsperYearSouthAfrica$Year)
      lines(ageRangeDeathsperYearSouthAfrica$`25-29 Years`, col = 'red')
      abline(h = 0)
      
      barplot(ageRangeDeathsperYearSouthAfrica$`30-34 Years`,main = '30-34 Year Olds', xlab = 'Year', ylab = 'Number of Deaths', names.arg = ageRangeDeathsperYearSouthAfrica$Year)
      lines(ageRangeDeathsperYearSouthAfrica$`30-34 Years`, col = 'red')
      abline(h = 0)
      
      barplot(ageRangeDeathsperYearSouthAfrica$`35-39 Years`,main = '35-39 Year Olds', xlab = 'Year', ylab = 'Number of Deaths', names.arg = ageRangeDeathsperYearSouthAfrica$Year)
      lines(ageRangeDeathsperYearSouthAfrica$`35-39 Years`, col = 'red')
      abline(h = 0)
      
      barplot(ageRangeDeathsperYearSouthAfrica$`40-44 Years`,main = '40-44 Year Olds', xlab = 'Year', ylab = 'Number of Deaths', names.arg = ageRangeDeathsperYearSouthAfrica$Year)
      lines(ageRangeDeathsperYearSouthAfrica$`40-44 Years`, col = 'red')
      abline(h = 0)
      
      #Switch rows and columns
        normData <- as.data.frame(matrix(ncol = 9))
        
        for (i in 2:27) {
          normData[i,] <- ageRangeDeathsperYear[,i]
        }
                
        colnames(normData) <- ageRangeDeathsperYear[,1]
        rownames(normData) <- colnames(ageRangeDeathsperYear)
        normData <- normData[-c(1,1),]
        
        #Scale and Normalize Deaths per year by age range        
          m <- apply(normData, 2, mean)
          s <- apply(normData, 2, sd)
          normData2 <- scale(normData, m, s)
          normData2 <- normData[-c(1,1),]
          
        #Euclidean distance        
          distance <- dist(normData2)
        #Cluster Dendrogram
          hc.c <- hclust(distance)
          plot(hc.c)        
          hc.a <- hclust(distance, method = 'average')
          plot(hc.a)
          member.c <- cutree(hc.c,4)
          member.a <- cutree(hc.a,4)
          table(member.c,member.a)

        #Scree Plot
          wss <- (nrow(normData2)-1)*sum(apply(normData2,2,var))
          for (i in 2:20) wss[i] <- sum(kmeans(normData2, centers = i)$withinss)
          plot(1:20, wss, type = 'b', xlab = 'Number of Clusters', ylab = 'Within group SS')
          
        #K-Means Cluster
          kc <- kmeans(normData2, 4)
          #plot kmeans cluster
            clusplot(normData2, kc$cluster, main = 'Grouped Ages in Numbers of Deaths', color = TRUE, shade = TRUE, lines = 0, labels = 2, cex = .5) 
          kc$size; kc$cluster; kc$withinss
          
    plot(ageRangeDeathsperYearSouthAfrica$`0 Year`)
  #Multiple regression by age
    deathsperYearData <- ageRangeDeathsperYearSouthAfrica
    colnames(deathsperYearData) <- c('Year','Total','Year0','Year1','Year2','Year3','Year4','Years59','Years1014','Years1519','Years2024','Years2529','Years3034','Years3539','Years4044','Years4549','Years5054','Years5559','Years6064','Years6569','Years7074','Years7579','Years8084','Years8589','Years9094','Years95+','Unspecified')
    deathsperYearData[,1] <- c(1,2,3,4,5,6,7,8,9)
    deathsperYearData.lm <- lm(Year ~ Years6064, data = deathsperYearData)
    summary(deathsperYearData.lm)      
    
    deathsperYearData <- deathsperYearData[,-c(2)]
    ggplot(data = normData) +
      geom_line(x = normData$`1996`)
    s <- c()
    count <- 1
  for (i in SouthAfrica$Cause) {
    if (i %in% CauseCodes[,1]) {
      s[count] <- i
      count <- count + 1
    } 
  }   
s <- unique(s)

View(SouthAfrica)
?filter
