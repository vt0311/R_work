getwd()
setwd('C:/work')
library(cluster)

#result <- read.csv('', header=TRUE)
result <- read.csv('c:/work/N06AB_2015_RE.csv', header=TRUE)
head(result)

library(dplyr)
result1 <- select(result, RECUPERATE_DATE, DOSE_DAYS, PRICE)
head(result1)
result2 <- hclust(dist(result1), method='ave')
names(result2)
result2$order
plot(result2, hang=-1, labels=result2$ID)


