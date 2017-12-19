getwd()
setwd('C:/work')
library(cluster)

#result <- read.csv('', header=TRUE)
result <- read.csv('c:/work/N06AB_2015_RE.csv', header=TRUE)
head(result)





result$SIDO_CODE2[result$SIDO_CODE == 11 ] <- '수도권'
result$SIDO_CODE2[result$SIDO_CODE == 26 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 27 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 28 ] <- '수도권'
result$SIDO_CODE2[result$SIDO_CODE == 29 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 30 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 31 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 36 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 41 ] <- '수도권'
result$SIDO_CODE2[result$SIDO_CODE == 42 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 43 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 44 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 45 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 46 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 47 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 48 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 49 ] <- '비수도권'


library(dplyr)
result1 <- select(result, PRICE, AGE_CODE, SIDO_CODE2)
head(result1)


# 계층적 군집분석 (X)
result2 <- hclust(dist(result1), method='ave')
names(result2)
result2$order
plot(result2, hang=-1, labels=result2$ID)

# 비계층적 군집분석 
result3 <- kmeans(result1, 2)
result3


windows()
plot(result[c("AGE_CODE")], col=result3$cluster)

points(result3$centers[,c('AGE_CODE')], col=c(1,2,3), pch=8, cex=2)



