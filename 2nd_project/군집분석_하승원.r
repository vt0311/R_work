getwd()
setwd('C:/work')
library(cluster)

#result <- read.csv('', header=TRUE)
result <- read.csv('c:/work/N06AB_2015_RE.csv', header=TRUE)
head(result)





result$SIDO_CODE2[result$SIDO_CODE == 11 ] <- '������'
result$SIDO_CODE2[result$SIDO_CODE == 26 ] <- '�������'
result$SIDO_CODE2[result$SIDO_CODE == 27 ] <- '�������'
result$SIDO_CODE2[result$SIDO_CODE == 28 ] <- '������'
result$SIDO_CODE2[result$SIDO_CODE == 29 ] <- '�������'
result$SIDO_CODE2[result$SIDO_CODE == 30 ] <- '�������'
result$SIDO_CODE2[result$SIDO_CODE == 31 ] <- '�������'
result$SIDO_CODE2[result$SIDO_CODE == 36 ] <- '�������'
result$SIDO_CODE2[result$SIDO_CODE == 41 ] <- '������'
result$SIDO_CODE2[result$SIDO_CODE == 42 ] <- '�������'
result$SIDO_CODE2[result$SIDO_CODE == 43 ] <- '�������'
result$SIDO_CODE2[result$SIDO_CODE == 44 ] <- '�������'
result$SIDO_CODE2[result$SIDO_CODE == 45 ] <- '�������'
result$SIDO_CODE2[result$SIDO_CODE == 46 ] <- '�������'
result$SIDO_CODE2[result$SIDO_CODE == 47 ] <- '�������'
result$SIDO_CODE2[result$SIDO_CODE == 48 ] <- '�������'
result$SIDO_CODE2[result$SIDO_CODE == 49 ] <- '�������'


library(dplyr)
result1 <- select(result, PRICE, AGE_CODE, SIDO_CODE2)
head(result1)


# ������ �����м� (X)
result2 <- hclust(dist(result1), method='ave')
names(result2)
result2$order
plot(result2, hang=-1, labels=result2$ID)

# ������� �����м� 
result3 <- kmeans(result1, 2)
result3


windows()
plot(result[c("AGE_CODE")], col=result3$cluster)

points(result3$centers[,c('AGE_CODE')], col=c(1,2,3), pch=8, cex=2)



