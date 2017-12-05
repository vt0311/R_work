# 1599번. 택배 서비스 고객의 군집 분석

# 1.간단한 데이터 분포 현황을 파악하시오.
data <- read.csv('c:/work/myRFM.csv')

head(data)

# 2.계층적 군집 분석을 이용해서 물류 서비스 고객사들을 그룹핑하시오.
data <- data
# 3.계층적 군집 분석을 토대로 비계층적 군집수를 지정해서 고객사들을 그룹핑하시오.

# 4.비계층적 군집 분석 결과를 활용해 시각화로 구현해 보세요.

> data2 <- data1[c('Recency','Frequency','Monetary')]
> result <- hclust(dist(data2), method='ave')
> result2 <- kmeans(data2, 3)
> data2$cluster <- result2$cluster
> cor(data2[,-5],method='pearson')
> plot(data2$Frequency, data2$Monetary, col=data2$cluster)
> points(result2$centers[,c('Frequency','Monetary')], col=c(1,2,3), pch=8, cex=2)