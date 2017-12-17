# 1599번. 택배 서비스 고객의 군집 분석

#install.packages('cluster')
library(cluster)

# 1.간단한 데이터 분포 현황을 파악하시오.
result <- read.csv('c:/R_work/myRFM.csv', header=TRUE)

head(result)
#   Recency Frequency Monetary
# 1       2        17      976
# 2       1         7      171
# 3       3        23     2322
# 4       3        32     3293
# 5       3        14      426
# 6       2        24      666


# 2.계층적 군집 분석을 이용해서 물류 서비스 고객사들을 그룹핑하시오.

result2 <- hclust(dist(result), method='ave')
names(result2)
# [1] "merge"       "height"      "order"       "labels"      "method"      "call"        "dist.method"
result2$order
# [1] 27 49 56 13 40 53 72 68 31 34  3 74  4 50 64 67 43 18 69  6 10 78 60 12 14 63 71  9 83 44 55 16 88 25 46 29 22 51 62
# [40]  2 57 35 81 32 23 52 37 86  5 38 47 28 87 26 15 54 79 39 48 20 11 17 24 58 85  1 36 70  8 19 33 76 84 45 82 59 65 89
# [79] 66 73 42  7 61 41 77 21 80 30 75

windows()
plot(result2, hang = -1, labels = result2$ID)


# 3.계층적 군집 분석을 토대로 비계층적 군집수를 지정해서 고객사들을 그룹핑하시오.

result3 <- kmeans(result, 3)
result3

# K-means clustering with 3 clusters of sizes 59, 3, 27
# 
# Cluster means:
#   Recency Frequency   Monetary
# 1 1.898305  14.69492   514.5763
# 2 2.000000  98.33333 10352.6667
# 3 2.148148  34.59259  2354.9259
# 
# Clustering vector:
#   [1] 1 1 3 3 1 1 3 1 1 1 1 1 3 1 1 1 1 1 1 1 3 1 1 1 1 1 2 1 1 3 3 1 1 3 1 1 1 1 1 3 3 3 1 1 1 1 1 1 2 3 1 1 3 1 1 2 1 1 3
# [60] 1 3 1 1 3 3 3 3 3 1 1 1 3 3 3 3 1 3 1 1 3 1 1 1 1 1 1 1 1 3
# 
# Within cluster sum of squares by cluster:
#   [1]  7917790.3   669195.3 22281681.8
# (between_SS / total_SS =  91.0 %)
# 
# Available components:
#   
#   [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"   


# 4.비계층적 군집 분석 결과를 활용해 시각화로 구현해 보세요.

windows()
plot(result[c("Frequency", "Monetary")], col=result3$cluster)

points(result3$centers[,c('Frequency','Monetary')], col=c(1,2,3), pch=8, cex=2)
