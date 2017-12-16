# 1599��. �ù� ���� ���� ���� �м�

#install.packages('cluster')
library(cluster)

# 1.������ ������ ���� ��Ȳ�� �ľ��Ͻÿ�.
result <- read.csv('c:/R_work/myRFM.csv', header=TRUE)

head(result)
#   Recency Frequency Monetary
# 1       2        17      976
# 2       1         7      171
# 3       3        23     2322
# 4       3        32     3293
# 5       3        14      426
# 6       2        24      666


# 2.������ ���� �м��� �̿��ؼ� ���� ���� ������� �׷����Ͻÿ�.

result2 <- hclust(dist(result), method='ave')
names(result2)
result2$order

windows()
plot(result2, hang = -1, labels = result2$ID)


# 3.������ ���� �м��� ���� ������� �������� �����ؼ� ������� �׷����Ͻÿ�.

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


# 4.������� ���� �м� ����� Ȱ���� �ð�ȭ�� ������ ������.

windows()
plot(result[c("Frequency", "Monetary")], col=result3$cluster)

points(result3$centers[,c('Frequency','Monetary')], col=c(1,2,3), pch=8, cex=2)
