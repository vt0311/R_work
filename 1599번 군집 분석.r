# 1599��. �ù� ���� ���� ���� �м�

# 1.������ ������ ���� ��Ȳ�� �ľ��Ͻÿ�.
data <- read.csv('c:/work/myRFM.csv')

head(data)

# 2.������ ���� �м��� �̿��ؼ� ���� ���� ������� �׷����Ͻÿ�.
data <- data
# 3.������ ���� �м��� ���� ������� �������� �����ؼ� ������� �׷����Ͻÿ�.

# 4.������� ���� �м� ����� Ȱ���� �ð�ȭ�� ������ ������.

> data2 <- data1[c('Recency','Frequency','Monetary')]
> result <- hclust(dist(data2), method='ave')
> result2 <- kmeans(data2, 3)
> data2$cluster <- result2$cluster
> cor(data2[,-5],method='pearson')
> plot(data2$Frequency, data2$Monetary, col=data2$cluster)
> points(result2$centers[,c('Frequency','Monetary')], col=c(1,2,3), pch=8, cex=2)