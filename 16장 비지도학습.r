# 1312 ��. ��Ŭ���� �Ÿ�
# �Ÿ����ϱ� �ǽ�

# matrix ��ü ����
x <- matrix(1:9, nrow=3, by=T)
x

# ��Ŭ���� �Ÿ� ����
dist <- dist(x, method='euclidean')
dist

# ���� �Լ��� ������� �ʰ� R�� ����Ѵٸ� ������ ���� �ڵ��ϸ� �ȴ�.
# 1��� 2���� ������ ��Ŭ���� �Ÿ� ���ϱ�

result <- sqrt(sum( (x[1,] - x[2,])^2 ))
result

sqrt(sum( (4-1)^2 + (5-2)^2 + (6-3)^2 ))


## 1314��
# ������ ���� �м�

install.packages('cluster')
library(cluster)

x = matrix(1:9, nrow=3, by=T)
x

dist=dist(x, method='euclidean')
dist

hc = hclust(dist)
hc

windows()
plot(hc)


# 1315��. ���б� 1�г� ��ü�˻� ��� ���� �м�
setwd('C:\\work')
body = read.csv("bodycheck.csv", header=TRUE)
head(body)

# �л��� �� 15���̴�.
# ��ȣ �÷��� ������ ������ �÷��鿡 ���� ��Ŭ���� �Ÿ��� �����Ѵ�.
ucldist = dist(body[, -1])

# -- Ŭ�����͸�
hc = hclust(ucldist)
hc

# hang �Ӽ��� ���� -1�� �����ϸ� ����α׷����� �������� ���ܽ�ų�� �ִ�.
# ������ �����ͳ��� ������ �����Ѵ�.
windows()
plot(hc, hang=-1)

# ���� ������ �׵θ� �����
# 3�� �������� �׷���(k=3)�Ѵ�.
rect.hclust(hc, k=3, border="red")

# ������ Ư�� �ľ��ϱ�
# ������ ������� ���Ͽ�, ��� ��跮�� �̿��� ������ Ư¡���� ���캸��.

# �� �׷캰 ����� �����
g1= subset(body, ��ȣ==10 | ��ȣ==4 | ��ȣ==8 | ��ȣ==1  | ��ȣ==15)
g2= subset(body, ��ȣ==11 | ��ȣ==3 | ��ȣ==5 | ��ȣ==6  | ��ȣ==14)
g3= subset(body, ��ȣ==2 | ��ȣ==9 | ��ȣ==13 | ��ȣ==7  | ��ȣ==12)


# ������ ��� ����
g1[2:5]
#    �Ƿ� ���� ü�� �Ȱ�����
# 1    28  146   34        1
# 4    25  156   38        1
# 8    23  153   40        1
# 10   27  152   39        1
# 15   25  142   32        1

g2[2:5]




# 1592��. ������ �ڸ���
head(iris[1:4])

idist <- dist(iris[1:4])

# ������ ���� �м�(Ŭ�����͸�)
hc <- hclust(idist)
hc

plot(hc, hang= -1)
ghc <- cutree(hc, k=3)
ghc
# 150���� ����ġ�� ������� 3���� �������� �����Ͽ� ������ �ǹ��ϴ� ����(1~3)�� ����� �ȴ�.

iris$ghc <- ghc
table(iris$ghc) # �󵵼�

# ��� ��跮 ���ϱ�
g1 <- subset(iris, ghc ==1)
summary(g1[1:4])

g2 <- subset(iris, ghc ==2)
summary(g2[1:4])

g3 <- subset(iris, ghc ==3)
summary(g1[1:4])

# iris �����ͼ��� ������� ������ ���� �м����� ���� ���� �ľ��� �� ���ϴ� ��������ŭ
# ���������� �߶� ������ �����ϰ�, �� �������� ��� ��跮�� ���Ͽ� ���� ���� Ư���� �˾� ����.


# 1317��: ������� ���� �м�
#ggplot2 ��Ű���� ���̾Ƹ�� ������ ��

# ������� ���� �м�(Ȯ���� �м�)
install.packages('ggplot2')
library(ggplot2)
data(diamonds)
nrow(diamonds)

t <- sample(1:nrow(diamonds), 1000)

test <- diamonds[t,]
dim(test)
test

head(test)

mydia = test[c('price', 'carat', 'depth', 'table')]
head(mydia)

result <- hclust(dist(mydia), method='ave')
result

windows()
plot(result, hang=-1)


result2 = kmeans(mydia, 3) # 3���� �������� �����Ѵ�.
result2 
# K-means clustering with 3 clusters of sizes 626, 270, 104
# 
# Cluster means:
#       price     carat    depth    table
# 1  1463.597 0.4913259 61.71565 57.38147
# 2  5845.300 1.1061481 61.62630 57.81926
# 3 13369.192 1.7645192 61.72885 58.25288
# 
# Within cluster sum of squares by cluster:
# [1] 488867034 732046443 721513561
#  (between_SS / total_SS =  87.9 %)
# 
# Available components:
# 
# [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
# [6] "betweenss"    "size"         "iter"         "ifault"      

names(result2) 
# [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
# [6] "betweenss"    "size"         "iter"         "ifault"      

result2$cluster # �� ���̽��� ���� �Ҽ� ������(1,2,3)

# �������� �����Ϳ� ������ �߰�
mydia$cluster = result2$cluster
head(mydia) 
# # A tibble: 6 x 5
#   price carat depth table cluster
#   <int> <dbl> <dbl> <dbl>   <int>
# 1 16960  1.73  62.9    58       3
# 2  5948  1.03  61.2    54       2
# 3   874  0.37  62.7    58       1
# 4  4529  0.96  61.7    55       2
# 5  2609  0.74  60.7    55       1
# 6  1409  0.71  65.5    61       1
windows()
# ���� ���� ����� ���� 
plot(mydia[,-5])
savePlot('���� ���� ����� ����.png', type='png')

# ��� ��� ����
cor(mydia[,-5], method="pearson")
#             price       carat       depth      table
# price  1.00000000  0.93138671 -0.04611429  0.1412640
# carat  0.93138671  1.00000000 -0.01162773  0.1933217
# depth -0.04611429 -0.01162773  1.00000000 -0.3506259
# table  0.14126397  0.19332175 -0.35062590  1.0000000

# ���� ����� ����, price(�ܰ�) ������ ���� ū ������ ��ġ�� ������ carat(���� : ĳ��)�̴�.
# depth�� ���� ������ ��ģ��.

