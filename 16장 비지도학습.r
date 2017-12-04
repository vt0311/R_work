# 1312 번. 유클라디안 거리
# 거리구하기 실습

# matrix 객체 생성
x <- matrix(1:9, nrow=3, by=T)
x

# 유클라디안 거리 생성
dist <- dist(x, method='euclidean')
dist

# 위의 함수를 사용하지 않고 R을 사용한다면 다음과 같이 코딩하면 된다.
# 1행과 2행의 변량의 유클리드 거리 구하기

result <- sqrt(sum( (x[1,] - x[2,])^2 ))
result

sqrt(sum( (4-1)^2 + (5-2)^2 + (6-3)^2 ))


## 1314번
# 계층적 군집 분석

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


# 1315번. 중학교 1학년 신체검사 결과 군집 분석
setwd('C:\\work')
body = read.csv("bodycheck.csv", header=TRUE)
head(body)

# 학생이 총 15명이다.
# 번호 컬럼을 제외한 나머지 컬럼들에 대한 유클리드 거리를 생성한다.
ucldist = dist(body[, -1])

# -- 클러스터링
hc = hclust(ucldist)
hc

# hang 속성의 값을 -1로 지정하면 덴드로그램에서 음수값을 제외시킬수 있다.
# 유사한 데이터끼리 군집을 생성한다.
windows()
plot(hc, hang=-1)

# 군집 단위로 테두리 만들기
# 3개 영역으로 그룹핑(k=3)한다.
rect.hclust(hc, k=3, border="red")

# 군집별 특성 파악하기
# 군집별 서브셋을 구하여, 요약 통계량을 이용한 군집별 특징들을 살펴보자.

# 각 그룹별 서브셋 만들기
g1= subset(body, 번호==10 | 번호==4 | 번호==8 | 번호==1  | 번호==15)
g2= subset(body, 번호==11 | 번호==3 | 번호==5 | 번호==6  | 번호==14)
g3= subset(body, 번호==2 | 번호==9 | 번호==13 | 번호==7  | 번호==12)


# 군집별 목록 보기
g1[2:5]
#    악력 신장 체중 안경유무
# 1    28  146   34        1
# 4    25  156   38        1
# 8    23  153   40        1
# 10   27  152   39        1
# 15   25  142   32        1

g2[2:5]




# 1592번. 군집수 자르기
head(iris[1:4])

idist <- dist(iris[1:4])

# 계층형 군집 분석(클러스터링)
hc <- hclust(idist)
hc

plot(hc, hang= -1)
ghc <- cutree(hc, k=3)
ghc
# 150개의 관측치를 대상으로 3개의 군집수를 지정하여 군집을 의미하는 숫자(1~3)가 출력이 된다.

iris$ghc <- ghc
table(iris$ghc) # 빈도수

# 요약 통계량 구하기
g1 <- subset(iris, ghc ==1)
summary(g1[1:4])

g2 <- subset(iris, ghc ==2)
summary(g2[1:4])

g3 <- subset(iris, ghc ==3)
summary(g1[1:4])

# iris 데이터셋을 대상으로 계층적 군집 분석으로 군집 수를 파악한 후 원하는 군집수만큼
# 인위적으로 잘라서 군집을 생성하고, 각 군집별로 요약 통계량을 구하여 군집 내의 특성을 알아 보다.


# 1317번: 비계층적 군집 분석
#ggplot2 패키지의 다이아몬드 데이터 셋

# 비계층적 군집 분석(확인적 분석)
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


result2 = kmeans(mydia, 3) # 3개의 군집수를 적용한다.
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

result2$cluster # 각 케이스에 대한 소속 군집수(1,2,3)

# 오리지널 데이터에 군집수 추가
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
# 변수 간의 상관성 보기 
plot(mydia[,-5])
savePlot('변수 간의 상관성 보기.png', type='png')

# 상관 계수 보기
cor(mydia[,-5], method="pearson")
#             price       carat       depth      table
# price  1.00000000  0.93138671 -0.04611429  0.1412640
# carat  0.93138671  1.00000000 -0.01162773  0.1933217
# depth -0.04611429 -0.01162773  1.00000000 -0.3506259
# table  0.14126397  0.19332175 -0.35062590  1.0000000

# 위의 결과를 보면, price(단가) 변수에 가장 큰 영향을 끼치는 변수는 carat(무게 : 캐럿)이다.
# depth는 부의 영향을 미친다.

