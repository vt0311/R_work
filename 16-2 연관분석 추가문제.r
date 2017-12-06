################# 1620 번: 패키지 추천 상품의 연관성 분석 ###################################

## 1. 전체 트랜잭션 개수와 상품 아이템 유형은 몇개인가?

# 단계1 : 연관분석을 위한 패키지(arules) 로딩
library(arules)

# 단계2 : 트랜잭션 객체 생성
tran <- read.transactions('mybasket.csv' , format = 'basket', sep = ",")
tran
# 786 transactions (rows) and
# 10 items (columns)

# 단계3 : 트랜잭션 데이터 보기
# arules 패키지에서 제공하는 inspect()함수를 이용하여 트랜잭션 객체를 확인할 수 있다.
inspect(tran)

# -> 전체 트랜잭션 갯수는 786개 이고, 상품 아이템 유형은 10개 이다.

## 2. 가장 발생 빈도가 높은 아이템은 무엇인가?
#rule <- apriori(tran, parameter = list(supp=0.1, conf=0.1))
rule <- apriori(tran, parameter = list())

#inspect(rule)


stran2 <- read.transactions("mybasket.csv", format = "single", sep=",", cols = c(1,2))
stran2
summary(stran2)

Items.df <- as(tran, "data.frame")
head(Items.df)

rules2 <- apriori(tran, parameter = list(supp=0.001, conf=0.8))
inspect(rules2)

windows()
plot(rules2, method="grouped")

library(arulesViz)
windows() 
plot(rules2, method="graph", control=list(type="items"))

# 3.  지지도를 10%로 설정했을 때의 생성되는 규칙의 가짓 수는?


# 4.  상품 아이템 중에서 가장 발생 확률이 높은 아이템과 낮은 아이템은 무엇인가?


# 5.  가장 발생 가능성이 높은 <2개 상품간>의 연관 규칙은 무엇인가?


# 6.  가장 발생 가능성이 높은 <2개 상품 이상에서><제 3의 상품으로>의 연관 규칙은 ?



########## 1623번. 자동차의 제동거리 ########################

# 1번.
# R에서 제공해주는 데이터 셋 중에 cars 데이터 셋이 있다.
# 자동차 데이터의 주행 속도(speed)와 브레이크를 밟았을 때의 제동 거리(dist)에 대한 정보를 담고 있다.

# 절편과 기울기를 구해보세요.
# dist = -17.579 + 3.932 * speed

cars

# 앞 7개의 항목에 대하여 
# 적합된 값 fitted() : 각 speed 값에 대한 모델의 예측된 dist 값을 구하기
#         1         2         3         4         5         6         7 
# -1.849460 -1.849460  9.947766  9.947766 13.880175 17.812584 21.744993 

# 앞 7개의 항목에 대하여 
# 잔차(residuals)를 구해 보세요
residuals(mydata)[1:7]
#         1         2         3         4         5         6         7 
#  3.849460 11.849460 -5.947766 12.052234  2.119825 -7.812584 -3.744993 

# summary()를 사용해 선형 회귀의 결과를 손쉽게 평가할 수 있다.
# 자동차 주행 속도(speed)와 제동 거리(dist)에 대한 분석표를 만들어 보세요.
