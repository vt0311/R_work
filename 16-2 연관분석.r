# 1309번: 장바구니 구매 물품의 연관성 분석

# 연관 규칙 생성
install.packages("arules")
library(arules)

setwd('C:/work')
# transaction 객체 생성(파일 이용)
tran = read.transactions('tran.txt', format = "basket", sep=",")
tran


# transaction 데이터 보기
# 트랜잭션 항목을 출력한다.
inspect(tran)
#     items           
# [1] {라면,맥주,우유}
# [2] {고기,라면,우유}
# [3] {고기,과일,라면}
# [4] {고기,맥주,우유}
# [5] {고기,라면,우유}
# [6] {과일,우유}     

# 규칙 발견
# 지지도와 신뢰도가 높을 수록 발견되는 규칙의 수는 적어진다.
rule= apriori(tran, parameter = list(supp=0.3, conf=0.1))
rule= apriori(tran, parameter = list(supp=0.1, conf=0.1))

inspect(rule) # 연관 규칙 결과 확인하기
#      lhs            rhs    support   confidence lift  count
# … 중략
# [12] {맥주}      => {고기} 0.1666667 0.5000000  0.750 1    
# … 중략
# [30] {라면,맥주} => {우유} 0.1666667 1.0000000  1.200 1    
# … 중략

# 부연 설명
# 맥주 → 고기의 조합의 거래는 [12]번에서 확인할 수 있다.
# 지지도는 0.1666667인데, 다른 조합({우유} => {맥주}, {고기} => {라면} )에 비하여 값이 적다.
# 즉, 해당 조합으로 판매되는 경우의 수가 다른 것보다 상대적으로 적다는 의미이다.
# 맥주를 구매하는 사람은 대체적으로 고기를 잘 사지 않는다.

# 라면, 맥주 → 고기의 조합의 거래는 [30]번에서 확인할 수 있다.



#---------------------------------
# 1311번: 식료품 가게(Groceries) 연관 분석

library(arules)
data('Groceries')

str(Groceries)
Groceries

Groceries.df <- as(Groceries, 'data.frame')
str(Groceries.df)

head(Groceries.df)

rules = apriori(Groceries, parameter = list(supp=0.001, conf=0.8))

install.packages('arulesViz')
library(arulesViz)

# 규칙을 구성하는 왼쪽 -> 오른쪽의 item 빈도수 보기
windows()
plot(rules, method='grouped')


rules = apriori(Groceries, parameter = list(supp=0.001, conf=0.8, maxlen=3))
inspect(rules)


# 발견된 규칙 시각화
library(arulesViz)
plot(rules, method='graph', control = list(type='items'))
# 

### 1621번 단순 선형 회귀 분석 수행

product <- read.csv('product.csv', header=T)
head(product)

str(product)

y <- product$제품_만족도
x <- product$제품_적절성
df <- data.frame(x,y)

result.lm <- lm(formula = y ~ x, df)

result.lm

# Call:
#   lm(formula = y ~ x, data = df)
# 
# Coefficients:
#   (Intercept)            x  
# 0.7789       0.7393  

# 모델의 적합값과 잔차 보기
names(result.lm)

# 모델의 적합 값 보기
fitted.values(result.lm)[1:2]

# 관측 값
head(df, 1)

# 잔차 계산
3-3.735963

# 모델의 잔차는 residuals() 함수를 사용하여 구할 수 있다.
residuals(result.lm)[1:2]

-0.735963 + 3.735963

## 선형 회귀 분석 모델 시각화
#단계1. x, y에 대한 산점도 그리기
windows()
plot(formula = y ~ x, data = df)

# 단계2. 선형 회귀 모델 생성
result.lm <- lm(formula = y ~ x, df)

# 단계3. 회귀 선 그리기
abline(result.lm, col='red')

savePlot('선형 회귀 분석 모델 시각화.png', type='png')

# 선형 회귀 분석 결과 보기
summary(result.lm)




### 1622번 다중 회귀 분석
# 다중 회귀 분석은 여러 개의 독립 변수가 하나의 종속 변수에 미치는 영향을 분석할 때 이용하는 분석 방법 이다.

str(product)
# 'data.frame':	264 obs. of  3 variables:
#   $ 제품_친밀도: int  3 3 4 2 2 3 4 2 3 4 ...
# $ 제품_적절성: int  4 3 4 2 2 3 4 2 2 2 ...
# $ 제품_만족도: int  3 2 4 2 2 3 4 2 3 3 ...

head(product)

y = product$제품_만족도  # 종속 변수
x1 = product$제품_친밀도 # 독립 변수1
x2 = product$제품_적절성 # 독립 변수2

df = data.frame(x1, x2, y)

result.lm = lm(formula=y ~ x1 + x2, data=df) 

# 절편과 기울기 확인
result.lm 
# Call:
# lm(formula = y ~ x1 + x2, data = df)
# 
# Coefficients:
# (Intercept)           x1           x2  
#     0.66731      0.09593      0.68522  

# 다중 공선성 문제 확인
install.packages('car')
library(car)

# 분산 팽창 요인의 값이 10 이상인 경우에는 다중 공선성의 문제를 의심해볼 필요가 있다.
vif(result.lm) # 분산 팽창 요인 확인하기
#       x1       x2 
# 1.331929 1.331929 

summary(result.lm)





