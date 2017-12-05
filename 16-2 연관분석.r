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