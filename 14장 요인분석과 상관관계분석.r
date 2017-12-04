# 1121번. 
getwd()

testdata <- read.csv('시험 점수와 공부 시간1.csv')
testdata

# cor() : 상관 계수를 구해준다.
result <- cor(testdata)
result

# 소수점 2째 자리까지 반올림
round(result, digits=2)

install.packages('corrplot')
library(corrplot)
windows()
corrplot(result, addCoef.col='black')

testdata

help(subset)
