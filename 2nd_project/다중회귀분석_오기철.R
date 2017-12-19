getwd()
setwd('C:\\work')
#파일 불러오기
#drug <- read.csv('2012_2015.csv')
drug <- read.csv('C:/work/2nd_project/weather_drug_2012_2015_N06A.csv')

#구조 확인
str(drug)

#변수 모델링
y <- drug$PRICE
x1 <- drug$temp_diff
x2 <- drug$ilsahap_mj_m2
x3 <- drug$ilsohap_hr

#dataframe생성
df <- data.frame(x1, x2, x3, y)
#df <- data.frame(x1, y)

#다중회귀분석
#result.lm <- lm(formula = y ~ x3 , data=df)
result.lm <- lm(formula = y ~ x1 + x2 + x3, data=df)

#절편과 기울기 확인
result.lm

#다중공선성 문제 확인 : 분산팽창요인 값 확인
install.packages('car')
library(car)
vif(result.lm)

#다중회귀분석 결과확인
summary(result.lm)
