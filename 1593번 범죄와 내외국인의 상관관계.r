#1593번 범죄와 내외국인의 상관관계 분석

setwd('C:/work')
data <- read.csv('범죄와외국등록일반.csv')
data
head(data)

summary(data)

sd(data$범죄)
sd(data$외국인등록)
sd(data$일반주민)
 
# cor() : 상관 계수를 구해준다.
# 피어슨 상관 계수를 이용하여 변수들 간의 상관 관계를 살펴 보도록 한다.
cor(product$제품치닐)