# 1268 번. 의사결정트리
# R에서 제공해주는 airquality

# party 패키지를 적용한 분류 분석
install.packages('party')
library(party)

library(datasets)
str(airquality)

formula = Temp ~ Solar.R + Wind + Ozone

air_ctree = ctree(formula, data=airquality)
air_ctree

windows()
plot(air_ctree)

# 1269 번 : 의사결정 트리 02

# 학습용 데이터와 검증용 데이터를 7:3
# 의사 결정 트리를 이용하여 어떤 변수가 

set.seed(1234)
# 매번 실행시마다 동일한 결과의 값을 얻으려면 set.seed() 함수를 사용하면 된다.

result = sample(1:nrow(iris), nrow(iris)*0.7)
result 
table(result)

train = iris[result,]
test = iris[-result,]

dim(train)

dim(test)

formula = Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

install.packages('party')
library(party)
iris_ctree = ctree(formula, data=train)

iris_ctree


## 1270

# 파일 읽기
weather = read.csv("weather.csv", header=TRUE) 

# 데이터 특성 보기
str(weather)
# 'data.frame':   366 obs. of  15 variables:
#  $ Date         : Factor w/ 366 levels "2014-11-01","2014-11-02",..: 1 2 3 4 5 6 7 8 9 10 ...
#  $ MinTemp      : num  8 14 13.7 13.3 7.6 6.2 6.1 8.3 8.8 8.4 ...
#  $ MaxTemp      : num  24.3 26.9 23.4 15.5 16.1 16.9 18.2 17 19.5 22.8 ...
#  $ Rainfall     : num  0 3.6 3.6 39.8 2.8 0 0.2 0 0 16.2 ...
#  $ Sunshine     : num  6.3 9.7 3.3 9.1 10.6 8.2 8.4 4.6 4.1 7.7 ...
#  $ WindGustDir  : Factor w/ 16 levels "E","ENE","ESE",..: 8 2 8 8 11 10 10 1 9 1 ...
#  $ WindGustSpeed: int  30 39 85 54 50 44 43 41 48 31 ...
#  $ WindDir      : Factor w/ 16 levels "E","ENE","ESE",..: 8 14 6 14 3 1 3 1 2 3 ...
#  $ WindSpeed    : int  20 17 6 24 28 24 26 24 17 6 ...
#  $ Humidity     : int  29 36 69 56 49 57 47 57 48 32 ...
#  $ Pressure     : num  1015 1008 1007 1007 1018 ...
#  $ Cloud        : int  7 3 7 7 7 5 6 7 7 1 ...
#  $ Temp         : num  23.6 25.7 20.2 14.1 15.4 14.8 17.3 15.5 18.9 21.7 ...
#  $ RainToday    : Factor w/ 2 levels "No","Yes": 1 2 2 2 2 1 1 1 1 2 ...
#  $ RainTomorrow : Factor w/ 2 levels "No","Yes": 2 2 2 2 1 1 1 1 2 1 ...

names(weather) # 15개 변수명
#  [1] "Date"          "MinTemp"       "MaxTemp"       "Rainfall"     
#  [5] "Sunshine"      "WindGustDir"   "WindGustSpeed" "WindDir"      
#  [9] "WindSpeed"     "Humidity"      "Pressure"      "Cloud"        
# [13] "Temp"          "RainToday"     "RainTomorrow" 

head(weather)

# install.packages('rpart')
library(rpart)

# cp 속성 값을 높이면 가지 수가 적어지고, 낮추면 가지 수가 많아진다.
# cp 기본값은 0.01
weather.df = rpart(RainTomorrow~., data=weather[, c(-1,-14)], cp=0.01)

weather.df
# n= 366 
# 
# node), split, n, loss, yval, (yprob)
#       * denotes terminal node
# 
#  1) root 366 66 No (0.81967213 0.18032787)  
#    2) Humidity< 71.5 339 46 No (0.86430678 0.13569322)  
#      4) WindGustSpeed< 64 321 34 No (0.89408100 0.10591900)  
#        8) Pressure>=1011.75 268 18 No (0.93283582 0.06716418)  
#         16) Sunshine>=5.15 222  7 No (0.96846847 0.03153153) *
#         17) Sunshine< 5.15 46 11 No (0.76086957 0.23913043)  
#           34) WindGustDir=E,ENE,N,NW,SE,SSE,WNW 25  1 No (0.96000000 0.04000000) *
#           35) WindGustDir=ESE,NE,NNE,NNW,S 21 10 No (0.52380952 0.47619048)  
#             70) MaxTemp< 15.8 7  0 No (1.00000000 0.00000000) *
#             71) MaxTemp>=15.8 14  4 Yes (0.28571429 0.71428571) *
#        9) Pressure< 1011.75 53 16 No (0.69811321 0.30188679)  
#         18) WindGustDir=ENE,NE,NNE,NNW,NW,SSE,W,WNW,WSW 44  9 No (0.79545455 0.20454545)  
#           36) Sunshine>=7 37  5 No (0.86486486 0.13513514) *
#           37) Sunshine< 7 7  3 Yes (0.42857143 0.57142857) *
#         19) WindGustDir=E,ESE,N,S,SW 9  2 Yes (0.22222222 0.77777778) *
#      5) WindGustSpeed>=64 18  6 Yes (0.33333333 0.66666667) *
#    3) Humidity>=71.5 27  7 Yes (0.25925926 0.74074074) *

weather_df = weather[-c(1, 14)]

# 데이터 셈플링
index = sample(1:nrow(weather_df), 0.7*nrow(weather_df))
weater_train = weather_df[ index, ]
weater_test = weather_df[ -index, ]

# 분류 모델을 생성한다.
form = RainTomorrow ~ .
model = rpart(formula = form, data = weater_train)

# 검정 데이터로 분류 모델을 예측해본다.
pred = predict(model, weater_test)

head(pred)
#          No        Yes
# 1 0.9545455 0.04545455
# 2 0.9545455 0.04545455
# 4 0.9545455 0.04545455
# 5 0.9545455 0.04545455
# 6 0.9545455 0.04545455
# 7 0.9545455 0.04545455

result = ifelse(pred[,1] >= 0.5, "NO", "YES")

# 비올 확률 빈도수 출력 
table(result)
# result
#  NO YES 
#  97  13 

table(result, weater_test$RainTomorrow)
# result No Yes
#    NO  82  15
#    YES  8   5

(82+5) / nrow(weater_test) 
# [1] 0.7909091

#--------------------- 1634번: 랜덤포레스트 예-----------------------
# install.packages('randomForest')
library(randomForest)
data(iris)

# 랜덤 포레스트 모델 생성
# 참고로 iris는 150개의 행을 가지고 있다.
model <- randomForest(Species ~ ., data = iris)
model
# Call:
#  randomForest(formula = Species ~ ., data = iris) 
#       Type of random forest: classification
#            Number of trees: 500 ← 복원 추출한 트리의 갯수
# No. of variables tried at each split: 2 ← 자식 노드 분류시 2개의 변수가 사용되었다. 
# 
#         OOB estimate of  error rate: 4.67% ← 오차의 비율
# Confusion matrix:
#            setosa versicolor virginica class.error
# setosa         50          0         0        0.00
# versicolor      0         47         3        0.06
# virginica       0          4        46        0.08

# 정분류율이라고 한다.
(50+47+46) / nrow(iris)
# [1] 0.9533333 ← 분류 정확도(약 95.3%) 

# 파라미터 조정 : 트리 갯수 300, 변수 4개 지정
# na.action=na.omit : 결측치가 있으면 제거하시오.
model2 <- randomForest(Species ~ ., data = iris, ntree = 300, mtry = 4, na.action=na.omit)
model2
# 기본 모델(model)의 결과와 차이가 없는 것으로 나타난다.

# 중요 변수 생성으로 랜덤 포레스트 모델 생성
# importance=T 옵션은 중요 변수 정보를 제공받겠다는 의미이다.
model3 <- randomForest(Species ~ ., data = iris, importance=T, na.action=na.omit)

# 중요 변수 보기
importance(model3)
# MeanDecreaseAccuracy : 분류 정확도에 얼마나 많이 기여 했느냐?
# MeanDecreaseGini : 불확실성 개선 기여도
#                 setosa versicolor virginica MeanDecreaseAccuracy MeanDecreaseGini
# Sepal.Length  6.361279   5.563544  8.333241            10.405082         9.362919
# Sepal.Width   3.899490   0.842789  4.211404             4.716578         2.245426
# Petal.Length 23.844959  32.834954 30.978737            35.770136        45.313673
# Petal.Width  20.929712  31.364889 29.797824            31.536985        42.283370

# 꽃의 종류를 분류하는 데 있어서 가장 크게 기여하는 변수는 Petal.Length으로 나타난다.
windows()
varImpPlot( model3)
#savePlot('중요 변수 시각화.png', type='png')

#---------1635번 -----------

# 최적의 파라미터 찾기

library(randomForest)
ntree <- c(400, 500, 600)
mtry <- c(2:4)
param <- data.frame(n=ntree, m=mtry)

param

for(i in param$n) {
  cat('ntree =', i, '\n')
  for(j in param$m){
    cat('mtry=', j, '\n')
    model_iris <- randomForest(Species~., data=iris, ntree=i, mtry=j, na.action=na.omit)
    print(model_iris) # 모델 출력
    cat('err.rate :', model_iris$err.rate[i], '\n\n')
  } # inner for
} # outer for

# ntree = 400 이고, mtry =3 일때 가장 작은 값을 가진다.
