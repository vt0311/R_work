
library(DBI)
#Sys.setenv(JAVA_HOME='c:/program files/Java/jre1.8.0_144')
library(rJava)
library(RJDBC)


driver <- 'oracle.jdbc.driver.OracleDriver'
driver
jarpath <- 'C:/oraclexe/app/oracle/product/11.2.0/server/jdbc/lib/ojdbc6.jar'


drv <- JDBC(driver, jarpath)
class( drv )
# [1] "JDBCDriver"
# attr(,"package")
# [1] "RJDBC"

url <- 'jdbc:oracle:thin:@//192.168.0.62:1521/xe'
url

id <- 'madang'
password <- 'madang'

conn <- dbConnect( drv, url, id, password)
class( conn ) 

query = 'select * from N06A_DRUG_2015 where atc_cd like \'N06AB%\''
#query2 = 'select PRODUCT_NAME, COUNT(PRODUCT_NAME) from N06A_DRUG_2015 where ATC_CD like \'N06AB%\' group by PRODUCT_NAME'
result1 <- dbGetQuery(conn, query)
class( result1 ) # data.frame
head(result1)

#dim(result1) # [1] 366  15


str(result)
# 'data.frame':	233 obs. of  2 variables:
# $ PRODUCT_NAME       : chr  "럭스푸람정(에스시탈로프람옥살산염)" "에스케이에스시탈로프람정" "시타프렉스정10밀리그램(에스시탈로프람옥살산염)" "산도스파록세틴정(무수염산파록세틴)" ...
# $ COUNT(PRODUCT_NAME): num  11864 35592 47456 9900 5260 ...

# 연관 규칙 생성
library(arules)

btran <- read.transactions("C:/Users/acorn/Desktop/N06AB10_DRUG_2015.txt", format="basket", sep = ",")
btran
# 지지도 : 

# Date, WindGustDir, WindDir, RainToday 컬럼 제거
data_df <- result1[, c(-1, -6, -8, -14)]

# y 변수(RainTomorrow)의 로짓 변환 : 더미 변수 생성
# 1이면 내일 비온다 이다.
data_df$RainTomorrow[weather_df$RainTomorrow == 'Yes'] <- 1
data_df$RainTomorrow[weather_df$RainTomorrow == 'No'] <- 0

# RainTomorrow 컬럼을 숫자 형식으로 변경하여 로지스틱 회귀 분석을 위한 환경을 마련한다.
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)

str(weather_df)
# 'data.frame':   366 obs. of  11 variables:
#  $ MinTemp      : num  8 14 13.7 13.3 7.6 6.2 6.1 8.3 8.8 8.4 ...
#  $ MaxTemp      : num  24.3 26.9 23.4 15.5 16.1 16.9 18.2 17 19.5 22.8 ...
#  $ Rainfall     : num  0 3.6 3.6 39.8 2.8 0 0.2 0 0 16.2 ...
#  $ Sunshine     : num  6.3 9.7 3.3 9.1 10.6 8.2 8.4 4.6 4.1 7.7 ...
#  $ WindGustSpeed: int  30 39 85 54 50 44 43 41 48 31 ...
#  $ WindSpeed    : int  20 17 6 24 28 24 26 24 17 6 ...
#  $ Humidity     : int  29 36 69 56 49 57 47 57 48 32 ...
#  $ Pressure     : num  1015 1008 1007 1007 1018 ...
#  $ Cloud        : int  7 3 7 7 7 5 6 7 7 1 ...
#  $ Temp         : num  23.6 25.7 20.2 14.1 15.4 14.8 17.3 15.5 18.9 21.7 ...
#  $ RainTomorrow : num  1 1 1 1 0 0 0 0 1 0 ...

# 학습용 데이터(train) : 훈련을 시키기 위한 데이터 → 모델 생성됨(weather_model)
# 검정용 데이터(test) : 모델을 검증하기 위한 데이터
idx <- sample(1:nrow(weather_df), 0.7 * nrow(weather_df))

train <- weather_df[idx, ] # 366 * 0.7 = 256.2 → 256개 
test <- weather_df[-idx, ] # 366 * 0.3 = 109.8 → 110개

# 로지스틱 회귀 모델 생성
weather_model <- glm(RainTomorrow ~ ., data=train, family = 'binomial' )
weather_model

summary(weather_model)

# 로지 스틱 회귀 모델 예측치 생성
# weather_model 모델을 이용하여 test 데이터를 검증해 주세요.
pred <- predict(weather_model, newdata=test, type='response')
pred # 1에 가까울수록 비 올 확률이 높다.

# 예측치를 이항형으로 변환
result_pred <- ifelse(pred >= 0.5, 1, 0)
result_pred

table(result_pred)
# result_pred
#  0  1 
# 97 13 

table(test$RainTomorrow)
#  0  1 
# 95 15 
# 모델 평가 : 분류 정확도 계산

table(result_pred, test$RainTomorrow)
# result_pred  0  1
#           0 90  7
#           1  5  8

(90 + 8) / nrow(test)
# [1] 0.8909091