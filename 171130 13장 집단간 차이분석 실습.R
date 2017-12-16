####### p.2 <신차 색상 고객 선호도 분석> ########
# 설명: A자동차 메이커에서 신차를 출시하기 전에 우수 고객 초청 행사에서 색상에 대한 선호도를 조사하였다.
# 예제데이터 : mycar.csv
# 가설: 귀무가설 : 신차의 색상에 대한 고객의 선호도는 차이가 없다.
# 변수명 : id : 아이디 번호, color : 선호하는 색상(1:검정색, 2:흰색, 3: 쥐색, 4:연청색, 5:연녹색)

# 1.색상의 유형별로 빈도수와 비율에 대한 기술 통계량을 구하시오.
  setwd('C:/R_work')
  mycar <- read.csv('mycar.csv', header=TRUE)
  head(mycar)
  
  
  table(mycar$color)
  # 1  2  3  4  5 
  # 88 65 52 40 55
  
  table(mycar[2])
  # 1  2  3  4  5 
  # 88 65 52 40 55
  
  # 빈도수 출력하기
  prop.table(table(mycar$color))
  #         1         2         3         4         5 
  # 0.2933333 0.2166667 0.1733333 0.1333333 0.1833333 
  
  prop.table(table(mycar[2]))
  #         1         2         3         4         5 
  # 0.2933333 0.2166667 0.1733333 0.1333333 0.1833333 
  
  # 백분율을 나타내기 위해 100을 곱함.
  prop.table(table(mycar[2]))*100
  #        1        2        3        4        5 
  # 29.33333 21.66667 17.33333 13.33333 18.33333 
  
  # 소수점 첫째자리까지 반올림
  round(prop.table(table(mycar[2]))*100, 1)
  #    1    2    3    4    5 
  # 29.3 21.7 17.3 13.3 18.3 
  
  
  # 2.색상 유형별 빈도 수와 비율 값을 테이블 형태로 표현하시오.
  surveyFreq <- c(table(mycar$color))  
  surveyProp <- c(round(prop.table(table(mycar$color))*100, 1))
  surveytable <- data.frame(Freq=surveyFreq, Prop=surveyProp)
  surveytable
  #   Freq Prop
  # 1   88 29.3
  # 2   65 21.7
  # 3   52 17.3
  # 4   40 13.3
  # 5   55 18.3
  
  
  # 3.패키지를 이용하여 색상 유형에 따른 빈도와 비율 값을 구해보시오.
  
  # 빈도와 비율의 기술통계량 분석을 한번에 해주는 기능을 가진 패키지
  install.packages('Hmisc')
  library(Hmisc)
  
  # 데이터 테이블의 빈도분석 중심의 기술통계량 분석을 한번에 해주는 기능함수를 가진 패키지
  install.packages('prettyR')
  library(prettyR)
  
  describe(mycar)
  # Description of mycar 
  # 
  # Numeric
  #               mean    median       var        sd   valid.n
  # id           150.5     150.5      7525     86.75       300
  # color        2.697         2     2.172     1.474       300
  
  describe(mycar$color)
  # Numeric
  #               mean    median       var        sd   valid.n
  # x            2.697         2     2.172     1.474       300
  
  #freq(mycar)
  freq(mycar$color)
  # Frequencies for mycar$color 
  #         1    2    5    3    4   NA
  #        88   65   55   52   40    0
  # %    29.3 21.7 18.3 17.3 13.3    0 
  # %!NA 29.3 21.7 18.3 17.3 13.3 
  
  
  # 4.색상 유형에 따른 선호도의 차이가 있는지를 검증하시오.
  chisq.test(surveyFreq)
  # Chi-squared test for given probabilities
  # 
  # data:  surveyFreq 
  # X-squared = 21.6333, df = 4, p-value = 0.0002371
  

######## P.3. <홍보 이벤트 효과 분석> #########
# 설명: A 화학의 치약 제품 B에 대하여 표본 고객들의 구매 여부에 대한 조사 데이터
#  매장에서의 고객 구매율 : 10%
#  고객들에게 홍보 이벤트를 실시하여 구매 비율을 높이려는 프로모션을 진행함.
#  mytooth.csv

# 가설 : 귀무 가설 : 홍보 이벤트 전/후의 구매 비율은 차이가 없다.
  
# 변수명 -  id : 아이디 번호,  buy : 구매 여분( 0 : 구매 하지 않음, 1 : 구매함 )
  
#  
  
  
  
  
######## p.4.책상 납품을 위한 학생 신장 분석 ########

# 한 집단의 평균이 어떤 특정한 값과 같은 지를 검증하는 것을 단일 집단 평균 분석이라고 한다.
# 한 집단의 특정 변수가 수치 데이터로 이루어진 경우 평균 값을 분석할 수 있으며, 
# 이 평균이 사전에 조사된 특정 평균과 동일한지를 비교하는 분석이라고 볼 수 있다.
# 전국 학교에 책상 납품을 위한 조달 경쟁에 이기는 방법은?

# -예제 데이터-
# 책상 제조 회사에서 중학생 표본을 대상으로 조사한 신장 데이터
# 일반적으로 중학생들의 평균 신장은 145cm 정도로 알려져 있음
# 교육부에서 발주한 전국 중학교에서 사용할 책상 물품 조달 입찰 시 사용할 근거 데이터 확보를 위하여 이 분석을 수행함

# 
# 1.표본 중학생들의 평균 신장 수치를 구하시오.
data <- read.csv('myheight.csv', header = TRUE)
data
head(data)

x <- data$id
x
y <- data$height
y

y1 <- mean(y)
y1

#summary(y)
#install.packages('Hmisc')
#library('Hmisc')
#describe(y)


#table(x)
#table(y)

#교차 분석
#table(x, y, useNA = 'ifany')

# 2.데이터 분포가 정규 분포를 이루고 있는 지를 검정하시오.

# 귀무가설 : 학생들의 평균키는 145.0 이다.(y의 데이터는 정규분포이다.)
library(stats)
shapiro.test(y)


# Shapiro-Wilk normality test
# 
# data:  y
# W = 0.90777, p-value = 0.01308

# p-value < 0.05 이므로 귀무가설을 기각한다. 
# 이 데이터는 정규분포를 따르지 않는다.
# 따라서 모수 검정인 T-검정으로 평균 차이 검정을 수행해야 한다.
windows()
hist(y)

#t.test(y, mu=145.0)

t.test(y, mu=145.0, alter='two.side', conf.level=0.95)

# One Sample t-test
# 
# data:  y
# t = 5.3862, df = 29, p-value = 8.674e-06
# alternative hypothesis: true mean is not equal to 145
# 95 percent confidence interval:
#   147.9567 151.5766
# sample estimates:
#   mean of x 
# 149.7667 


# 3.식생활 습관 및 체질 개선에 따라 신장이 기존에 알려진 수치보다 커졌는지를 검정하시오.

# 검정결과 : p-value 값은 8.674e-06으로써 평균값 145.0 과 차이가 있다고 볼 수 있다.
# 검정통계량 : t = 5.3862, df = 29, p-value = 8.674e-06 이며, 95% 신뢰수준에서 
# 신뢰구간은 147.9567 ~ 151.5766(구간추정)이고, x의 평균은 149.7667 으로 나타났다.


# 단일 집단 t-검정 결과를 작성하시오.(교재 367쪽 참고 요망) 
#-----------------------------------
#가설설정| 연구가설(H1) : 학생들의 평균키는 145.0이 아니다.
#         ------------------------------
#        | 귀무가설(H0) : 학생들의 평균키는 145.0 이다.
#-----------------------------------
#연구환경| 교육부에서 발주한 전국 중학교에서 사용할 책상 물품 조달 입찰 시 
#          사용할 근거 데이터 확보를 위하여 이 분석을 수행함
#--------------------------------------
#유의수준| 0.05
#--------------------------------
#분석방법| 단일표본 T검정
#---------------------------------
#검정통계량| t = 5.3862, df = 29
#--------------------------------
#유의확률| p-value = 8.674e-06
#------------------------------
#결과해석| 유의 수준 에서 귀무가설이 기각되었다. 학생들의 평균키는 145.0 이라고 말할 수 없다.
#        | 따라서 식생활 습관 및 체질 개선에 따라 신장이 기존에 알려진 수치보다 커졌다고 볼 수 있다.
#---------------------------

mymethod <- read.csv('mymethod.csv')
mymethod

mymethod2 <- subset(mymethod, performance < 99)
mymethod2
