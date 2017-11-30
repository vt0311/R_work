# 1251
# 단일 표본의 빈도 수와 기술 통계량
# 실습 데이터 가져 오기
data <- read.csv('one_sample.csv', header=T)

head(data)
#   no gender survey time
# 1  1      2      1  5.1
# 2  2      2      0  5.2
# 3  3      2      1  4.7
# 4  4      2      1  4.8
# 5  5      2      1  5.0
# 6  6      2      1  5.4

survey <- data$survey

# 빈도 수와 비율 계산
summary(survey)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.0000  1.0000  1.0000  0.9067  1.0000  1.0000 

length(survey)
# [1] 150

table( survey )
# 0 : 불만족(14), 1 : 만족(136) 
# survey
#   0   1 
#  14 136 

# 패키지를 이용한 빈도 수와 비율 계산
install.packages('prettyR')
library(prettyR)

freq( survey )
# Frequencies for survey 
#         1    0   NA
#       136   14    0
# %    90.7  9.3    0 
# %!NA 90.7  9.3 

# 이항 분포 비율 검정
# 기술 통계량을 보면 만족(136), 불만족(14)이다.
# 136명의 만족 고객이 전체의 80% 이상의 만족율을 나타내는 지를 위해서 0.8을 지정한다.

# 양측 검정
binom.test(c(136, 14), p=0.8)
#         Exact binomial test
# 
# data:  c(136, 14)
# number of successes = 136, number of trials = 150, p-value = 0.0006735
# alternative hypothesis: true probability of success is not equal to 0.8
# 95 percent confidence interval:
#  0.8483615 0.9480298
# sample estimates:
# probability of success 
# 0.9066667
# 풀이 하기
# 유의 확률 p-value(0.0006735) < 0.05이므로 귀무 가설을 기각한다.
# 즉, '작년과 올해의 불만율에 차이가 있다'라고 말할 수 있다.


# 1123 번

# A 회사의 건전지 수명 시간이 1000이라고 가정하자.
# 무작위로 뽑은 10개의 건전지의 수명이 다음과 같다.
# 이 샘플이 모집단과 다르다고 할 수 있는 가?
# 귀무 가설 : 건전지 수명 시간은 1000시간이다.

somedata = c(980, 1008, 968, 1032, 1012, 996, 1021, 1002, 996, 1017)

# 단일 표본에 대한 검정을 수행하기 전에 데이터의 분포 형태가 
# 정규 분포를 따르는 지 확인하려면 shapiro.test()을 사용하면 된다.
# 검정 결과가 0.05보다 큰 경우이면 정규 분포이다.
shapiro.test( somedata )
#         Shapiro-Wilk normality test
# 
# data:  somedata
# W = 0.97571, p-value = 0.9382

# 해석 p-value > 0.05이다.
# 따라서, 정규 분포를 따르고 이에 따른 t-test를 수행할 수 있다.

t.test( somedata, mu = 1000, alternative = 'two.sided' )
#         One Sample t-test
# 
# data:  somedata
# t = 0.5269, df = 9, p-value = 0.611
# alternative hypothesis: true mean is not equal to 1000
# 95 percent confidence interval:
#   989.4613 1016.9387
# sample estimates:
# mean of x 
# 1003.2

# 해석 p-value > 0.05이다.
# 그러므로, 귀무 가설을 채택한다.
# 즉, 모집단은 평균 수명 시간이 1000시간이 맞다.



# 1531 번
# 가설
# 귀무 가설 : 국내에서 생산된 노트북과 A회사에서 생산된 노트북의 평균 사용 시간에 차이가 없다.
# 
# 연구 환경
# 국내에서 생산된 노트북 평균 사용 시간이 5.2시간이다.
# A회사에서 생산된 노트북 평균 사용 시간과 차이가 있는 지를 검정하기 위해서 
# A회사의 노트북 150대를 랜덤으로 선정하여 검정을 실시한다.

# 단일 표본의 평균 계산하기
data <- read.csv('one_sample.csv', header=T)
str(data)
# 'data.frame':   150 obs. of  4 variables:
#  $ no    : int  1 2 3 4 5 6 7 8 9 10 ...
#  $ gender: int  2 2 2 2 2 2 2 2 2 1 ...
#  $ x: int  1 0 1 1 1 1 1 1 0 1 ...
#  $ time  : num  5.1 5.2 4.7 4.8 5 5.4 NA 5 4.4 4.9 ...

head(data)
#   no gender survey time
# 1  1      2      1  5.1
# 2  2      2      0  5.2
# 3  3      2      1  4.7
# 4  4      2      1  4.8
# 5  5      2      1  5.0
# 6  6      2      1  5.4

x <- data$time
head(x)
# [1] 5.1 5.2 4.7 4.8 5.0 5.4

# 데이터 분포/결측치 제거하기
summary(x)
# 결측치가 41개이다.
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   3.000   5.000   5.500   5.557   6.200   7.900      41 

# NA 때문에 평균을 제대로 구할 수 없다
mean(x)
# NA

# 데이터 정제 후 평균에 대한 통계치를 계산해본다.
mean(x, na.rm=T) # NA 제외 평균(방법1)
# [1] 5.556881

x1 <- na.omit( x ) # NA 제외 평균(방법2)
mean(x1)
# [1] 5.556881

# 단일 표본 평균 차이 검정을 하기 전에 데이터의 분포 형태가 정규 분포 인지를 먼저 검정해야 한다.
# 정규 분포 검정은 기본 패키지에 속하는 stats 패키지의 shapiro.test() 함수를 이용할 수 있다.
library(stats)

# 귀무 가설 : x1의 데이터 분포는 정규 분포이다.
shapiro.test(x1)
#         Shapiro-Wilk normality test
# 
# data:  x1
# W = 0.99137, p-value = 0.7242

# p-value > 0.05이므로 귀무 가설을 채택한다.
# 이 데이터는 정규 분포를 따른다고 볼 수 있다.
windows()
# 정규 분포 시각화
hist(x1)
savePlot('x1 변량의 히스토그램.png', type='png')
windows()
qqnorm(x1)
qqline(x1, lty=1, col='blue')
savePlot('정규 분포 시각화.png', type='png')

# 평균 차이 검정
# t 검정은 모집단의 평균을 검정하는 방법으로 stats 패키지에서 제공하는 t.test() 함수를 사용하면 된다.

# 단일 표본 차이 검정하기
# 양측 검정 : x1 객체와 기존 모집단의 평균 5.2시간 비교하기
t.test(x1, mu=5.2)
#         One Sample t-test
# 
# data:  x1
# t = 3.9461, df = 108, p-value = 0.0001417
# alternative hypothesis: true mean is not equal to 5.2
# 95 percent confidence interval:
#  5.377613 5.736148
# sample estimates:
# mean of x 
# 5.556881

# 결과를 보면 95%의 신뢰 수준에서 신뢰 구간은 5.377613 ~ 5.736148이다.
# 평균은 5.556881이다.
# p-value < 0.05 이므로, 귀무 가설을 기각한다.
# 국내에서 생산된 노트북과 A회사에서 생산된 노트북의 평균 사용 시간에 차이가 있다.

# 방향성을 갖는 연구 가설 검정 
t.test(x1, mu=5.2, alter='greater', conf.level=0.95)
#         One Sample t-test
# 
# data:  x1
# t = 3.9461, df = 108, p-value = 7.083e-05
# alternative hypothesis: true mean is greater than 5.2
# 95 percent confidence interval:
#  5.406833      Inf
# sample estimates:
# mean of x 
# 5.556881

# p-value < 0.05 이므로, 귀무 가설을 기각한다.
# A회사에서 생산된 노트북의 평균 사용 시간이 국내에서 생산된 노트북 사용 시간보다 더 길다고 할 수 있다.

# 귀무 가설에 대한 임계 값
# stats 패키지의 qt() 함수를 사용하면 된다.
# qt(p-value, df)
qt(7.083e-05, 108)
# [1] -3.946073
# t 검정 통계량이 절대값 3.946073 보다 크기 때문에 귀무 가설을 기각할 수 있다.

# T 검정 변수 보기
# result 변수에 저장된 컬럼 이름들을 확인한 뒤 각 컬럼에 저장된 T-검정 관련 정보를 확인할 수 있다.
result <- t.test(x1, mu=5.2, alter='greater', conf.level=0.95)
names(result)

attach( result )
statistic # t  = 3.94606 
parameter # df = 108 
p.value # 7.083346e-05
conf.int
estimate
null.value
alternative
method
data.name
detach(result)

# 1253 

# 귀무 가설 : 두 가지 교육 방법에 따라 교육생의 만족율에 차이가 없다. 

# 연구 환경
# 교육 센터에서는 다음과 같이 2가지 교육 방법을 제시하고 있다. 
# PT 교육 방법, 코딩 교육 방법이다.
# 더 효과적인 교육 방법이 무엇인지를 조사하기 위하여 다음과 같이 설문 조사를 하였다.
#            만족   불만족 참가자
# PT 교육     110   40   150
# 코딩 교육    135   15   150
# 합계        245   55   300

# 실습 데이터 가져 오기
data <- read.csv('two_sample.csv', header=T)
head(data)
#   no gender method survey score
# 1  1      1      1      1   5.1
# 2  2      1      1      0   5.2
# 3  3      1      1      1   4.7
# 4  4      2      1      0   4.8
# 5  5      1      1      1   5.0
# 6  6      1      1      1   5.4

# 두 집단의 subset 작성과 교차 분석 수행하기
method <- data$method # 교육 방법(1, 2) → NA 없음
survey <- data$survey # 만족도(1 : 만족, 0 : 불만족)

# 집단별 빈도 분석
table( method )
# method
#   1   2 
# 150 150 

table( survey )
# survey
#   0   1 
#  55 245 

# 두 변수에 대한 교차 분석
# useNA='ifany'는 결측치까지 출력하는 옵션이다.
table(method, survey, useNA='ifany') 
#       survey
# method   0   1
#      1  40 110
#      2  15 135

# 집단 간의 비율 차이를 검정하기 위하여 교육 방법과 만족도 컬럼을 추출해 봤다.
# 빈도 분석과 교차 분석을 통하여 집단 간의 차이를 검정 통계량으로 알아 보았다.


# 두 집단 비율 차이 검정
# c(110, 135)는 교육 방법에 대한 만족 수이다.
# c(150, 150)는 변량의 길이이다.

prop.test(c(110, 135), c(150, 150))
#         2-sample test for equality of proportions with continuity correction
# 
# data:  c(110, 135) out of c(150, 150)
# X-squared = 12.824, df = 1, p-value = 0.0003422
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#  -0.25884941 -0.07448392
# sample estimates:
#    prop 1    prop 2 
# 0.7333333 0.9000000 

# 풀이 하기
# 95%의 신뢰 수준에서 검정 통계량 p-value(0.0003422) < 0.05이므로 귀무 가설을 기각한다.
# 즉, '두 가지 교육 방법에 따라 교육생의 만족율에 차이가 있다.'라고 말할 수 있다.

# 검정 통계량은 X-squared = 12.824, df = 1, p-value = 0.0003422이다.
# 95% 신뢰 수준에서 신뢰 구간은 -0.25884941 ~ -0.07448392이고, 
# 첫 번째 교육 방법의 비율은 0.7333333이고, 두 번째 교육 방법의 비율은 0.9000000으로 나타났다.

# 1254번
# (1) 독립 표본 평균 계산

# 실습 데이터 가져 오기
data <- read.csv('two_sample.csv', header=T)

head(data)
#   no gender method survey score
# 1  1      1      1      1   5.1
# 2  2      1      1      0   5.2
# 3  3      1      1      1   4.7
# 4  4      2      1      0   4.8
# 5  5      1      1      1   5.0
# 6  6      1      1      1   5.4

summary(data) # score의 NA 갯수 : 73개
#        no             gender         method        survey           score      
#  Min.   :  1.00   Min.   :1.00   Min.   :1.0   Min.   :0.0000   Min.   :3.000  
#  1st Qu.: 75.75   1st Qu.:1.00   1st Qu.:1.0   1st Qu.:1.0000   1st Qu.:5.100  
#  Median :150.50   Median :1.00   Median :1.5   Median :1.0000   Median :5.600  
#  Mean   :150.50   Mean   :1.42   Mean   :1.5   Mean   :0.8167   Mean   :5.685  
#  3rd Qu.:225.25   3rd Qu.:2.00   3rd Qu.:2.0   3rd Qu.:1.0000   3rd Qu.:6.300  
#  Max.   :300.00   Max.   :2.00   Max.   :2.0   Max.   :1.0000   Max.   :8.000  
#                                                                 NA's   :73  


# 두 집단의 subset 작성과 데이터 전처리
# method : 교육 방법, score : 점수
# score가 NA가 아닌 것 중에서 method, score
result <- subset(data, !is.na(score), c(method, score))

# 정제된 데이터를 대상으로 subset 생성
result 

# 데이터 분리
method1 <- subset(result, method == 1 )
method2 <- subset(result, method == 2 )

score1 <- method1$score
score2 <- method2$score

# 기술 통계량
length( score1 )
# [1] 109

length( score2 )
# [1] 118

mean( score1 )
# [1] 5.556881

mean( score2 )
# [1] 5.80339

# (2) 동질성 검정

var.test( score1, score2 )

# 검정 통계량은 
# 귀무 가설을 채택하므로 두 집단간의 분포 형태가 동질하다고 볼 수 있다.


# (3) 두 집단 평균 차이 검정 수행하기
t.test(score1, score2)

# 1532번. 세 집단 비율 검정(교육 만족도)

# 귀무가설 : 세 교육 방법에 따른 집단간 만족도에 차이가 없다.

data = read.csv('three_sample.csv', header = TRUE)
data

method = data$method
survey = data$survey
method
survey

# 교육 방법과 만족도 교차 분할표
table(method, survey, useNA='ifany')

prop.test(c(34, 37, 39), c(50,50, 50))
# p-value = 0.5232



### 1533 번. 분산 분석(교육 방법에 따른 실기 시험 평균)
mydata = read.csv('three_sample.csv', header=T)
head(mydata)

mydata = subset(mydata, !is.na(score), c(method, score))
head(mydata)

windows()
plot(mydata$score)

windows()
barplot(mydata$score)

mean(mydata$score)
mydata2 = subset(mydata, score <= 14)
length(mydata2$score)

x = mydata2$score

windows()
boxplot(x)

windows()
plot(x)
savePlot('분산 분석(산점도_after).png', type='png')


mydata2$method2[mydata2$method==1] = '방법1'
mydata2$method2[mydata2$method==2] = '방법2'
mydata2$method2[mydata2$method==3] = '방법3'

table(mydata2$method2)

x <- table(mydata2$method2)

y <- tapply(mydata2$score, mydata2$method2, mean)


df <- data.frame(교육방법=x, 성적=y)
df


bartlett.test(score ~ method2, data=mydata2)

result = aov(score ~ method2, data= mydata2)
names(result)

summary(result)



#install.packages('Hmsic')
