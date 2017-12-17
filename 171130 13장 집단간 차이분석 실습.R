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
# 문제- 1.구매 여부의 빈도 수와 비율에 대한 기술 통계량을 구하시오.
  
  mytooth <- read.csv('mytooth.csv', header=TRUE)
  head(mytooth)
  
  table(mytooth$buy)
  #  0  1 
  # 40 10
  
  # 항목별 비율 출력
  prop.table(table(mytooth$buy))
  #   0   1 
  # 0.8 0.2 
  
  prop_tb_mybuy <- prop.table(table(mytooth$buy))
                                        
  # 백분율 표현
  round(prop_tb_mybuy*100, 2)
  #  0  1 
  # 80 20
  
  
  # 2.구매 여부의 빈도 수와 비율을 테이블 형태로 표현하시오.
  tb_mybuy <- table(mytooth$buy)
  buyFreq <- c(tb_mybuy)
  buyProp <- c(round(prop.table(tb_mybuy)*100, 1))
  buytable <- data.frame(Freq = buyFreq, Prop = buyProp ) 
    
  buytable
  #   Freq Prop
  # 0   40   80
  # 1   10   20
  
  
  
  # 3.패키지를 이용하여 구매 여부에 따른 빈도와 비율 값을 구해 보시오.
  library(Hmisc)
  library(prettyR)
  describe(mytooth$buy)
  # Numeric
  #               mean    median       var        sd   valid.n
  # x              0.2         0    0.1633    0.4041        50
  freq(mytooth$buy)
  # Frequencies for mytooth$buy 
  #         0    1   NA
  #        40   10    0
  # %      80   20    0 
  # %!NA   80   20 
  
  
  # 4.실제 구매 비율이 10%보다 향상이 되었는 지를 검증하시오.
  binom.test(c(40,10), p=0.10)
  # Exact binomial test
  # 
  # data:  c(40, 10) 
  # number of successes = 40, number of trials = 50, p-value < 2.2e-16
  # alternative hypothesis: true probability of success is not equal to 0.1 
  # 95 percent confidence interval:
  #   0.6628169 0.8996978 
  # sample estimates:
  #   probability of success 
  #   0.8 
  
  binom.test(c(40,10), p=0.15, alternative = 'greater', conf.level=0.95)
  # Exact binomial test
  # 
  # data:  c(40, 10) 
  # number of successes = 40, number of trials = 50, p-value < 2.2e-16
  # alternative hypothesis: true probability of success is greater than 0.15 
  # 95 percent confidence interval:
  #   0.6844039 1.0000000 
  # sample estimates:
  #   probability of success 
  #   0.8 
  
  # 유의확률(p-value < 2.2e-16)값이 유의 수준(0.05)보다 낮으므로 귀무가설을 기각하고 대립가설을 채택한다.->
  # 이번 이벤트 부스의 홍보 이벤트에 의한 구매비율은 15%보다 크다.
  # 즉, 일반 매장에서의 구매비율인 10%보다 높은 것으로 이번 홍보 이벤트의 반응효과가 더 높다고 할 수 있다.
  
  
  
###################### p.4. <책상 납품을 위한 학생 신장 분석> ######################

# 한 집단의 평균이 어떤 특정한 값과 같은 지를 검증하는 것을 단일 집단 평균 분석이라고 한다.
# 한 집단의 특정 변수가 수치 데이터로 이루어진 경우 평균 값을 분석할 수 있으며, 
# 이 평균이 사전에 조사된 특정 평균과 동일한지를 비교하는 분석이라고 볼 수 있다.
# 전국 학교에 책상 납품을 위한 조달 경쟁에 이기는 방법은?

# 설명 -
# 책상 제조 회사에서 중학생 표본을 대상으로 조사한 신장 데이터
# 일반적으로 중학생들의 평균 신장은 145cm 정도로 알려져 있음
# 교육부에서 발주한 전국 중학교에서 사용할 책상 물품 조달 입찰 시 사용할 근거 데이터 확보를 위하여 이 분석을 수행함
# myheight.csv

# 귀무 가설 : 학생들의 평균 키는 145.0이다. 
  
# 변수명 : id : 아이디 번호, height : 신장 데이터
  
# 1.표본 중학생들의 평균 신장 수치를 구하시오.

getwd()  
myheight <- read.csv('myheight.csv', header = TRUE)

head(myheight)

myheight$height
# [1] 148 150 149 144 152 150 155 147 148 151 150 149 150 144 147 150 153 147 152 150 151 149 149 153 147 152 160 165 140
# [30] 141

mean(myheight$height)
# [1] 149.7667

range(myheight$height)
# [1] 140 165

myheight5 <- subset(myheight, height != 999, c(height))
head(myheight5)
#   height
# 1    148
# 2    150
# 3    149
# 4    144
# 5    152
# 6    150

mean(myheight5$height)
# [1] 149.7667

range(myheight5$height)
# [1] 140 165


#install.packages('Hmisc')
library(Hmisc)
library(prettyR)
describe(myheight5)
# Description of myheight5 
# 
# Numeric
#               mean    median       var        sd   valid.n
# height       149.8       150      23.5     4.847        30

y <- myheight5$height
describe(y)
# Description of structure(list(x = c(148L, 150L, 149L, 144L, 152L, 150L, 155L,  147L, 148L, 151L, 150L, 149L, 150L, 144L, 147L, 150L, 153L, 147L,  152L, 150L, 151L, 149L, 149L, 153L, 147L, 152L, 160L, 165L, 140L,  141L)), .Names = "x", row.names = c(NA, -30L), class = "data.frame") 
# 
# Numeric
#               mean    median       var        sd   valid.n
# x            149.8       150      23.5     4.847        30



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
#windows()
#hist(y)


t.test(y, mu=145.0)

# Shapiro-Wilk normality test
# 
# data:  y 
# W = 0.9078, p-value = 0.01308
# 
# > t.test(y, mu=145.0)
# 
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

#t.test(y, mu=145.0, alter='two.sided', conf.level=0.95)
t.test(y, mu=145.0, alter='greater', conf.level=0.95)
# One Sample t-test
# 
# data:  y 
# t = 5.3862, df = 29, p-value = 4.337e-06
# alternative hypothesis: true mean is greater than 145 
# 95 percent confidence interval:
#   148.263     Inf 
# sample estimates:
#   mean of x 
# 149.7667 

# 유의 확률이(4.337e-06) 값이 유의수준 (0.05)보다 작으므로 귀무가설 기각한다.
# 결국 중학생들의 평균신장은 식생활 습관 및 체질개선에 따라 커졌다고 볼수있다.


# 3.식생활 습관 및 체질 개선에 따라 신장이 기존에 알려진 수치보다 커졌는지를 검정하시오.

# 검정결과 : p-value 값은 8.674e-06으로써 평균값 145.0 과 차이가 있다고 볼 수 있다.
# 검정통계량 : t = 5.3862, df = 29, p-value = 8.674e-06 이며, 95% 신뢰수준에서 
# 신뢰구간은 147.9567 ~ 151.5766(구간추정)이고, x의 평균은 149.7667 으로 나타났다.


# 4. 단일 집단 t-검정 결과를 작성하시오.(교재 367쪽 참고 요망) 
#----------------------------------------------------------
#가설설정| 연구가설(H1) : 학생들의 평균키는 145.0이 아니다.
#         --------------------------------------------------
#        | 귀무가설(H0) : 학생들의 평균키는 145.0 이다.
#----------------------------------------------------------
#연구환경| 교육부에서 발주한 전국 중학교에서 사용할 책상 물품 조달 입찰 시 
#          사용할 근거 데이터 확보를 위하여 이 분석을 수행함
#--------------------------------------------------------
#유의수준| 0.05
#--------------------------------------------------------
#분석방법| 단일표본 T검정
#-------------------------------------------------------
#검정통계량| t = 5.3862, df = 29
#---------------------------------------------------------
#유의확률| p-value = 8.674e-06
#----------------------------------------------------------
#결과해석| 유의 수준 에서 귀무가설이 기각되었다. 학생들의 평균키는 145.0 이라고 말할 수 없다.
#        | 따라서 식생활 습관 및 체질 개선에 따라 신장이 기존에 알려진 수치보다 커졌다고 볼 수 있다.
#----------------------------------------------------------



################### p.5. <광고모델반응분석> ##################
#예제데이터 :2가지 광고 대안별로 독립적인 모니터링 그룹에서 해당 광고에 대한 관심 유무를 조사한 데이터
#             mycf.csv

#귀무가설 : 연예인 CF과 일반인 CF에 대한 고객의 반응율은 차이가 없다.

#문제1. 모니터링 그룹별 해당 광고의 관심 유무에 대한 빈도수와 비율을 구하시오.
mycf <- read.csv('mycf.csv', header = TRUE)
mycf$group
#  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
# [59] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
mycf$interest
#  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
# [59] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
mycf[c('group', 'interest')]

table(mycf$group)
#  1  2 
# 50 50 

table(mycf$interest)
#  0  1 
# 40 60 

table(mycf$group, mycf$interest)
#    0  1
# 1 13 37
# 2 27 23

prop.table(table(mycf$group, mycf$interest))
#      0    1
# 1 0.13 0.37
# 2 0.27 0.23

round(prop.table(table(mycf$group, mycf$interest))*100, 1)
#    0  1
# 1 13 37
# 2 27 23



# 2. 두 집단 간의 관심 유무에 차이가 있는 지를 검증하시오.

 prop.test(c(13, 27), c(50, 50))
 
# 2-sample test for equality of proportions with continuity correction
# 
# data:  c(13, 27) out of c(50, 50) 
# X-squared = 7.0417, df = 1, p-value = 0.007963
# alternative hypothesis: two.sided 
# 95 percent confidence interval:
#   -0.48402799 -0.07597201 
# sample estimates:
#   prop 1 prop 2 
#     0.26   0.54 
 
 prop.test(c(13, 27), c(50, 50), alter='less', conf.level=0.95) 

 # 2-sample test for equality of proportions with continuity correction
 # 
 # data:  c(13, 27) out of c(50, 50) 
 # X-squared = 7.0417, df = 1, p-value = 0.003982
 # alternative hypothesis: less 
 # 95 percent confidence interval:
 #   -1.0000000 -0.1055588 
 # sample estimates:
 #   prop 1 prop 2 
 #     0.26   0.54 



#################### p.6. <영업 사원 교육 효과 분석> ####################### 
# 1.교육 방법별로 교육을 이수한 사람들의 빈도 수와 영업 실적 평균 값을 구하시오.

mymethod <- read.csv('mymethod.csv')
head(mymethod)

mymethod$method
#  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [60] 2 2 2 2
mymethod$performance
#  [1] 27  5 21 99 14 23 20  9 28 15 29 99  9  5 19 13 10 29  5 28  6 20 17  9 19 45 21 37 26 26 24 37 34 44 39 20 99 15 41
# [40] 38 21 26 26 22 99 31 34 27 21 32 35 35 29 43 18 15 99 30 23 32 99 28 29

groupA <- subset(mymethod, method == 1 & performance < 99)
groupB <- subset(mymethod, method == 2 & performance < 99)

groupAcount <- length(groupA$method)
groupAmean <- round(mean(groupA$performance), 2)
groupAcount; groupAmean
# [1] 22
# [1] 16.41

groupBcount <- length(groupB$method)
groupBmean <- round(mean(groupB$performance), 2)
groupBcount; groupBmean
# [1] 35
# [1] 29.23



# 2.실제 영업 실적 비교 대상자만의 데이터를 별도로 추출하시오.

groupcount <- c(groupAcount, groupBcount)
groupmean <- c(groupAmean, groupBmean)
groupcount; groupmean
# [1] 22 35
# [1] 16.41 29.23

grouptable <- data.frame(Freq=groupcount, Mean=groupmean)
grouptable
#   Freq  Mean
# 1   22 16.41
# 2   35 29.23



# 3.두 교육 방법별 영업 실적 데이터 간에 분포 모양이 비슷한지를 검정하시오.

var.test(groupA$performance, groupB$performance)

# F test to compare two variances
# 
# data:  groupA$performance and groupB$performance 
# F = 1.0648, num df = 21, denom df = 34, p-value = 0.8494
# alternative hypothesis: true ratio of variances is not equal to 1 
# 95 percent confidence interval:
#   0.502791 2.427170 
# sample estimates:
#   ratio of variances 
# 1.06479 


# 4.두 집단 간의 영업 실적에 차이가 있는 지 검증하시오.

t.test(groupA$method, groupB$performance, alter="two.sided", conf.int =TRUE, conf.level=0.95)

# Welch Two Sample t-test
# 
# data:  groupA$method and groupB$performance 
# t = -20.26, df = 34, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#   -31.06012 -25.39702 
# sample estimates:
#   mean of x mean of y 
# 1.00000  29.22857 


t.test(groupA$method, groupB$performance, alter="greater", conf.int =TRUE, conf.level=0.95)

# Welch Two Sample t-test
# 
# data:  groupA$method and groupB$performance 
# t = -20.26, df = 34, p-value = 1
# alternative hypothesis: true difference in means is greater than 0 
# 95 percent confidence interval:
#   -30.58456       Inf 
# sample estimates:
#   mean of x mean of y 
# 1.00000  29.22857 
