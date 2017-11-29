# 1529번. 교차분석01

# 2개 이상의 변수들간의 관련성을 알아보기 위하여 교차 분할표를 만들어본다.
setwd('C:/work')
mydata = read.csv('cleanDescriptive.csv', header=TRUE)

x = mydata$level2
y = mydata$pass2

x 
y

result = data.frame(Level=x, Pass=y)
dim(result)

table(result)

#1150 카이제곱
# 1번 엑셀 파일에 대한 카이 제곱 검증 테스트
# 데이터 파일 로딩
chisq01 <- read.csv("카이제곱검정01.csv", header=T)

str(chisq01)
# 'data.frame':   40 obs. of  2 variables:
#  $ gender: Factor w/ 2 levels "남자","여자": 1 1 1 1 1 1 1 1 1 1 ...
#  $ brand : Factor w/ 3 levels "A","B","C": 1 1 1 1 1 1 2 2 2 2 ...

table(chisq01$gender)
# 남자 여자 
#   20   20 

table(chisq01$brand)
#  A  B  C 
# 12 13 15 
# chisq01 데이터를 이용하여 gender를 행으로 brand를 열로 데이터를 카운팅
mytable <- xtabs( ~ gender + brand, data=chisq01)

mytable 
#       brand
# gender A B C
#   남자 6 6 8
#   여자 6 7 7

# 분할표가 만들어 졌으면, chisq.test()를 이용하여 카이 제곱 검정을 수행할 수 있다.
chisq.test( xtabs( ~ gender + brand, data=chisq01) )
#         Pearson's Chi-squared test
# 
# data:  xtabs(~gender + brand, data = chisq01)
# X-squared = 0.14359, df = 2, p-value = 0.9307
# 다음 문장은 동일한 결과를 출력해 준다.
chisq.test( mytable )
#         Pearson's Chi-squared test
# 
# data:  mytable
# X-squared = 0.14359, df = 2, p-value = 0.9307


# p-value(0.9307) > 0.05 이므로 귀무 가설을 채택한다.
# 그러므로, 성별에 따라서 선호하는 커피의 차이가 없다.


# 2번 엑셀 파일에 대한 카이 제곱 검증 테스트
# 데이터 파일 로딩
chisq02 <- read.csv("카이제곱검정02.csv", header=T)

str(chisq02)
# 'data.frame':   40 obs. of  2 variables:
#  $ gender: Factor w/ 2 levels "남자","여자": 1 1 1 1 1 1 1 1 1 1 ...
#  $ brand : Factor w/ 3 levels "A","B","C": 1 1 1 1 1 1 2 2 2 2 ...

table(chisq02$gender)
# 남자 여자 
#   20   20 

table(chisq02$brand)
#   A  B  C 
#  22  8 10 
# chisq02 데이터를 이용하여 gender를 행으로 brand를 열로 데이터를 카운팅
mytable <- xtabs( ~ gender + brand, data=chisq02)

mytable 
#       brand
# gender A B C
#   남자 6 6 8
#   여자 16 2 2

# 분할표가 만들어 졌으면, chisq.test()를 이용하여 카이 제곱 검정을 수행할 수 있다.
chisq.test( xtabs( ~ gender + brand, data=chisq02) )
#         Pearson's Chi-squared test
# 
# data:  xtabs(~gender + brand, data = chisq02)
# X-squared = 10.145, df = 2, p-value = 0.006265

# Warning message:
#   In chisq.test(xtabs(~gender + brand, data = chisq02)) :
#   카이제곱 approximation은 정확하지 않을수도 있습니다

# 다음 문장은 동일한 결과를 출력해 준다.
chisq.test( mytable )
#         Pearson's Chi-squared test
# 
# data:  mytable
# X-squared = 10.145, df = 2, p-value = 0.006265
# 
# Warning message:
#   In chisq.test(mytable) :
#   카이제곱 approximation은 정확하지 않을수도 있습니다.


# 1249번 

data <- c(4, 6, 17, 16, 8, 9)
chisq.test(data)
fisher.test(data)

# 1527번 : 귀무가설 : 스포츠음료에 대한 선호도에 차이가 없다.
data <- textConnection(
  "스포츠음료종류 관측도수
  1 41
  2 30
  3 51
  4 71
  5 61"
)

data <- read.table(data, header = T)
data

chisq.test(data$관측도수)

# Chi-squared test for given
# probabilities
# 
# data:  data$관측도수
# X-squared = 20.488, df = 4, p-value =
#   0.0003999

# p-value < 0.05 이므로 귀무 가설 기각.
# 즉, 스포츠 음료에 대한 선호도에 차이 있음.

#### 1144 번: 독립성 검정(쿠폰 유형별 사용분야)
mycoupon <- read.csv('mycoupon.csv', header=T)

mycoupon$coupon2 <- factor(mycoupon$coupon, levels = c(1,2), labels=c('할인_쿠폰', '적립_쿠폰'))

mycoupon$category2 <- factor(mycoupon$category, levels = c(1,2,3,4), labels=c('food', 'beauty', 'travel', 'park'))


twogrouptable <- table(mycoupon$coupon2, mycoupon$category2)

twogrouptable

pccgroup <- prop.table(twogrouptable, 1)

roundccgroup <- round(pccgroup, 2)

addmargins(roundccgroup)

chisq.test(twogrouptable)

# 귀무가설을 채택한다.
# 결론 : 쿠폰의 종류와 사용 분야는 서로 독립적이다.


# 1528 번:이원 카이 제곱 검정(동질성검정)

# 귀무가설 : 교육 방법에 따른 만족도에 차이가 없다.
data <- read.csv('homogenity.csv', header=TRUE)

head(data)

data <- subset(data, !is.na(survey), c(method, survey))

head(data)

data$method2[data$method == 1] <- '방법1'
data$method2[data$method == 2] <- '방법2'
data$method2[data$method == 3] <- '방법3'

data$survey2[data$survey == 1] <- '매우 만족'
data$survey2[data$survey == 2] <- '만족'
data$survey2[data$survey == 3] <- '보통'
data$survey2[data$survey == 4] <- '불만족'
data$survey2[data$survey == 5] <- '매우 불만족'

head(data)

# 12장 연습 문제

# 01. 교육수준과 흡연율 간의 관련성을 분석하기 위한 연구가설을 수립하고, 각 단계별로 가설을 검증하시오.[독립성 검정]

# 단계 1: 파일 가져오기
smoke <- read.csv('smoke2.csv', header = T)
smoke
head(smoke)

# 단계 2: 코딩 변경
# education 컬럼(독립변수) : 1:대졸, 2:고졸, 3:중졸 
# smoke 컬럼(종속변수) : 1:과다흡연, 2:보통흡연, 3: 비흡연


smoke$education2[smoke$education == 1] <- '대졸'
smoke$education2[smoke$education == 2] <- '고졸'
smoke$education2[smoke$education == 3] <- '중졸'

smoke$smoke2[smoke$smoke == 1] <- '과다흡연'
smoke$smoke2[smoke$smoke == 2] <- '보통흡연'
smoke$smoke2[smoke$smoke == 3 ] <- '비흡연'

smoke$smoke2 = ifelse(!is.na(smoke$smoke), smoke$smoke2, '비흡연')
smoke$smoke = ifelse(!is.na(smoke$smoke), smoke$smoke, 3)
smoke

# 단계 3: 교차분할표 작성
table(smoke$education, smoke$smoke)
# 단계 4: 독립성 검정
chisq.test(smoke$education2, smoke$smoke2)
# 단계 5: 검정결과 해


#### 추가 문제  ##########

# 문제 1.다음 물음에 답하세요.
# 
# 소괄호 안의 내용은 리코딩된 컬럼의 이름이다.
# 1) 엑셀 파일 somefile.csv를 변수 somefile에 읽어 들이시오.
somefile <- read.csv("somefile.csv", header = TRUE)
somefile

# 2) 세대별(age2)로 브랜드(brand2) 선호도에 차이가 있는 지 분석하시오.
# 수직 막대 그래프를 그리시오.



# 3) 성별(gender2)로 브랜드(brand2) 선호도에 차이가 있는 지 분석하시오.
# 수평 막대 그래프를 그리시오.
# 
# 4) 전공별(subject2)로 영화(movie2) 선호도에 차이가 있는 지 분석하시오.
# 수직 누적 막대 그래프를 그리시오.
# 
# 참고
# (1) gender2 : 성별(남자 : 1, 3, 여자 : 2, 4)
# 
# (2) age2 : 나이는 10대 ~ 40대으로 표현한다.
# 단, 결측치 나이는 20으로 치환하도록 한다.
# 
# (3) brand2 : lg(엘지) ss(삼성) hd(현대)
#     1. lg(엘지) 2. ss(삼성) 3. hd(현대)
# 이상치는 1(엘지), 결측치는 3(현대)로 변경하여 처리하시오.
# 
# (4) movie2 : (액션 : 1, 멜로 : 2, 공포 : 3, 환타지 : 4)
# 이상치 및 결측치는 (액션 : 1)으로 변경하여 처리하시오.

