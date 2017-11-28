getwd()

# 1164번
# 기술 통계량 분석
abcCsv <- read.csv('abc.csv', header=T)

# 기본적인 데이터 특성 조회
# 몇행 몇열인가?
dim(abcCsv)
# [1] 6 9

# 열의 갯수
length(abcCsv)
# [1] 9

# price 컬럼의 행 갯수
length(abcCsv$price)
# [1] 6

# 각 셀의 요소들의 값
str(abcCsv)
# 'data.frame':   6 obs. of  9 variables:
#  $ gender  : int  1 1 1 1 2 2
#  $ job     : int  1 2 2 3 3 2
#  $ age     : int  10 20 30 40 50 60
#  $ position: int  2 5 4 4 5 7
#  $ address : Factor w/ 3 levels "busan","daegu",..: 3 1 2 1 3 2
#  $ total   : int  50 80 60 70 50 40
#  $ check   : int  5 NA 5 3 3 3
#  $ price   : int  1200 NA 2500 1200 1400 3000
#  $ survey  : int  3 NA 2 5 6 1
# price 컬럼이 가지고 있는 데이터 보기

str(abcCsv$price)
#  int [1:6] 1200 NA 2500 1200 1400 3000

summary(abcCsv) 
#      gender           job             age          position    address      total           check    
#  Min.   :1.000   Min.   :1.000   Min.   :10.0   Min.   :2.0   busan:2   Min.   :40.00   Min.   :3.0  
#  1st Qu.:1.000   1st Qu.:2.000   1st Qu.:22.5   1st Qu.:4.0   daegu:2   1st Qu.:50.00   1st Qu.:3.0  
#  Median :1.000   Median :2.000   Median :35.0   Median :4.5   seoul:2   Median :55.00   Median :3.0  
#  Mean   :1.333   Mean   :2.167   Mean   :35.0   Mean   :4.5             Mean   :58.33   Mean   :3.8  
#  3rd Qu.:1.750   3rd Qu.:2.750   3rd Qu.:47.5   3rd Qu.:5.0             3rd Qu.:67.50   3rd Qu.:5.0  
#  Max.   :2.000   Max.   :3.000   Max.   :60.0   Max.   :7.0             Max.   :80.00   Max.   :5.0  
#                                                                                         NA's   :1    
#      price          survey   
#  Min.   :1200   Min.   :1.0  
#  1st Qu.:1200   1st Qu.:2.0  
#  Median :1400   Median :3.0  
#  Mean   :1860   Mean   :3.4  
#  3rd Qu.:2500   3rd Qu.:5.0  
#  Max.   :3000   Max.   :6.0  
#  NA's   :1      NA's   :1    
summary(abcCsv$price)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    1200    1200    1400    1860    2500    3000       1 

# 명목 척도 변수의 기술 통계량 조회
length(abcCsv$gender) 
# [1] 6

summary(abcCsv$gender)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.000   1.000   1.000   1.333   1.750   2.000 
abcCsvtable <- table(abcCsv$gender)

abcCsvtable # 1이 4번, 2가 2번 나왔다
# 1 2 
# 4 2 
prop.table(abcCsvtable)
#         1         2 
# 0.6666667 0.3333333 

y <- prop.table(abcCsvtable)
round(y*100, 2) # 100을 곱한 다음, 소수 2째자리 까지 표시
#     1     2 
# 66.67 33.33 

# 서열 척도 변수의 기술 통계량 조회
length(abcCsv$position)
# [1] 6

summary(abcCsv$position)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     2.0     4.0     4.5     4.5     5.0     7.0 

abcCsvPosition <- table(abcCsv$position)
abcCsvPosition #2가 1번, 4가 2번 나온다, ....
# 2 4 5 7 
# 1 2 2 1 

y1 <- prop.table(abcCsvPosition)

round(y1*100, 2) # 100을 곱한 다음, 소수 2째자리 까지 표시
#     2     4     5     7 
# 16.67 33.33 33.33 16.67 


# 소스 코드 2
# 비율 척도 변수의 기술 통계량 구하기
min(abcCsv$total)
# [1] 40

max(abcCsv$total)
# [1] 80

range(abcCsv$total)
# [1] 40 80

sort(abcCsv$total)
# [1] 40 50 50 60 70 80

sort(abcCsv$total, decreasing=T)
# [1] 80 70 60 50 50 40

mean(abcCsv$total)
# [1] 58.33333

# 등간 척도 변수의 기술 통계량 구하기
min(abcCsv$total) # 최소 값
# [1] 40

max(abcCsv$total) # 최대 값
# [1] 80

range(abcCsv$total) # 하한값 ~ 상한값
# [1] 40 80

sort(abcCsv$survey) # 오름차순 정렬
# [1] 1 2 3 5 6

sort(abcCsv$survey, decreasing=T) # 내림차순 정렬
# [1] 6 5 3 2 1

sort(abcCsv$price) # 오름차순 정렬(NA는 포함하지 않는다.)
# [1] 1200 1200 1400 2500 3000

sort(abcCsv$price, na.last=T) # 오름차순 정렬(NA는 마지막에 보여 준다.)
# [1] 1200 1200 1400 2500 3000   NA

min(abcCsv$price) # NA를 포함하여 최소값 구하기
# [1] NA

# NA는 배제하고 최소값 구하기
min(abcCsv$price, na.rm=T)
# [1] 1200

price2 <- abcCsv$price

price2 # 특별한 언급이 없으면 NA는 포함된다.
# [1] 1200   NA 2500 1200 1400 3000

# NA는 빼주세요.
price2 <- na.omit(abcCsv$price)
price2
# [1] 1200 2500 1200 1400 3000

######## 1139번##########################

# 쿠폰 01.기본 기술 통계 분석

# 해당 데이터 파일(csv 파일)을 로딩한다.
mycoupon <- read.csv("mycoupon.csv", header=T)

head(mycoupon) # 데이터 정보를 조회한다.
#   gender age job coupon category   amount
# 1      M  26   1      1        1 5.722788
# 2      F  37   1      1        1 7.306465
# 3      M  31   1      1        1 3.430107
# 4      F  23   1      1        1 6.186975
# 5      M  39   1      1        1       NA
# 6      F  36   1      1        1 5.761108

tail(mycoupon)
#     gender age job coupon category   amount
# 95       F  32   3      2        4 5.507381
# 96       M  42   3      2        4 7.399862
# 97       F  37   3      2        4       NA
# 98       M  35   3      2        4 3.580148
# 99       F  39   3      2        4 6.138364
# 100      M  48   3      2        4 4.014816

View(mycoupon)

# 데이터 파일에 대한 내부 구조 정보를 조회한다.
str(mycoupon)
# 'data.frame':   100 obs. of  6 variables:
#  $ gender  : Factor w/ 2 levels "F","M": 2 1 2 1 2 1 2 1 2 1 ...
#  $ age     : int  26 37 31 23 39 36 45 29 43 39 ...
#  $ job     : int  1 1 1 1 1 1 1 1 1 1 ...
#  $ coupon  : int  1 1 1 1 1 1 1 1 1 1 ...
#  $ category: int  1 1 1 1 1 1 1 1 NA NA ...
#  $ amount  : num  5.72 7.31 3.43 6.19 NA ...

summary(mycoupon)
#  gender      age             job           coupon       category   
#  F:50   Min.   :20.00   Min.   :1.00   Min.   :1.0   Min.   :1.00  
#  M:50   1st Qu.:27.00   1st Qu.:1.00   1st Qu.:1.0   1st Qu.:1.00  
#         Median :35.00   Median :2.00   Median :1.0   Median :2.00  
#         Mean   :34.56   Mean   :1.85   Mean   :1.4   Mean   :2.39  
#         3rd Qu.:42.00   3rd Qu.:3.00   3rd Qu.:2.0   3rd Qu.:3.00  
#         Max.   :49.00   Max.   :3.00   Max.   :2.0   Max.   :4.00  
#                                                      NA's   :18    
#      amount     
#  Min.   :3.030  
#  1st Qu.:4.403  
#  Median :5.493  
#  Mean   :5.484  
#  3rd Qu.:6.715  
#  Max.   :7.856  
#  NA's   :8  

#-------------------------------------
#쿠폰 02.쿠폰 유형 기술 통계 분석

# coupon 컬럼에 들어 있는 유형의 정보를 확인한다.
mycoupon$coupon
#   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#  [33] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2
#  [65] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#  [97] 2 2 2 2

# 유형별 분할표를 만든다.
table(mycoupon$coupon)
#  1  2 
# 60 40 

coupon01 <- table(mycoupon$coupon)

# 분할표에 있는 쿠폰들의 유형에 대한 비율을 구한다.
prop.table( coupon01 )
#   1   2 
# 0.6 0.4 

# 유형의 숫자가 1은 할인 쿠폰으로, 2는 적립 쿠폰이라는 레이블을 새롭게 만든다.
mycoupon$coupon2 <- factor(mycoupon$coupon, levels=c(1, 2), labels=c("할인_쿠폰", "적립_쿠폰"))

coupon02 <- table(mycoupon$coupon2)

# 유형별 레이블 이름에 대한 분할표를 만든다.
coupon02 
# 할인_쿠폰 적립_쿠폰 
#        60        40 

# 분할표에 있는 쿠폰들의 유형에 대한 비율을 구한다.
prop.table( coupon02 )
# 할인_쿠폰 적립_쿠폰 
#       0.6       0.4 
windows()
# 최대 60의 값을 가지므로 y 눈금의 최대 값은 70으로 설정(임의 값으로 설정 가능)했다.
plot(mycoupon$coupon2, xlab="쿠폰 유형", ylab="샘플 갯수", 
     main="쿠폰 유형별 갯수", ylim=c(0, 70))

# 파일 이름 : 쿠폰 유형별 갯수.png


# 3. 의약품등 회수(폐기) 처리 지침
# category 컬럼은 쿠폰의 사용 분야에 대한 정보를 알려 주는 컬럼이다.
# 1 : food, 2 : beauty, 3 : travel, 4 : park라고 가정한다.

# category 컬럼에 들어 있는 유형의 정보를 확인한다.
mycoupon$category
#   [1]  1  1  1  1  1  1  1  1 NA NA NA  2  2  2  2  2  2  2  2  2  2
#  [22]  2 NA NA  3  3  3  3  3  3  3  3  3  3  3 NA NA NA NA NA NA NA
#  [43] NA NA NA NA  4  4  4  4  4  4  4  4  4  4  4  4  4 NA NA  1  1
#  [64]  1  1  1  1  1  1  1  1  1  1  1  1  1  1  2  2  2  2  2  2  2
#  [85]  2  2  2  3  3  3  3  3  3  3  4  4  4  4  4  4

# 유형별 분할표를 만든다.
table(mycoupon$category)
#  1  2  3  4 
# 24 21 18 19 

mycatetable01 <- table(mycoupon$category)

# Sum 컬럼을 확인해 보면 82이다.(결측치가 18개이다.)
addmargins(mycatetable01)
#   1   2   3   4 Sum 
#  24  21  18  19  82 

# 분할표에 있는 유형에 대한 비율을 구한다.
prop.table(mycatetable01)
#         1         2         3         4 
# 0.2926829 0.2560976 0.2195122 0.2317073 

myproptable01 <- prop.table(mycatetable01)

pctproptable01 <- round(myproptable01, 2)

addmargins( pctproptable01 )
#    1    2    3    4  Sum 
# 0.29 0.26 0.22 0.23 1.00 


# 카테고리 별로 레이블을 붙여서 빈도수, 백분율을 구한다.
# 쿠폰 사용 분야 데이터인 category 변수에 레이블 반영
mycoupon$category2 <- factor(mycoupon$category,
                             levels=c(1, 2, 3, 4),
                             labels=c("food", "beauty", "travel", "park"))

# 유형별 분할표를 만든다.
table(mycoupon$category2)
#   food beauty travel   park 
#     24     21     18     19 

mycatetable02 <- table(mycoupon$category2)

addmargins(mycatetable02) 
#   food beauty travel   park    Sum 
#     24     21     18     19     82 

prop.table(mycatetable02) 
#      food    beauty    travel      park 
# 0.2926829 0.2560976 0.2195122 0.2317073 

myproptable02 <- prop.table(mycatetable02)

pctproptable02 <- round(myproptable02, 2)

addmargins( pctproptable02 )
#   food beauty travel   park    Sum 
#   0.29   0.26   0.22   0.23   1.00 

# 1128번
# 기본 기술 통계 분석(광고)
#
mycf <- read.csv('mycf.csv', header=T)
head(mycf)
tail(mycf)
view(mycf)
str(mycf)
summary(mycf)

# 광고 집단 유형에 대한 기술 통계 분석

mycf$group
table(mycf$group)

cf01 <- table(mycf$group)
prop.table(cf01)

mycf$group2 <- factor(mycf$group, levels=c(1,2), labels=c("연예인 CF", "일반인 CF"))

mycf$group2

cf02 <- table(mycf$group2)
cf02

prop.table(cf02)

windows()

plot(mycf$group2, xlab="집단유형", ylab="샘플수", main="집단 유형별 샘플수", ylim=c(0, 1700))

# 광고에 대한 관심 유무에 따른 기술 통계 분석
mycf$interest 

table(mycf$interest)
#    0    1 
#  540 2460 

myint01 <- table(mycf$interest)

prop.table( myint01 )
#    0    1 
# 0.18 0.82 

mycf$interest2 <- factor(mycf$interest, 
                         levels=c(1, 0),
                         labels=c("관심있음", "관심없음"))

mycf$interest2

table(mycf$interest2)
# 관심있음 관심없음 
#     2460      540 

myint02 <- table(mycf$interest2)

prop.table( myint02 )
# 관심있음 관심없음 
#     0.82     0.18 

windows()

plot(mycf$interest2, xlab="광고 관심", 
     main="광고 관심 유무 사용자수", ylim=c(0, 2500)) 


# 기술 통계량 보고서 작성

# 변수 리코딩
# 성별을 한글로 보여주기
mycf$gender2[mycf$gender == 'M'] <- '남자'
mycf$gender2[mycf$gender == 'F'] <- '여자'

mycf$age2[mycf$age <= 40] <- '청년층'
mycf$age2[mycf$age > 40] <- '중년층'

age02 <- table(mycf$age2)
age02

prop.table(age02)


# 1145 번 쿠폰 사용 금액(기술 통계 분석)
#
# 사용 금액 데이터 조회
mycoupon$amount

mean(mycoupon$amount)

# [1] NA

# 결측치를 제외한 평균 구하기(결측치 : 8개)
# 504.507336 / 92
ma = mean( mycoupon$amount, na.rm = T )

ma 
# [1] 5.483775

round(ma, 2)
# [1] 5.48

# 결측치를 가지고 있는 행은 제거하기
data <- na.omit( mycoupon )

head( data ) 
#   gender age job coupon category   amount   coupon2 category2
# 1      M  26   1      1        1 5.722788 할인_쿠폰      food
# 2      F  37   1      1        1 7.306465 할인_쿠폰      food
# 3      M  31   1      1        1 3.430107 할인_쿠폰      food
# 4      F  23   1      1        1 6.186975 할인_쿠폰      food
# 6      F  36   1      1        1 5.761108 할인_쿠폰      food
# 7      M  45   1      1        1 7.422525 할인_쿠폰      food

data$amount
#  [1] 5.722788 7.306465 3.430107 6.186975 5.761108 7.422525 5.371209
#  [8] 3.702164 5.800221 5.727845 4.456667 6.985964 4.698146 5.209010
# [15] 4.240813 5.478430 7.617031 6.293589 5.385648 7.558523 3.366998
# [22] 6.963223 5.892600 6.117369 7.314494 6.185221 3.946833 6.321561
# [29] 6.894233 6.020020 4.706893 4.489292 4.998690 4.707950 3.971833
# [36] 7.641475 3.473521 3.419693 7.279393 6.253286 7.687094 5.095864
# [43] 4.039109 3.769531 6.113341 3.319958 6.705457 4.573698 7.283545
# [50] 3.662493 6.639281 7.855961 4.466964 4.768514 6.744688 7.156283
# [57] 4.715717 3.174431 3.242971 7.633816 3.331369 4.835222 4.158529
# [64] 5.829377 3.120142 6.110949 5.168465 6.840545 4.760922 6.844339
# [71] 5.507381 7.399862 3.580148 6.138364 4.014816


# 결측치 제거 전처리 된 데이터를 별도의 파일로 저장하기
write.csv( data, 'clean.csv' )

# quote= F : 따옴표 붙이지 않기 옵션이다.
write.csv( data, 'clean2.csv', quote= F )
