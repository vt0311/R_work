###############################
#카페 1286번

# 단계. 데이터 셋 보기
dataset = read.csv("dataset.csv", header=TRUE)   # 헤더가 있는 경우

str(dataset) 
# 'data.frame':   300 obs. of  7 variables:
#  $ resident: int  1 2 NA 4 5 3 2 5 NA 2 ...
#  $ gender  : int  1 1 1 2 1 1 2 1 1 1 ...
#  $ job     : int  1 2 2 NA 3 2 1 2 1 2 ...
#  $ age     : int  26 54 41 45 62 57 36 NA 56 37 ...
#  $ position: int  2 5 4 4 5 NA 3 3 5 3 ...
#  $ price   : num  5.1 4.2 4.7 3.5 5 5.4 4.1 675 4.4 4.9 ...
#  $ survey  : int  1 2 4 2 1 2 4 4 3 3 ...

# -- 데이터셋 전체 보기
print( head(dataset) ) # 콘솔 창으로 전체 보기
View(dataset)  # 뷰어창 출력

head(dataset) 
#   resident gender job age position price survey
# 1        1      1   1  26        2   5.1      1
# 2        2      1   2  54        5   4.2      2
# 3       NA      1   2  41        4   4.7      4
# 4        4      2  NA  45        4   3.5      2
# 5        5      1   3  62        5   5.0      1
# 6        3      1   2  57       NA   5.4      2

tail(dataset) 
#     resident gender job age position price survey
# 295        2      1   1  20        1   3.5      5
# 296        1      5   2  26        1   7.1      2
# 297        3      1   3  24        1   6.1      2
# 298        4      1   3  59        5   5.5      2
# 299        3      0   1  45        4   5.1      2
# 300        1      1   3  27        2   4.4      2


# 단계. 데이터 셋 구조 보기
names(dataset) # 변수명(컬럼)
# [1] "resident" "gender"   "job"      "age"      "position" "price"    "survey"  

attributes(dataset) # names(열이름), class, row.names(행이름)

# 단계. 데이터 셋 조회 
dataset$age # dataset의 age 컬럼 출력

dataset$resident # dataset의 resident 컬럼 출력

length(dataset$age) # age 컬럼의 데이터 갯수

# 조회된 결과를 변수에 저장하는 방식
# dataset의 gender 컬럼을 변수 x에 저장한다.
x = dataset$gender                              
x

y = dataset$price
y

# 산점도 형태로 구매 가격 변수 조회하기
plot(dataset$price)                              

# $기호 대신 [""]기호를 이용한 변수 조회
dataset["gender"] 
dataset["price"]

# 색인(index)으로 칼럼 조회 
dataset[2] # 두 번째 컬럼
dataset[6] # 여섯번째 컬럼
dataset[3,] # 3번째 관찰치(행) 전체
dataset[,3] # 3번째 변수(열) 전체

# --dataset에서 2개 이상 칼럼 조회
dataset[c("job","price")]
dataset[c(2,6)] 

dataset[c(1,2,3)] 
dataset[c(1:3)] 
dataset[c(2,4:6,3,1)] 

# dataset의 특정 행/열을 조회하는 경우
dataset[, c(2:4)] # 2~4열
dataset[ c(2:4), ] # 2~4행 전체
dataset[-c(1:100), ]# 1~100행은 제외



###########################################
# 카페 1287번

# 결측치

summary(dataset$price)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -457.200    4.425    5.400    8.752    6.300  675.000       30 
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -457.200    4.425    5.400    8.752    6.300  675.000       30 

# 결측치가 포함된 데이터를 대상으로 sum() 함수를 적용하면 NA가 출력이 된다.
sum(dataset$price)
# [1] NA

# 결측치 제거 방법1
# na.rm=T 속성을 설정하면 해당 컬럼의 결측치를 제거해준다
sum(dataset$price, na.rm=T)
# [1] 2362.9

# 결측치 제거 방법2
# price 컬럼에 있는 모든 NA 제거
price2 = na.omit(dataset$price) 

sum(price2)    # 2362.9
# [1] 2362.9

length(price2) # 결측치 30개가 제거되었으므로 300 - 30 = 270개
# [1] 270

#  결측치 대체하기
x = dataset$price  # price vector 생성 

x[1:30] # 5.1 4.2 4.7 3.5 5.0
#  [1]   5.1   4.2   4.7   3.5   5.0   5.4   4.1 675.0   4.4   4.9   2.3   4.2   6.7   4.3
# [15] 257.8   5.7   4.6   5.1   2.1   5.1   6.2   5.1   4.1   4.1 -75.0   2.3   5.0    NA
# [29]   5.2   4.7

# 결측치를 0으로 대체한다.
dataset$price2 = ifelse(!is.na(x), x, 0) 

dataset$price2[1:30]
#  [1]   5.1   4.2   4.7   3.5   5.0   5.4   4.1 675.0   4.4   4.9   2.3   4.2   6.7   4.3
# [15] 257.8   5.7   4.6   5.1   2.1   5.1   6.2   5.1   4.1   4.1 -75.0   2.3   5.0   0.0
# [29]   5.2   4.7

x = dataset$price # price vector 생성 

x[1:30] # 5.1 4.2 4.7 3.5 5.0
#  [1]   5.1   4.2   4.7   3.5   5.0   5.4   4.1 675.0   4.4   4.9   2.3   4.2   6.7   4.3
# [15] 257.8   5.7   4.6   5.1   2.1   5.1   6.2   5.1   4.1   4.1 -75.0   2.3   5.0    NA
# [29]   5.2   4.7

# 결측치를 평균으로 대체한다.
dataset$price3 = ifelse(!is.na(x), x, round(mean(x, na.rm=TRUE), 2) )     

dataset$price3[1:30]
#  [1]   5.10   4.20   4.70   3.50   5.00   5.40   4.10 675.00   4.40   4.90   2.30   4.20
# [13]   6.70   4.30 257.80   5.70   4.60   5.10   2.10   5.10   6.20   5.10   4.10   4.10
# [25] -75.00   2.30   5.00   8.75   5.20   4.70

# 결측치, 결측치 0으로 대체, 결측시 평균으로 대체한 컬럼들을 동시에 보기
dataset[c('price', 'price2', 'price3')]

############# 극단치 처리(#1288) ###############################################################################
# gender 컬럼은 성별을 명목 척도로 표현한 변수이다.
# 범주형 변수의 극단치 처리
gender = dataset$gender

gender # 0, 5의 극단치 값이 보인다.

table(gender)

windows(height = 12, width = 10)
pie(table(gender))


# 파일 이름 : before_극단치.png(그림 참고)

# subset() 함수를 이용하여 성별 변수를 정제한다.
dataset = subset(dataset, gender==1 | gender==2)
dataset # gender변수 데이터 정제

length(dataset$gender) # 297개 - 3개 정제됨
pie(table(dataset$gender))
# 연속형 변수의 극단치 처리
dataset$price # 세부 데이터 보기

length(dataset$price) # 300개(NA포함)
# [1] 297

plot(dataset$price)   # 산점도 

summary(dataset$price) # 범위확인
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -457.200    4.400    5.400    8.784    6.300  675.000       30 
# 연속형 변수는 산점도를 이용하여 전반적인 분포 형태를 보면서 극단치를 확인하는 것이 좋다.
# 또한 summary() 함수에서 제공하는 요약 통계량(최소, 최대, 평균 등등)을 통해서 극단치를 어떻게 처리할 것인가를 결정한다.


# 1289 번
dataset = read.csv("dataset2.csv", header=TRUE)   # 헤더가 있는 경우
dataset2 = subset(dataset, dataset$price >= 2 & dataset$price <= 8) 

# 가독성을 위한 코딩 변경하기
# dataset$새칼럼[조건식] = "값"
dataset2$resident2[dataset2$resident == 1] ='1.서울특별시'
dataset2$resident2[dataset2$resident == 2] ='2.인천광역시'
dataset2$resident2[dataset2$resident == 3] ='3.대전광역시'
dataset2$resident2[dataset2$resident == 4] ='4.대구광역시'
dataset2$resident2[dataset2$resident == 5] ='5.시구군'

head( dataset2[c("resident","resident2")] ) # 2개만 지정
#   resident    resident2
# 1        1 1.서울특별시
# 2        2 2.인천광역시
# 3       NA         <NA>
# 4        4 4.대구광역시
# 5        5     5.시구군
# 6        3 3.대전광역시

dataset2$job2[dataset2$job == 1] = '공무원'
dataset2$job2[dataset2$job == 2] = '회사원'
dataset2$job2[dataset2$job == 3] = '개인사업'

head(dataset2[c('job', 'job2')])
#   job     job2
# 1   1   공무원
# 2   2   회사원
# 3   2   회사원
# 4  NA     <NA>
# 5   3 개인사업
# 6   2   회사원

# 척도 변경을 위한 코딩 변경하기
dataset2$age2[dataset2$age <= 30] ="청년층"
dataset2$age2[dataset2$age > 30 & dataset2$age <=55] ="중년층"
dataset2$age2[dataset2$age > 55] ="장년층"

head(dataset2)
#   resident gender job age position price survey    resident2     job2   age2
# 1        1      1   1  26        2   5.1      1 1.서울특별시   공무원 청년층
# 2        2      1   2  54        5   4.2      2 2.인천광역시   회사원 중년층
# 3       NA      1   2  41        4   4.7      4         <NA>   회사원 중년층
# 4        4      2  NA  45        4   3.5      2 4.대구광역시     <NA> 중년층
# 5        5      1   3  62        5   5.0      1     5.시구군 개인사업 장년층
# 6        3      1   2  57       NA   5.4      2 3.대전광역시   회사원 장년층

survey = dataset2$survey # 만족도 변수 추출
maxval <- max(survey)
csurvey = maxval + 1 - survey # 역코딩
csurvey

dataset2$survey = csurvey  # survery 수정
head(dataset2) # survey 결과를 확인해보도록 한다.
#   resident gender job age position price survey    resident2     job2   age2
# 1        1      1   1  26        2   5.1      1 1.서울특별시   공무원 청년층
# 2        2      1   2  54        5   4.2      2 2.인천광역시   회사원 중년층
# 3       NA      1   2  41        4   4.7      4         <NA>   회사원 중년층
# 4        4      2  NA  45        4   3.5      2 4.대구광역시     <NA> 중년층
# 5        5      1   3  62        5   5.0      1     5.시구군 개인사업 장년층
# 6        3      1   2  57       NA   5.4      2 3.대전광역시   회사원 장년층


### 코딩변경 예시 문제

abcCsv = read.csv("abc.csv", header=TRUE)

# 나이가 30세 이하는 "청년층", 45세 이하는 "중년층", 이외는 "장년층"이라고 하자.
# age2 컬럼을 코딩 변경(리코딩)하시오.

# 청년층 = 1, 중년층 = 2, 장년층 = 3이라고 하자.
# 숫자 1, 2, 3을 컬럼으로 가지는 age3 컬럼을 코딩 변경(리코딩)하시오. 
abcCsv$age2[abcCsv$age <= 30] <- '청년층'
abcCsv$age2[abcCsv$age > 30 &  abcCsv$age <= 45] <- '중년층'
abcCsv$age2[abcCsv$age > 45] <- '장년층'

abcCsv
#head(abcCsv[c("age", "age2", "age3")])
#   age   age2 age3
# 1  10 청년층    1
# 2  20 청년층    1
# 3  30 청년층    1
# 4  40 중년층    2
# 5  50 장년층    3
# 6  60 장년층    3 

# total 변수를 이용해 High 그룹과 Low 그룹으로 나누기
# High 그룹은 60이상이라고 가정한다.
# 이하는 'low'라고 가정한다.
# 이것을 위한 abcCsvtotal 컬럼을 리코딩하시오.

# survey 변수를 이용해 만족 그룹(Good)과 불만족 그룹(Bad)으로 나누기
# 만족 그룹(Good)은 3이상이라고 가정한다.


#최종 결과는 다음과 같다.
#gender job age position address total check price survey   age2 age3
#1      1   1  10        2   seoul    50     5  1200      3 청년층    1
#2      1   2  20        5   busan    80    NA    NA     NA 청년층    1
#3      1   2  30        4   daegu    60     5  2500      2 청년층    1
#4      1   3  40        4   busan    70     3  1200      5 중년층    2
#5      2   3  50        5   seoul    50     3  1400      6 장년층    3
#6      2   2  60        7   daegu    40     3  3000      1 장년층    3

#abcCsvtotal abcCsvsurvey
#1         low         Good
#2        high         <NA>
#  3        high          Bad
#4        high         Good
##5         low         Good
#6         low          Bad


# 1290번
setwd('C:\\work\\')
# 탐색적 분석을 위한 시각화	
newdata = read.csv("new_data.csv", header=TRUE)
str(newdata)
# 'data.frame':   251 obs. of  10 variables:
#  $ resident : int  1 2 NA 4 5 3 2 NA 2 5 ...
#  $ gender   : int  1 1 1 2 1 1 2 1 1 2 ...
#  $ job      : int  1 2 2 NA 3 2 1 1 2 NA ...
#  $ age      : int  26 54 41 45 62 57 36 56 37 29 ...
#  $ position : int  2 5 4 4 5 NA 3 5 3 2 ...
#  $ price    : num  5.1 4.2 4.7 3.5 5 5.4 4.1 4.4 4.9 2.3 ...
#  $ survey   : int  1 2 4 2 1 2 4 3 3 5 ...
#  $ resident2: Factor w/ 5 levels "1.서울특별시",..: 1 2 NA 4 5 3 2 NA 2 5 ...
#  $ job2     : Factor w/ 3 levels "개인사업","공무원",..: 2 3 3 NA 1 3 2 2 3 NA ...
#  $ age2     : Factor w/ 3 levels "장년층","중년층",..: 3 2 2 2 1 1 2 1 2 3 ...

# 컬럼 뒤에 숫자 2로 끝나는 컬럼은 기존 컬럼을 대상으로 코딩 변경한 컬럼이다.

newdata$gender2[newdata$gender == '1'] ="남자"
newdata$gender2[newdata$gender == '2'] ="여자"

# 거주 지역과 성별 컬럼으로 빈도 수 구하기
resident_gender <- table( newdata$resident2, newdata$gender2 )

resident_gender 
#                남자 여자
#   1.서울특별시   67   43
#   2.인천광역시   26   20
#   3.대전광역시   16   10
#   4.대구광역시    6    9
#   5.시구군       19   15

gender_resident <- table( newdata$gender2, newdata$resident2 )
#        1.서울특별시 2.인천광역시 3.대전광역시 4.대구광역시 5.시구군
#   남자           67           26           16            6       19
#   여자           43           20           10            9       15

windows(height = 12, width = 10)

# 성별에 따른 거주 지역 분포 현황 시각화
barplot( resident_gender, beside=T, horiz=T, col=rainbow(5),
         legend=row.names( resident_gender),
         main='성별에 따른 거주 지역 분포 현황')

savePlot('성별에 따른 거주 지역 분포 현황.png', type='png')

# 성별에 따른 거주 지역의 분포 현황을 보면 남여 모두 서울시에 거주자가 가장 많다.

# 거주 지역에 따른 성별 분포 현황
barplot( gender_resident, beside=T, horiz=T, col=rep(c(2, 4), 5),
         legend=c('남자', '여자'),
         main='거주 지역에 따른 성별 분포 현황')

savePlot('거주 지역에 따른 성별 분포 현황.png', type='png')

# 거주 지역에 따른 성별의 분포 현황을 보면 '서울 특별시' 거주자의 남여 비율이 가장 많다.

install.packages('lattice')
library(lattice)

windows(height = 12, width = 10)
#plot.points =TRUE : 밀도, auto.key=T : 범례
densityplot( ~ age, data=newdata, groups=job2, plot.points=TRUE, auto.key=T) 

savePlot('직업 유형에 따른 나이 분포 현황.png', type='png')

# 개인 사업은 20~30대, 공무원은 50~60대, 회사원은 40대가 가장 많이 분포 되었다.

newdata$position2[newdata$position == 1] = "1급"
newdata$position2[newdata$position == 2] = "2급"
newdata$position2[newdata$position == 3] = "3급"
newdata$position2[newdata$position == 4] = "4급"
newdata$position2[newdata$position == 5] = "5급"

densityplot( ~ price | factor(gender2), data=newdata, groups=position2, plot.points=TRUE, auto.key=T)

densityplot( ~ price | factor(position2), data=newdata, groups=position2, plot.points=TRUE, auto.key=T)
windows(height = 12, width = 10)
xyplot(price ~ age | factor(gender2), data=newdata)

getwd
getwd
setwd('C:/work')
#변수 member에 member2.csv 파일 내용을 저장하세요.
member <- read.csv('member2.csv', header = TRUE)
member
#변수 board에 board2.csv 파일 내용을 저장하세요.
board <- read.csv('board2.csv')
---------------------------------------------------------------------------------
  #  변수 member
  member
board

#성별 컬럼을 이용하여 성별2 컬럼을 리코딩하시오.
#1:남자, 2:여자
member$성별2[member$성별 == '1' ] <- '남자'
member$성별2[member$성별 == '2' ] <- '여자'
member

#적립포인트 컬럼을 이용하여 적립포인트2 컬럼을 리코딩하시오.
#포인트가 200이상이면 우수고객, 이하이면 일반고객이다.
member$적립포인트2[member$적립포인트 >= 200 ] <- '우수고객'
member$적립포인트2[member$적립포인트 < 200 ] <- '일반고객'
member
#---------------------------------------------------------------------------------
#  변수 board
#조회수 컬럼을 이용하여 조회수2 컬럼을 리코딩하시오.
#조회수2 컬럼 : 조회수가 3이상이면 '좋음', 3미만이면 '나쁨'으로 표현하도록 한다.

board$조회수2[board$조회수 >= 3 ] <- '좋음'
board$조회수2[board$조회수 < 3 ] <- '나쁨'
board
#library('dplyr')
#plyr 패키지를 이용하여 2개의 변수(member, board)를 병합하시오.
library('plyr')
#newdata 변수에 회원 이름별로 조회수의 평균과 적립 포인트의 토탈 금액을 구하시오.

newdataset <- join(member, board, by='아이디')
avg_df <- ddply(newdataset, .(이름), summarise, 조회수평균=mean(조회수), 적립포인트토탈=sum(적립포인트))
#avg_df2 <- ddply(newdataset, .(이름), summarise, 적립포인트토탈=sum(적립포인트))
#newdata <- select(avg_df, avg_df2) 
newdata <- ddply(newdataset, .(이름), summarise, 조회수평균=100* mean(조회수), 적립포인트토탈=sum(적립포인트))

newname  <- newdata['이름']
newname

mychart <- newdata[c('조회수평균', '적립포인트토탈')]
mychart <- as.matrix(mychart)
legend <- newname
barplot(mychart, beside=T, horize=T, main='차트그리기', col=rainbow(4), legend)





#newdata 변수를 이용하여 세로 막대 그래프를 그리시오.
#단, 조회수의 평균은 수치 100*을 곱하여 그리도록 한다.

#일련 번호가 1이거나 3인 데이터만 조회하되, 이름 급여 작성 일자만 조회하시오.
#단, 일련 번호의 역순으로 조회하시오.
library(dplyr)
newdataset %>% arrange( desc(일련번호) %>% filter(일련번호 %in% c(1,3)) %>% select(일련번호, 이름, 급여, 작성일자))

help(in)

