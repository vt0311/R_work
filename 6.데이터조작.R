df_kor = data.frame(id=c(1,2,3), kor=c(95,90,75))

df_eng = data.frame(id=c(1,2,4), kor=c(80,75,88))

df_kor

df_eng

df_user = merge(df_kor, df_eng, by.x='id', by.y='id')
df_user

install.packages('plyr')
library(plyr)
left <- join(df_kor, df_eng, by='id')
left
right <- join(df_kor, df_eng, by='id', type='right')

right
data1 = data.frame(id= c(1,2,3,4,5), kor=c(60, 75, 80, 60, 70))
data2 = data.frame(id= c(5,4,1,3,2), kor=c(55,73,60,55,80))                   
result = join(data1, data2, by='id')


# member.csv 파일을 읽어 member 변수에 저장하세요.
member <- read.csv('member.csv')
member

# board.csv 파일을 읽어 board 변수에 저장하세요.
board <- read.csv('board.csv')
board

# merge 함수를 이용하여 데이터를 병합해 보세요.
# 몇건이 조회되는 가? 왜 그렇게 나오는 지 설명하세요.
library(plyr)
mergedata <- merge(member, board)
mergedata
# plyr 패키지를 설치하고 메모리에 로딩하세요.

# join() 함수를 이용하여 병합해 보세요.
joindata <- join(member, board, by='아이디')
joindata
# 몇건이 조회되는 가? 왜 그렇게 나오는 지 설명하세요.

# 'inner' 조인을 수행해 보세요.
# 몇건이 조회되는 가? 왜 그렇게 나오는 지 설명하세요.
innerdata <- join(member, board, by='아이디', type='inner')
innerdata
# 양쪽 모두를 포함하는 full join을 수행하세요.
# 몇건이 조회되는 가? 왜 그렇게 나오는 지 설명하세요.
fulldata <- join(member, board, by='아이디', type='full')
fulldata
# tapply 함수를 이용하여 각 성별로 급여의 평균을 구해보세요.
tapply(member$급여, member$성별, mean)
# tapply 함수를 이용하여 각 성별 적립 포인트의 총합을 구해보세요.
tapply(member$적립포인트, member$성별, sum)
# ddply() 함수를 사용하여 각 성별로 급여의 평균과 급여의 총금액을
# 각각 avg 변수와 tot 변수에 저장해보세요.
avg <- ddply(member, .(성별), summarise, avg = mean(급여) )
avg
avg(1)
avg[1,]
tot <- ddply(member, .(성별), summarise, tot = sum(급여))
tot
# 꽃의 종류별(Species)로 꽃받침 길이(Sepal.Length)의 평균 계산
avg_df <- ddply(iris, .(Species), summarise, avg=mean(Sepal.Length))
avg_df

#########################################################################
#파일 : 2013년_프로야구선수_성적.csv

#문제01 : 파일을 읽어와서 data1에 저장하세요.
data1 <- read.csv('2013년_프로야구선수_성적.csv')
data1
#문제02 : 경기 수가 120 이상인 선수만 필터링하세요. 
install.packages(c("dplyr", "hflights"))
library(dplyr)
filter(data1, 경기 >= 120)
#문제03 : 경기 수가 120 이상이고, 득점이 80이상인 선수만 필터링하세요. 
filter(data1, 경기 >=120 & 득점 >=80)

#문제04 : 포지션이 1루수와 3루수인 선수들만 필터링하세요. 
filter(data1, 포지션 == '1루수' | 포지션 == '3루수')

#문제05 : 선수명, 포지션, 팀 컬럼만 조회하세요.
select(data1, 선수명, 포지션, 팀)

#filter(data1, 선수명| 포지션 팀)
#문제06 : 순위 컬럼 부터 타수 컬럼까지 조회하세요. 
data1
select(data1, 순위:타수)

#문제07 : 홈런, 타점, 도루를 제외한 나머지 컬럼들만 조회하세요.
select(data1, -(홈런:도루))

#문제08 : 
  # %>% : 여러 개의 문장을 조합할 때 사용하는 문법
#  선수 이름, 팀, 경기, 타수를 조회하되, 타수가 400이상인 선수들만 출력하기


tempt <- filter(data1, 타수>=400)
tempt %>% 

temp2 <- tempt %>% select(선수명, 팀, 경기, 타수)

help('%>%')

#문제09 : 
#  선수명, 팀, 경기, 타수 컬럼을 조회하세요.
#단, 타수가 400 이상인 것만 조회하세요.
#타수로 정렬하세요.
select(data1, 선수명, 팀, 경기, 타수)
filter(data1, '타수'>=400)

arrange(temp2, 타수)



#문제10 : 
# 문제09번과 동일한 조건으로 하되, 타수의 역순으로 정렬하시오. 
arrange(temp2, desc(타수))

#문제11 : 
#  선수명, 팀, 경기, 타수 컬럼을 조회하세요.
#새로운 컬럼을 '결과'를 추가하세요.
#새로운 컬럼 결과는 다음과 같다고 한다.
#결과 = 경기 * 타수
#'결과' 컬럼을 기준으로 정렬하시오.
mutate()

#문제12 : 
  # 팀별로 평균 경기 횟수를 출력하기
  # 예를 들어, 삼성 선수는 2명이다.
  # 평균 경기 횟수 = (117 + 113) / 2 = 115이다.
  # 힌트 : group_by와 summarize를 사용하면 된다.
  # 결과 예시
  # # A tibble: 7 x 2
  #       팀 average
  #   <fctr>   <dbl>
  # 1    KIA     125
  # 2     LG     123
  # 3     SK     115
# 4   넥센     128
# 5   롯데     128
# 6   삼성     115
# 7   한화     101

#library("nycflights13")

bus <- read.csv('busdatamini.csv', header = TRUE)
head(bus)

extbus <- bus[, c(1,2,3,7,8)]
head(extbus)

install.packages('dplyr')
library(dplyr)

bus_top5 <- extbus %>% group_by(노선번호) %>% 
    summarise(승차총수 = sum(승차총승객수), 하차총수 = sum(하차총승객수)) %>%
    arrange(desc(승차총수)) %>% slice(1:5) 
bus_top5    
head( bus_stop5 )

data <- bus_top5 %>% select(2:3)
data <- t(data)
data

colors=c('red', 'blue')

mybarchart = barplot( data / 10000 , beside = T, main = "버스이용객 Top5", col=colors, ylab='승객수(만명)', xlab='노선명', ylim=c(0,120), names.arg= bus_top5$노선번호 )

grid()

text(x=mybarchart, y=data/10000 -5, labels=data, col='yellow', cex=0.6)

legend(12, 120, c('승차인원수', '하차인원수'), col=colors, bg='white', lty=1, lwd=2)
windows(height = 12, width = 10)

install.packages( 'reshape' )
library( reshape )

result <- read.csv('mysample.csv', header=F)
result
#       V1   V2 V3
# 1 김철수 국어 40
# 2 김철수 영어 50
# 3 박영희 국어 60
# 4 박영희 영어 70

# rename 함수 이름 컬럼 명 변경
result <- rename(result, c(V1='name', V2='subject', V3='jumsu'))

# 긴 형식 보기
head(result)
#     name subject jumsu
# 1 김철수    국어    40
# 2 김철수    영어    50
# 3 박영희    국어    60
# 4 박영희    영어    70

# 가로로 긴 형식을 세로로 넓은 형식으로 변경하기
wide <- reshape(result, idvar='name', timevar='subject',
                v.names='jumsu', direction='wide')
wide
#     name jumsu.국어 jumsu.영어
# 1 김철수         40         50
# 3 박영희         60         70

# 행변수와 열변수의 값이 조합이 되어 '넓은 형식'을 '긴 형식'로 지원한다.
reshape(wide, direction='long')
#               name subject jumsu
# 김철수.국어 김철수    국어    40
# 박영희.국어 박영희    국어    60
# 김철수.영어 김철수    영어    50
# 박영희.영어 박영희    영어    70

# varying 속성으로 색인 지정
# 컬럼 순서 번호 조합에 의해 '긴 형식'으로 변경한다.
# 2:3 --> 시작열번호:끝열번호
long <- reshape(wide, idvar='name', varying= 2:3, v.names='jumsu', direction='long')
#            name time jumsu
# 김철수.1 김철수    1    40
# 박영희.1 박영희    1    60
# 김철수.2 김철수    2    50
# 박영희.2 박영희    2    70


# reshape 패키지 활용하기2
# 참조용 파일 : pay_data_부분합.xlsx

install.packages( 'reshape2' )
library( reshape2 )

pay_data <- read.csv('pay_data.csv', header = T)

head(pay_data)
# user_id product_type pay_method  price
# 고객 아이디 상품 타입 지불 방법   가격

# 고객별 상품 유형에 따른 구매 금액을 구하시오.
# 예를 들어서, 김철수의 사과 총 구매 금액은 21,000원이다.
# 결과 행은 고객 id이고, 열은 상품 타입이다.
product_price <- dcast(pay_data, user_id ~ product_type , sum, na.rm=T)
product_price 

#  고객별 지불 유형에 따른 구매 금액을 구하시오.
# 예를 들어서, 김철수의 신용 카드 사용액은 14,000원이다.
# 결과 행은 고객 id이고, 열은 지불의 유형이다.
pay_price = dcast(pay_data, user_id ~ pay_method, sum, na.rm=T)
pay_price


install.packages( 'reshape2' )
data <- read.csv('data.csv')
head(data, 3)
#       Date Customer_ID Buy
# 1 20150101           1   3
# 2 20150101           2   4
# 3 20150102           1   2

# 1월 1일의 고객 아이디 1의 총합은 5이다.
wide <- dcast(data, Customer_ID ~ Date, sum)
# Using Buy as value column: use value.var to override.

# 파일 저장 및 읽기
write.csv( wide, 'wide.csv', row.names=FALSE)
wide <- read.csv('wide.csv')

colnames(wide)
# [1] "Customer_ID" "X20150101"   "X20150102"   "X20150103"   "X20150104"   "X20150105"   "X20150106"  
# [8] "X20150107"  

colnames(wide) <- c('Customer_ID', 'day2', 'day2', 'day3', 'day4', 'day5', 'day6', 'day7')

wide
#   Customer_ID day2 day2 day3 day4 day5 day6 day7
# 1           1    5    2    5    5    0    0    9
# 2           2    7    0    4    8    0    6    0
# 3           3    0    0    0    5    0    6    4
# 4           4    0    6    8    0    0    0    0
# 5           5    0    1    5    0    6    0   10

# 넓은 형식을 긴 형식으로 변경 : melt() 함수
# id 속성에 지정되어 있는 항목을 기준으로 '긴 형식'으로 변경하는 함수이다.
long <- melt(wide, id='Customer_ID')
head(long, 3)
#   Customer_ID variable value
# 1           1     day2     5
# 2           2     day2     7
# 3           3     day2     0

# 컬럼 이름 변경
colnames(long) <- c('Customer_ID', 'Date', 'Buy')
head(long, 3)
#   Customer_ID Date Buy
# 1           1 day2   5
# 2           2 day2   7
# 3           3 day2   0

students <- read.csv('students.csv')
students
#   subject time age weight height
# 1  김철수    1  33     90   1.87
# 2  박영희    1  NA     NA   1.54

# 첫번째 컬럼과 두번째 컬럼을 기준으로 긴 형식의 데이터프레임으로 변경
long <- melt(id=1:2, students)
head(long, 3)
#   subject time variable value
# 1  김철수    1      age    33
# 2  박영희    1      age    NA
# 3  김철수    1   weight    90

# 1520번
fruits <- read.csv('fruits_10.csv')
# 연도별 과일별 판매 수량과 금액
fruits

# melt 함수를 이용하여, 데이터를 아래로 길게(long) 보여 주기
# id 속성에 기준이 되는 변수 이름을 넣어 준다.
melt(fruits, id='year')

# 컬럼 여러 개를 이용하기
melt(fruits, id=c('year', 'name'))

# 컬럼의 이름을 호호호와 하하하로 변경하기
melt(fruits, id=c('year', 'name'), variable.name='asdf', value.name='qwert')

mtest <- melt(fruits, id=c('year', 'name'), variable.name='asdf', value.name='qwert')
mtest

# dcast 함수를 이용하여 원래 모양으로 되돌리기
dcast(mtest, year+name~asdf)

# melt 함수 사용시 사용했던 컬럼을 누락시키고 dcast를 적용하는 경우 경고 메시지가 나온다.
dcast(mtest, year~asdf)
# Using qwert as value column: use value.var to override.
# Aggregation function missing: defaulting to length

# name 별로 qty와 price의 합계 금액을 출력하는 예시
dcast(mtest, name~asdf, sum)

# subset 함수는 plyr 패키지가 필요하다.
library('plyr')
dcast(mtest, name~asdf, sum, subset=.(name == 'apple'))

# 6장 연습 문제
# 조건1) 꽃의 종류(Species)를 기준으로 '넓은 형식'을 '긴 형식'으로 변경하기.(melt 함수 이용)
newiris <- read.csv('newiris.csv')
newiris
wide 
reshape <- reshape(wide, direction = 'long')
