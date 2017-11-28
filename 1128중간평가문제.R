# 문제 1.다음 물음에 답하세요.
getwd()
setwd('C:/work')
# 1) 학생 관련 파일 hakseng.csv을 읽어 들이세요.
hakseng <- read.csv('hakseng.csv', sep='#', header=T)

# 2) 각 과목의 평균 및 학생들의 평균을 구하세요.
hakseng
sub_mean <- c(mean(hakseng$kor), mean(hakseng$eng), mean(hakseng$math))
sub_mean

library('plyr')
hakseng
avg_df <- ddply(hakseng, .(name), summarise, 국어평균=mean(kor,), 영어평균=mean(eng), 수학평균=mean(math)  )
avg_df

help(mean)


# 3) 과목별 평균에 대하여 파이 차트를 그리세요.

# table 만들기
header <- c('국어', '영어', '수학')

somedata <- c(mean(hakseng$kor), mean(hakseng$eng), mean(hakseng$math))

testdata <- xtabs(somedata ~ header) ;

testdata

# 내림 차수 정렬
#testdata <- sort(testdata, decreasing=T)

testdata 

# pie 차트 만들기
# 색상 지정
colors <- c('red', 'green', 'blue')

colors 

# pie chart 그리기 최종 완성
# label 설정 : 건수
# paste() : 열거한 문자열들을 합쳐 주는 함수(concatenation)
label <- paste(names(testdata), '\n', testdata, '점')
# '\n'는 엔터키

windows()
# pie 차트
# cex : 글자 크기( default : 1 )
pie( testdata, main='과목별 평균', col=colors, cex=0.8, labels=label)



# 4) 학생별 과목별에 대한 가로 막대 그래프를 그리세요.
windows()
hakseng
#chart_data <-   c(mean(50, 60, 70), mean(60, 70, 80), mean(40, 50, 60) )
#chart_data

newdata <- ddply(hakseng, .(name), summarise, 
                 평균 = sum(kor+eng+math) / 3)
newdata

mychart <- newdata[c('평균')]
mychart

chart_data <- as.matrix(mychart)
chart_data
str(chart_data)

#chart_data2 <- chart_data[,2]
#dataframe2 <- dataframe(chart_data)
#chart_data2 <- c( chart_data[,2])
#chart_data2

newname <- newdata['name']
newname


windows()
barplot(chart_data, beside=T, horiz=T, xlab='점수', ylab='김철수 박영희 홍길동',  main='차트 그리기', col=rainbow(4))



# 문제 2. 숫자 1개를 입력하면 더하기 5를 수행해주는 함수 plus5를 작성하세요.
#x <- 6
#result1 = plus5(x)
#result1 # 11
plus5 <- function(x) {
  y = x + 5
  return (y)
} 
x <- 6
result1 = plus5(x)
result1 # 11

x <- 1
result1 = plus5(x)
result1 # 6

# 문제 3.다음 물음에 답하세요.
# 1) 엑셀 파일 mydata.csv를 변수 mydata에 읽어 들이시오
mydata <- read.csv('mydata.csv')
mydata
# 2) 다음 요구 조건에 맞는 코딩 변경을 수행하세요.
# (1) gender2 : 성별
mydata$gender2[mydata$gender == 1 | mydata$gender == 3] <- '남'
mydata$gender2[mydata$gender == 2 | mydata$gender == 4] <- '여'
mydata

# (2) age2 : 나이는 10대 ~ 40대으로 표현한다.
# 단, 결측치 나이는 20으로 치환하도록 한다.
mydata$age2[mydata$age >= 10 & mydata$gender < 20] <- '10대'
mydata$age2[mydata$age >= 20 & mydata$gender < 30] <- '20대'
mydata$age2[mydata$age >= 30 & mydata$gender < 40] <- '30대'
mydata$age2[mydata$age >= 40 & mydata$gender < 50] <- '40대'
mydata
mydata$age2 = ifelse(!is.na(mydata$age), mydata$age2, '20대')
mydata

# (3) salary2 : 급여 컬럼이다.
# '150'만 미만은 저수익, '230' 미만은 중수익, '230' 이상은 고수익으로 표기한다.
mydata$salary2[mydata$salary < 150 ] <- '저수익'
mydata$salary2[mydata$salary >= 150 & mydata$salary < 230] <- '중수익'
mydata$salary2[mydata$salary >= 230] <- '고수익'
mydata

# (4) survey2 : 만족도를 역코딩하세요.(1 : 가장 좋음 ~ 5 : 가장 나쁨)
survey <- mydata$survey
survey2 <- 6-survey
survey2
mydata$survey2 <- survey2

mydata$survey2

mydata$survey2[mydata$survey == 1] <- '가장좋음'
mydata$survey2[mydata$survey == 2] <- '좋음'
mydata$survey2[mydata$survey == 3] <- '보통'
mydata$survey2[mydata$survey == 4] <- '나쁨'
mydata$survey2[mydata$survey == 5] <- '가장나쁨'
mydata

# (5) brand2 : lg(엘지) ss(삼성) hd(현대)
mydata$brand2[mydata$brand == 1] <- '엘지'
mydata$brand2[mydata$brand == 2] <- '삼성'
mydata$brand2[mydata$brand == 3] <- '현대'
mydata

# 3) 기술 통계량 보고서를 작성하세요.
# 
# 문제 4.테이블 다루는 문제
# 1) 마당 생성 스크립트.txt 파일을 이용하여 테이블을 생성하세요.
# 주의 :  gomdori 계정이 아니다.

# 2) 고객 이름, 책 이름, 판매 가격을 조회하세요.
# CTAS 기법을 이용하여 테이블 MySale 테이블에 복제하세요.
# select customers.name, books.bookname, orders.saleprice
# from ( customers inner join orders
#        on customers.custid = orders.custid ) inner join books 
# on orders.bookid = books.bookid
# 
# 3) MySale 테이블을 R에서 읽어들이세요.
# siljuk : 판매가에 대한 실적 컬럼이다.
# 판매가가 8,000원 미만은 '나쁨', 8,000원 이상 그리고 15,000미만은 '보통', 15,000 이상은 '좋음'으로 표기한다.
# 
# 4) 고객별 총 구매 금액을 조회하되, 구매 금액이 높은 순으로 정렬하세요.
# CTAS 기법을 이용하여 테이블 MySummary 테이블에 복제하세요.
# 힌트 : CTAS를 사용할 때에는 함수는 반드시 alias를 사용해줘야 한다.
# select customers.name, sum(orders.saleprice)
# from customers inner join orders
# on customers.custid = orders.custid
# group by customers.name
# order by sum(orders.saleprice) desc ;
# 
# 5) MySummary 테이블을 R에서 읽어들이세요.
# 이에 대한 pie 차트를 그려 보세요.
# 
# 문제 5.워드 클라우드 문제
# 첨부된 presidential address.txt 파일을 이용하여 
# 꺽은 선 그래프와 
# 워드 클라우드를 만드세요.
# 단, 상위 15개만 그리도록 한다.

library(KoNLP)  

txt <- readLines("presidential address.txt") 
mode(txt)
# [1] "character"
class(txt)
# [1] "character"

pro <- sapply(txt, extractNoun, USE.NAMES=F)
pro
head(pro) 
mode(pro) 
class(pro) 
# list 형태로 출력됨을 확인됩니다

# 필터링을 위해 unlist 작업을 해서 저장합니다.
imsi <- unlist(pro) 
pro2 <- gsub("\\.","", imsi)
pro2 <- gsub("\\n","" ,pro2) 
pro2 <- gsub("\\d+","", pro2) 

# 두 글자 이상 되는 것만 필터링하기
#pro2 <- Filter(function(x) {nchar(x) >= 2}, pro2) 
#pro2 <- Filter(function(x) {nchar(x) <= 6} ,pro2) 

head(unlist(pro2), 15)
write(unlist(pro2),"pro_3.txt") 
rev <- read.table("pro_3.txt")
nrow(rev) 

wordcount <- table(rev)
head(wordcount, 15)

# 큰 값을 기준으로 상위 15개만 정렬하여 보여 준다.
head(sort(wordcount, decreasing=T),15)
mydata <- head(sort(wordcount, decreasing=T),15)
names(mydata)
#windows()
#bp <- barplot(mydata,  main = "TOP 15", col = rainbow(15), cex.names=0.7, las = 2,ylim=c(0,60))

#pct <- round(mydata/sum(mydata) * 100 ,1)

# 예시 : 8건
#text(x = bp, y = mydata * 1.05, labels = paste(mydata, "건"), col = "black", cex = 0.7)

# 예시 : 4.8%
#text(x = bp, y = mydata * 0.85, labels = paste( "(", pct, "%", ")" ), col = "black", cex = 0.7)

#savePlot('top 15_1.png', type='png')

library( wordcloud )
par(mar=c(2, 2, 2, 2))
set.seed( 12345 )
windows()
wordcloud(words=names(mydata), freq=mydata, scale=c(5, 0.5), min.freq=1, 
          color=rainbow(15), random.color=FALSE, random.order=FALSE, rot.per=0.25 )    


#### 꺾은 선 그래프

#data <- read.csv("주요선수별성적-2013년.csv", header=T)
#data

# 선수명(2번째)와 연봉대비출루율(21), 연봉대비타점율(22) 번째 컬럼을 newdata에 저장
#newdata <- data[, c(2,21,22)]
#newdata
mydata
line1 <- data$연봉대비출루율
line2 <- data$연봉대비타점율

par(mar = c(5, 4, 4, 4) + 0.1)

plot(as.matrix(mydata), type = "o", axes = F, ylab = "", xlab = "", ylim = c(0, 50), lty =2, col = "blue",
     main = "대통령 연설사", lwd = 2)
axis(side = 1, at = 1:15, lab = names(mydata), las = 2)
axis(2, las = 1)
par(new = T)

# 연봉 대비 출루율에 대한 꺽은 선 그래프
# type="o" : 선과 점 모두 그리기
# axes=F : 테두리 그리지 않기, 
plot(line1, type="o", axes=F, ylab="", xlab="", ylim=c(0,50), lty=2,col="blue", 
     main="한국프로야구선수별 기록분석-2013년", lwd=2)

# help(axis)

# axis() : 그래프에 축을 넣어 주는 함수이다.
# x 축
# side이 취할 수 있는 값 : 1=below, 2=left, 3=above and 4=right.
axis(side=1, at=1:25, lab=names(mydata), las=2)

# y 축
axis(2, las=1)
par(new=T) 

# 연봉 대비 타점율에 대한 꺽은 선 그래프
plot(line2, type="o", axes=F, ylab="", xlab="", ylim=c(0,50), lty=2, col="red")
axis(4, las=1) 

# 마진 영역(가장 왼쪽, 오른쪽 등등)에 글자를 넣어 주는 함수
mtext(side=4,line=2.5,"연봉대비 타점율") 
mtext(side=2,line=2.5,"연봉대비 출루율") 

# 모눈 격자
abline(h=seq(0,50,5),v=seq(1,25,1),col="gray",lty=2)

# 범례 넣기
legend(18,50,names(data[21:22]),cex=0.8,col=c("red","blue"),lty=1,lwd=2,bg="white")







# 1185번. Yesterday 워드클라우드
lyrics <- scan('presidential address.txt', what='character')
str(lyrics)
lyrics

grep('\\,', lyrics)

grep('\\.', lyrics)
grep('\\!', lyrics)
grep('\\?', lyrics)

lyrics2 <- gsub(',', '', lyrics)
lyrics2 <- gsub('\\.', '', lyrics2)
lyrics2 <- gsub('\\!', '', lyrics2)
lyrics2 <- gsub('\\,', '', lyrics2)
lyrics2

result1<-table(lyrics2)
result2<-sort(result1, decreasing = TRUE)

result3 <- result2[result2 > 1]
par(mar=c(4, 6, 4, 4))
windows(height = 12, width = 10)
barplot(rev(result3), horiz = TRUE, las=2, main='Beatles Yesterday', col='lightblue')

#install.packages('wordcloud')
library(wordcloud)
par(mar=c(2,2,2,2))
set.seed(12345)
wordcloud(words=names(result1), freq=result1, scale=c(5,0,5), min.freq = 1, color=rainbow(10), random.color=FALSE, random.order=FALSE, rot.per=0.25  )

