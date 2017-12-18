####### mid_test_1 ##########################################################

# 문제1-1
hakseng <- read.csv('C:\\Rwork\\mid_test_1\\중간 평가 문제\\hakseng.csv', sep='#',header = T)

# 문제1-2 학생 평균 필요
mean_kor <- mean(hakseng$kor)
mean_eng <- mean(hakseng$eng)
mean_math <- mean(hakseng$math)

# 문제1-3
sub_mean<- c(mean_kor, mean_eng, mean_math)
lab <-t(hakseng)[seq(1,9,4)]
pie(sub_mean, main = '과목별 평균', col = rainbow(3), cex = 0.8, labels = lab)
savePlot("C:\\Rwork\\mid_test_1\\중간 평가 문제\\pbj_1_pieplot.png", type = "png")


# 문제1-4
par(las=2)
par(mar =c(8, 8, 1, 1))


barplot(as.matrix(hakseng[1:3, 2:4]), horiz = TRUE, beside = T, names.arg = hakseng$name, col = c("red", "green", "blue"), xlab = "점수",
	ylab = "이름")
legend("topright", legend=c("국어", "영어", "수학"),
       col=c("red", "green", "blue"), lty=1, cex=0.7)
savePlot("C:\\Rwork\\mid_test_1\\중간 평가 문제\\pbj_1_barplot.png", type = "png")


# 문제2
plus5 <- function(x){
	imsi <- x + 5
	return(imsi)
}

plus5(6)

# 문제3-1
mydata <- read.csv('C:\\Rwork\\mid_test_1\\중간 평가 문제\\mydata.csv')

# 문제3-2 리코딩
mydata$gender2[mydata$gender == 1] = "남자"
mydata$gender2[mydata$gender == 2] = "여자"
mydata$gender2[mydata$gender == 3] = "남자"
mydata$gender2[mydata$gender == 4] = "여자"

mydata$age2[mydata$age >= 10] = "10대"
mydata$age2[mydata$age >= 20] = "20대"
mydata$age2[mydata$age >= 30] = "30대"
mydata$age2[mydata$age >= 40] = "40대"
mydata$age2[mydata$age == 'NA'] = "20대"

mydata$salary2[mydata$salary < 150] = "저수익"
mydata$salary2[mydata$salary < 230] = "중수익"
mydata$salary2[mydata$salary >= 230] = "고수익"

mydata$survey2[mydata$survey == 1] = "가장 좋음"
mydata$survey2[mydata$survey == 2] = "좋음"
mydata$survey2[mydata$survey == 3] = "보통"
mydata$survey2[mydata$survey == 4] = "나쁨"
mydata$survey2[mydata$survey == 5] = "가장 나쁨"

mydata$brand2[mydata$brand == 1] = "lg(엘지)"
mydata$brand2[mydata$brand == 2] = "ss(삼성)"
mydata$brand2[mydata$brand == 3] = "hd(현대)"

# 문제3-3 기술 통계량 보고서
table_gender <- table(mydata$gender2)
table_age <- table(mydata$age2)
table_salary <- table(mydata$salary2)
table_survey <- table(mydata$survey2)
table_brand <- table(mydata$brand2)

# 문제4-1 계정, 테이블 생성 O

# 문제4-2 조회, 테이블 복제 O
#create table Mysale
#as
#select customers.name, books.bookname, orders.saleprice
#from ( customers inner join orders
#on customers.custid = orders.custid ) inner join books 
#on orders.bookid = books.bookid;

# 문제4-3 테이블 R에서 읽기 

library(DBI)
Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jdk1.8.0_144')
library(rJava)
library(RJDBC)

driver <- 'oracle.jdbc.driver.OracleDriver'
jarpath <- 'C:\\oraclexe\\app\\oracle\\product\\11.2.0\\server\\jdbc\\lib\\ojdbc6.jar'

drv <- JDBC(driver, jarpath)
class(drv)

url <- 'jdbc:oracle:thin:@//127.0.0.1:1521/xe'

id <- 'madang'
password <- 'madang'

conn <- dbConnect(drv, url, id, password)
class(conn)

query <- 'select * from Mysale'

MySale <- dbGetQuery(conn, query)

MySale$siljuk[MySale$SALEPRICE < 8000] = "나쁨"
MySale$siljuk[MySale$SALEPRICE < 15000] = "보통"
MySale$siljuk[MySale$SALEPRICE >= 15000] = "좋음"

# 문제4-4 데이터 조회
#create table MySummary
#as
#select customers.name, sum(orders.saleprice) as cus_name
#from customers inner join orders
#on customers.custid = orders.custid
#group by customers.name
#order by sum(orders.saleprice) desc ;

# 문제4-5 pie chart
query <- 'select * from MySummary'

MySummary <- dbGetQuery(conn, query)
lab <-t(MySummary)[seq(1,7,2)]
pie(MySummary$CUS_NAME, main = '김연아 짱', col = rainbow(4), cex = 0.8, labels = lab)
savePlot("C:\\Rwork\\mid_test_1\\중간 평가 문제\\pbj_4_pie.png", type = "png")

# 문제5 워드클라우드

poten <- scan('C:\\Rwork\\mid_test_1\\중간 평가 문제\\presidential address.txt', what = 'character')
str(poten)

# 단어 추출
wordcount <- table(poten)
head(wordcount, 15)

head(sort(wordcount, decreasing = T), 15)
mydata <- head(sort(wordcount, decreasing = T), 15)
xname <- data.frame(mydata)

# 바 플롯
# bp <- barplot(mydata, main = "대통령연설사", col = rainbow(10),
#	cex.names = 0.7, las= 2, ylim = c(0, 60))


# 워드 클라우드
library(wordcloud)
par(mar = c(2, 2, 2, 2))
set.seed(12345)
wordcloud(words = names(mydata), freq = mydata, scale = c(5, 0.5), min.freq = 1,
	color = rainbow(10), random.color = FALSE, random.order = FALSE, rot.per = 0.25)
savePlot("C:\\Rwork\\mid_test_1\\중간 평가 문제\\pbj_wordcloud.png", type = "png")

# 꺾은선
par(mar = c(5, 4, 4, 4) + 0.1)

plot(as.matrix(mydata), type = "o", axes = F, ylab = "", xlab = "", ylim = c(0, 50), lty =2, col = "blue",
	main = "대통령 연설사", lwd = 2)
axis(side = 1, at = 1:15, lab = xname$poten, las = 2)
axis(2, las = 1)
par(new = T)
savePlot("C:\\Rwork\\mid_test_1\\중간 평가 문제\\pbj_president_graph.png", type = "png")

