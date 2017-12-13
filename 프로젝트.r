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

# url <- 'jdbc:oracle:thin:@127.0.0.1:1521:xe'
#url <- 'jdbc:oracle:thin:@//127.0.0.1:1521/xe'
url <- 'jdbc:oracle:thin:@//192.168.0.62:1521/xe'
url

id <- 'madang'
password <- 'madang'

conn <- dbConnect( drv, url, id, password)
class( conn ) 

query = 'select power(2, 10) from dual'
result <- dbGetQuery(conn, query)
class( result ) # data.frame
result

query2 = 'select * from drug_2015'
result2 <- dbGetQuery(conn, query2)
class( result2 ) # data.frame
head(result2)


query3 = 'select * from asdf'
result3 <- dbGetQuery(conn, query3)
class( result3 ) # data.frame
result3

query4 = 'select * from table10'
result4 <- dbGetQuery(conn, query4)
class( result4 ) # data.frame
result4

# table10에 대하여 회원별 게시물 건수를 그래프로 작성하시오.
library(dplyr)
ans2 <- group_by(result4, NAME) # 회원별 게시물 건수
ans2
class( ans2)

#iris
#species<-group_by(iris, Species)
#species

barplot(ans2)
table_ans2 <- table(ans2$NAME)
table_ans2
windows(height = 12, width = 10)
barplot(table_ans2)

table_ans4 <- table(result4$NAME)
table_ans4
windows(height = 12, width = 10)
barplot(table_ans4)

setwd('C:\\work')
# 야구 선수들의 연봉 대비 출루율을 이용하여 막대 그래프를 그려 보세요.
data <- read.csv('야구성적.csv', header=T)
head(data, 25)
windows(height = 12, width = 10)
bp <- barplot(data$연봉대비출루율, main=paste("야구선수별 연봉 대비 출루율 분석", "\n", "(밥값여부계산 ^^)"), col=rainbow(25), cex.names=0.7, las=2, names.arg=data$선수명, ylim=c(0,50))

# y축 좌측 옆의 설명 문구
title(ylab="연봉대비출루율", col.lab="red")

aver <- 0 

for(i in 1:length(data$연봉대비출루율)){
  aver <- aver + data$연봉대비출루율[i]  
}

aver
aver <- aver/length(data$연봉대비출루율)
aver

abline(h=aver, col="blue")


text(x=aver-11, y=14.5, col="black", cex=0.8, labels = paste(aver, '%', "(평균출루율)"))

text(x=bp*1.01, y=data$연봉대비출루율*1.05, col="black", cex=0.7, labels=paste(data$연봉대비출루율, '%'))

savePlot('baseball_bar.png', type='png')

################################################################################
#1257번
setwd('C:/work')
data <- read.csv("주요선수별성적-2013년.csv", header=T)
data

# 선수명(2번째)와 연봉대비출루율(21), 연봉대비타점율(22) 번째 컬럼을 newdata에 저장
newdata <- data[, c(2,21,22)]
newdata

line1 <- data$연봉대비출루율
line2 <- data$연봉대비타점율

par(mar=c(5,4,4,4)+0.1) 

# 연봉 대비 출루율에 대한 꺽은 선 그래프
# type="o" : 선과 점 모두 그리기
# axes=F : 테두리 그리지 않기, 
plot(line1, type="o", axes=F, ylab="", xlab="", ylim=c(0,50), lty=2,col="blue", 
     main="한국프로야구선수별 기록분석-2013년", lwd=2)

# help(axis)

# axis() : 그래프에 축을 넣어 주는 함수이다.
# x 축
# side이 취할 수 있는 값 : 1=below, 2=left, 3=above and 4=right.
axis(side=1, at=1:25, lab=newdata$선수명, las=2)

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



windows(height = 12, width = 10)
##### 1538번
# 서울시 주요 병원 현황(정형)
data1 <- read.csv("2013년_서울_주요구별_병원현황.csv", header=T)
data1

barplot(as.matrix(data1[1:9,2:11]),
        main=paste("서울시 주요 구별 과목별-2013년","\n",
                   "출처[국민건강보험공단]") , ylab="병원수", beside=T ,
        col=rainbow(8),ylim=c(0,350))

abline(h=seq(0,350,10),lty=3,lwd=0.2)
name <- data1$표시과목
name
windows(height = 12, width = 10)
legend(75,350,name,cex=0.8,fill=rainbow(8),bg="white")