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

# table10�� ���Ͽ� ȸ���� �Խù� �Ǽ��� �׷����� �ۼ��Ͻÿ�.
library(dplyr)
ans2 <- group_by(result4, NAME) # ȸ���� �Խù� �Ǽ�
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
# �߱� �������� ���� ��� ������� �̿��Ͽ� ���� �׷����� �׷� ������.
data <- read.csv('�߱�����.csv', header=T)
head(data, 25)
windows(height = 12, width = 10)
bp <- barplot(data$������������, main=paste("�߱������� ���� ��� ����� �м�", "\n", "(�䰪���ΰ�� ^^)"), col=rainbow(25), cex.names=0.7, las=2, names.arg=data$������, ylim=c(0,50))

# y�� ���� ���� ���� ����
title(ylab="������������", col.lab="red")

aver <- 0 

for(i in 1:length(data$������������)){
  aver <- aver + data$������������[i]  
}

aver
aver <- aver/length(data$������������)
aver

abline(h=aver, col="blue")


text(x=aver-11, y=14.5, col="black", cex=0.8, labels = paste(aver, '%', "(��������)"))

text(x=bp*1.01, y=data$������������*1.05, col="black", cex=0.7, labels=paste(data$������������, '%'))

savePlot('baseball_bar.png', type='png')

################################################################################
#1257��
setwd('C:/work')
data <- read.csv("�ֿ伱��������-2013��.csv", header=T)
data

# ������(2��°)�� ������������(21), �������Ÿ����(22) ��° �÷��� newdata�� ����
newdata <- data[, c(2,21,22)]
newdata

line1 <- data$������������
line2 <- data$�������Ÿ����

par(mar=c(5,4,4,4)+0.1) 

# ���� ��� ������� ���� ���� �� �׷���
# type="o" : ���� �� ��� �׸���
# axes=F : �׵θ� �׸��� �ʱ�, 
plot(line1, type="o", axes=F, ylab="", xlab="", ylim=c(0,50), lty=2,col="blue", 
     main="�ѱ����ξ߱������� ��Ϻм�-2013��", lwd=2)

# help(axis)

# axis() : �׷����� ���� �־� �ִ� �Լ��̴�.
# x ��
# side�� ���� �� �ִ� �� : 1=below, 2=left, 3=above and 4=right.
axis(side=1, at=1:25, lab=newdata$������, las=2)

# y ��
axis(2, las=1)
par(new=T) 

# ���� ��� Ÿ������ ���� ���� �� �׷���
plot(line2, type="o", axes=F, ylab="", xlab="", ylim=c(0,50), lty=2, col="red")
axis(4, las=1) 

# ���� ����(���� ����, ������ ���)�� ���ڸ� �־� �ִ� �Լ�
mtext(side=4,line=2.5,"������� Ÿ����") 
mtext(side=2,line=2.5,"������� �����") 

# �� ����
abline(h=seq(0,50,5),v=seq(1,25,1),col="gray",lty=2)

# ���� �ֱ�
legend(18,50,names(data[21:22]),cex=0.8,col=c("red","blue"),lty=1,lwd=2,bg="white")



windows(height = 12, width = 10)
##### 1538��
# ����� �ֿ� ���� ��Ȳ(����)
data1 <- read.csv("2013��_����_�ֿ䱸��_������Ȳ.csv", header=T)
data1

barplot(as.matrix(data1[1:9,2:11]),
        main=paste("����� �ֿ� ���� ����-2013��","\n",
                   "��ó[���ΰǰ��������]") , ylab="������", beside=T ,
        col=rainbow(8),ylim=c(0,350))

abline(h=seq(0,350,10),lty=3,lwd=0.2)
name <- data1$ǥ�ð���
name
windows(height = 12, width = 10)
legend(75,350,name,cex=0.8,fill=rainbow(8),bg="white")