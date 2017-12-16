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

query = 'select * from N06A_DRUG_2015 where atc_cd like \'N06AB%\''
result <- dbGetQuery(conn, query)
class( result ) # data.frame
head(result)

result$SIDO_CODE2[result$SIDO_CODE == 11 ] <- '서울'
result$SIDO_CODE2[result$SIDO_CODE == 26 ] <- '부산'
result$SIDO_CODE2[result$SIDO_CODE == 27 ] <- '대구'
result$SIDO_CODE2[result$SIDO_CODE == 28 ] <- '인천'
result$SIDO_CODE2[result$SIDO_CODE == 29 ] <- '광주'
result$SIDO_CODE2[result$SIDO_CODE == 30 ] <- '대전'
result$SIDO_CODE2[result$SIDO_CODE == 31 ] <- '울산'
result$SIDO_CODE2[result$SIDO_CODE == 36 ] <- '세종'
result$SIDO_CODE2[result$SIDO_CODE == 41 ] <- '경기'
result$SIDO_CODE2[result$SIDO_CODE == 42 ] <- '강원'
result$SIDO_CODE2[result$SIDO_CODE == 43 ] <- '충북'
result$SIDO_CODE2[result$SIDO_CODE == 44 ] <- '충남'
result$SIDO_CODE2[result$SIDO_CODE == 45 ] <- '전북'
result$SIDO_CODE2[result$SIDO_CODE == 46 ] <- '전남'
result$SIDO_CODE2[result$SIDO_CODE == 47 ] <- '경북'
result$SIDO_CODE2[result$SIDO_CODE == 48] <- '경남'
result$SIDO_CODE2[result$SIDO_CODE == 49] <- '제주'


sido <- table( result$SIDO_CODE2)
sido

windows()
barplot(sido, beside=T, ylim=c(0, 600000), ylab='판매량', col=rainbow(17), main='2015년 시도별 항우울제(N06AB) 판매량')
bp <- barplot(sido, beside=T, ylim=c(0, 600000), ylab='판매량', col=rainbow(17), main='2015년 시도별 항우울제(N06AB) 판매량') 
text(x = bp, y = sido + 30000, labels = paste(sido, "건"), col = "black", cex = 0.7)

pct <- round(sido/sum(sido) * 100 ,1)
text(x = bp, y = sido + 15000, labels = paste( "(", pct, "%", ")" ), col = "black", cex = 0.7)

sido_mat <- as.matrix(sido)

sido_mat
str(sido_mat)
sido_mat[,1]


시도 <- c('강원', '경기', '경남', '경북', '광주', '대구', '대전', '부산', '서울', '세종', '울산', '인천', '전남', '전북', '제주', '충남', '충북' )
#밀도 <- c(90, 1221, 316, 141, 3005, 2794, 2860, 4484, 16425, 403, 1097, 2748, 146, 227, 324, 256, 215  )
인구 <- c(, 1221, 316, 141, 3005, 2794, 2860, 4484, 16425, 403, 1097, 2748, 146, 227, 324, 256, 215  )
data2 <- data.frame(시도, 밀도)
data2
data2[,2]
str(data2)
data2_mat <- as.matrix(data2)
data2_mat
str(t(data2_mat))
data2_mat <- t(data2_mat)

data2_mat[2,]

시도 <- c('강원', '경기', '경남', '경북', '광주', '대구', '대전', '부산', '서울', '세종', '울산', '인천', '전남', '전북', '제주', '충남', '충북' )
graph1 <- sido_mat[1,] / data2[,2]
str(graph1)
graph_mat <- as.matrix(graph1)
graph_mat
graph2 <- data.frame(시도, graph_mat)
str(graph2)
#[1] 1279.300000   94.297297  364.357595  816.574468   38.315141   41.208661   40.257692   25.677297    7.009863  285.699752
#[11]  104.956244   41.898472  788.609589  507.211454  355.361111  449.753906  535.520930

getwd()
setwd('C:\\work')

pop <- read.csv('2015_인구.csv', header=TRUE)
pop

windows()
barplot(graph2, beside=T, col=rainbow(17))

gender <- table(result$SEX, result$ATC_CD)
windows()
barplot(gender, beside=T)
