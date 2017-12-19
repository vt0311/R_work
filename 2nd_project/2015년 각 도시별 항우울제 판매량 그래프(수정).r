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

url <- 'jdbc:oracle:thin:@//192.168.0.62:1521/xe'
url

id <- 'madang'
password <- 'madang'

conn <- dbConnect( drv, url, id, password)
class( conn ) 

#query = 'select * from N06A_DRUG_2015 where atc_cd like \'N06AB%\''
#result <- dbGetQuery(conn, query)
getwd()
result <- read.csv('N06AB_2015_RE.csv', header = TRUE)
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
result$SIDO_CODE2[result$SIDO_CODE == 48 ] <- '경남'
result$SIDO_CODE2[result$SIDO_CODE == 49 ] <- '제주'


sido <- table(  result$SEX, result$SIDO_CODE2)
sido

windows()
barplot(sido, beside=T, ylim=c(0, 9000), ylab='판매량', col= c('lightblue', 'pink'), main='2015년 시도별 항우울제(N06AB) 판매량')
bp <- barplot(sido, beside=T, ylim=c(0, 9000), ylab='판매량', col= c('lightblue', 'pink'), legend = c('남', '여'), main='2015년 시도별 항우울제(N06AB) 판매량') 
text(x = bp, y = sido + 470, labels = paste(sido, "건"), col = "black", cex = 0.7)

pct <- round(sido/sum(sido) * 100 ,1)
text(x = bp, y = sido + 200, labels = paste( "(", pct, "%", ")" ), col = "black", cex = 0.7)


################################################################################
