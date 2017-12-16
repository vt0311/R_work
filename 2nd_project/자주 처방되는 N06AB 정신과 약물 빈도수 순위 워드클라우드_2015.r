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

query = 'select * from N06A_DRUG_2015 where atc_cd like \'N06AB%\''
#query2 = 'select PRODUCT_NAME, COUNT(PRODUCT_NAME) from N06A_DRUG_2015 where ATC_CD like \'N06AB%\' group by PRODUCT_NAME'
result1 <- dbGetQuery(conn, query)
class( result1 ) # data.frame
head(result1)

#result2 <- table(result$COMPANY_NAME)
result2 <- table(result1$PRODUCT_NAME)
result2

#word_result <- head(sort(rowSums(result), decreasing=F), 10)
word_result <- result2 
word_result

# 빈도를 워드클라우드로 나타내기
library(wordcloud)
par(mar=c(2,2,2,2))
set.seed(12345)
palete <- rainbow(word_result)
#palete <- brewer.pal(9, 'Set1')
#x11()
windows()
wordcloud(words=names(word_result), freq=word_result, scale=c(5, 0.5), min.freq = 1000, colors=palete, random.color=FALSE, random.order=FALSE, rot.per=0.25  )
#warnings()
