#install.packages('DBI')
library(DBI)
#Sys.setenv(JAVA_HOME='c:/program files/Java/jre1.8.0_144')
#install.packages('rJava')
library(rJava)
#install.packages('RJDBC')
library(RJDBC)


driver <- 'oracle.jdbc.driver.OracleDriver'
driver
jarpath <- 'C:/oraclexe/app/oracle/product/11.2.0/server/jdbc/lib/ojdbc6.jar'


drv <- JDBC(driver, jarpath)
class( drv )
# [1] "JDBCDriver"
# attr(,"package")
# [1] "RJDBC"

url <- 'jdbc:oracle:thin:@//localhost:1521/xe'
#url <- 'jdbc:oracle:thin:@//192.168.0.62:1521/xe'
url

#id <- 'madang'
#password <- 'madang'

id <- 'scott'
password <- 'tiger'

conn <- dbConnect( drv, url, id, password)
class( conn ) 

#query = 'select * from N06A_DRUG_2015 where atc_cd like \'N06AB%\''
#query2 = 'select PRODUCT_NAME, COUNT(PRODUCT_NAME) from N06A_DRUG_2015 where ATC_CD like \'N06AB%\' group by PRODUCT_NAME'
#quary3 = 'select A.*, B.ATC_CD, (select INGREDIENT_NAME from GNL_NM_CD C where A.DRUG_INGREDIENT_CODE = C.DRUG_INGREDIENT_CODE  ) as INGREDIENT_NAME from (select * from DRUG_2015 ) A, (select atc_cd, DRUG_INGREDIENT_CODE from barcodedata b where atc_cd like \'N06AB%\' group by DRUG_INGREDIENT_CODE, atc_cd order by atc_cd) B where A.DRUG_INGREDIENT_CODE = B.DRUG_INGREDIENT_CODE'

#result1 <- dbGetQuery(conn, query3)
result1 <- read.csv('N06AB_2015_RE2.csv', header = TRUE)
#result1 <- read.csv('N06A_2015_RE2.csv', header = TRUE)
#result0 <- read.csv('N06_2015_RE2.csv', header = TRUE)
class( result1 ) # data.frame
head(result1)

result2 <- table(result1$DRUG_INGREDIENT_CODE)
#result2 <- table(result0$INGREDIENT_NAME)
result2

#word_result <- head(sort(rowSums(result), decreasing=F), 10)
word_result <- result2 
word_result

# 빈도를 워드클라우드로 나타내기
#install.packages('wordcloud')
library(wordcloud)
par(mar=c(2,2,2,2))
set.seed(12345)
palete <- rainbow(word_result)
#palete <- brewer.pal(9, 'Set1')
#x11()
windows()
#windows(width = '640', height = '400')
wordcloud(words=names(word_result), freq=word_result, scale=c(5, 0.5), min.freq = 1, colors=palete, random.color=FALSE, random.order=FALSE, rot.per=0.25  )
#warnings()
