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

result$SIDO_CODE2[result$SIDO_CODE == 11 ] <- '����'
result$SIDO_CODE2[result$SIDO_CODE == 26 ] <- '�λ�'
result$SIDO_CODE2[result$SIDO_CODE == 27 ] <- '�뱸'
result$SIDO_CODE2[result$SIDO_CODE == 28 ] <- '��õ'
result$SIDO_CODE2[result$SIDO_CODE == 29 ] <- '����'
result$SIDO_CODE2[result$SIDO_CODE == 30 ] <- '����'
result$SIDO_CODE2[result$SIDO_CODE == 31 ] <- '���'
result$SIDO_CODE2[result$SIDO_CODE == 36 ] <- '����'
result$SIDO_CODE2[result$SIDO_CODE == 41 ] <- '���'
result$SIDO_CODE2[result$SIDO_CODE == 42 ] <- '����'
result$SIDO_CODE2[result$SIDO_CODE == 43 ] <- '���'
result$SIDO_CODE2[result$SIDO_CODE == 44 ] <- '�泲'
result$SIDO_CODE2[result$SIDO_CODE == 45 ] <- '����'
result$SIDO_CODE2[result$SIDO_CODE == 46 ] <- '����'
result$SIDO_CODE2[result$SIDO_CODE == 47 ] <- '���'
result$SIDO_CODE2[result$SIDO_CODE == 48 ] <- '�泲'
result$SIDO_CODE2[result$SIDO_CODE == 49 ] <- '����'


sido <- table(  result$SEX, result$SIDO_CODE2)
sido

windows()
barplot(sido, beside=T, ylim=c(0, 9000), ylab='�Ǹŷ�', col= c('lightblue', 'pink'), main='2015�� �õ��� �׿����(N06AB) �Ǹŷ�')
bp <- barplot(sido, beside=T, ylim=c(0, 9000), ylab='�Ǹŷ�', col= c('lightblue', 'pink'), legend = c('��', '��'), main='2015�� �õ��� �׿����(N06AB) �Ǹŷ�') 
text(x = bp, y = sido + 470, labels = paste(sido, "��"), col = "black", cex = 0.7)

pct <- round(sido/sum(sido) * 100 ,1)
text(x = bp, y = sido + 200, labels = paste( "(", pct, "%", ")" ), col = "black", cex = 0.7)


################################################################################
