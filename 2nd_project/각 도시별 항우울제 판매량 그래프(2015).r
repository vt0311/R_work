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
result$SIDO_CODE2[result$SIDO_CODE == 48] <- '�泲'
result$SIDO_CODE2[result$SIDO_CODE == 49] <- '����'


sido <- table( result$SIDO_CODE2)
sido

windows()
barplot(sido, beside=T, ylim=c(0, 600000), ylab='�Ǹŷ�', col=rainbow(17), main='2015�� �õ��� �׿����(N06AB) �Ǹŷ�')
bp <- barplot(sido, beside=T, ylim=c(0, 600000), ylab='�Ǹŷ�', col=rainbow(17), main='2015�� �õ��� �׿����(N06AB) �Ǹŷ�') 
text(x = bp, y = sido + 30000, labels = paste(sido, "��"), col = "black", cex = 0.7)

pct <- round(sido/sum(sido) * 100 ,1)
text(x = bp, y = sido + 15000, labels = paste( "(", pct, "%", ")" ), col = "black", cex = 0.7)

sido_mat <- as.matrix(sido)

sido_mat
str(sido_mat)
sido_mat[,1]


�õ� <- c('����', '���', '�泲', '���', '����', '�뱸', '����', '�λ�', '����', '����', '���', '��õ', '����', '����', '����', '�泲', '���' )
#�е� <- c(90, 1221, 316, 141, 3005, 2794, 2860, 4484, 16425, 403, 1097, 2748, 146, 227, 324, 256, 215  )
�α� <- c(, 1221, 316, 141, 3005, 2794, 2860, 4484, 16425, 403, 1097, 2748, 146, 227, 324, 256, 215  )
data2 <- data.frame(�õ�, �е�)
data2
data2[,2]
str(data2)
data2_mat <- as.matrix(data2)
data2_mat
str(t(data2_mat))
data2_mat <- t(data2_mat)

data2_mat[2,]

�õ� <- c('����', '���', '�泲', '���', '����', '�뱸', '����', '�λ�', '����', '����', '���', '��õ', '����', '����', '����', '�泲', '���' )
graph1 <- sido_mat[1,] / data2[,2]
str(graph1)
graph_mat <- as.matrix(graph1)
graph_mat
graph2 <- data.frame(�õ�, graph_mat)
str(graph2)
#[1] 1279.300000   94.297297  364.357595  816.574468   38.315141   41.208661   40.257692   25.677297    7.009863  285.699752
#[11]  104.956244   41.898472  788.609589  507.211454  355.361111  449.753906  535.520930

getwd()
setwd('C:\\work')

pop <- read.csv('2015_�α�.csv', header=TRUE)
pop

windows()
barplot(graph2, beside=T, col=rainbow(17))

gender <- table(result$SEX, result$ATC_CD)
windows()
barplot(gender, beside=T)
