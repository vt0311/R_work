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


#sido <- table(  result$SEX, result$SIDO_CODE2)
sido <- table(  result$SIDO_CODE2)
sido

# 강원   경기   경남   경북   광주   대구   대전   부산   서울   세종   울산   인천   전남   전북   제주   충남 
# 51396 505929 125135 117922 121138 126953  65551 126851 437947   9774  23543  77795 109804  95826  62592 129807 
# 충북 
# 81490

windows()
barplot(sido, beside=T, ylim=c(0, 9000), ylab='판매량', col= c('lightblue', 'pink'), main='2015년 시도별 항우울제(N06AB) 판매량')
bp <- barplot(sido, beside=T, ylim=c(0, 9000), ylab='판매량', col= c('lightblue', 'pink'), legend = c('남', '여'), main='2015년 시도별 항우울제(N06AB) 판매량') 
text(x = bp, y = sido + 470, labels = paste(sido, "건"), col = "black", cex = 0.7)

pct <- round(sido/sum(sido) * 100 ,1)
text(x = bp, y = sido + 200, labels = paste( "(", pct, "%", ")" ), col = "black", cex = 0.7)


################################################################################



sido_mat <- as.matrix(sido)

sido_mat

# 밀도로 나눈 것

# 강원  51396
# 경기 505929
# 경남 125135
# 경북 117922
# 광주 121138
# 대구 126953
# 대전  65551
# 부산 126851
# 서울 437947
# 세종   9774
# 울산  23543
# 인천  77795
# 전남 109804
# 전북  95826
# 제주  62592
# 충남 129807
# 충북  81490

## 인구로 나눈 것

# 강원  51396 / 1517 = 33.88
# 경기 505929 / 12423 = 40.725
# 경남 125135 / 3330 = 37.578
# 경북 117922 / 2678 = 
# 광주 121138 / 1506 =
# 대구 126953 / 2469 =
# 대전  65551 / 1542
# 부산 126851 / 3452
# 서울 437947 / 9941 = 44.05
# 세종   9774 / 187 = 
# 울산  23543 / 1164 = 20.226
# 인천  77795 / 2883 = 26.984
# 전남 109804 / 1797 = 61.104
# 전북  95826 / 1835 = 52.22
# 제주  62592 / 599 = 104.494
# 충남 129807 / 2103 = 61.72
# 충북  81490 / 1589 = 51.28

str(sido_mat)
sido_mat[,1]

# 강원   경기   경남   경북   광주   대구   대전   부산   서울   세종   울산   인천   전남   전북   제주   충남 
# 51396 505929 125135 117922 121138 126953  65551 126851 437947   9774  23543  77795 109804  95826  62592 129807 
# 충북 
# 81490 

# 강원   경기   경남   경북   광주   대구   대전   부산   서울   세종   울산   인천   전남   전북   제주   충남 
# 51396 505929 125135 117922 121138 126953  65551 126851 437947   9774  23543  77795 109804  95826  62592 129807 
# 충북 
# 81490


#시도 <- c('강원', '경기', '경남', '경북', '광주', '대구', '대전', '부산', '서울', '세종', '울산', '인천', '전남', '전북', '제주', '충남', '충북' )
#밀도 <- c(90, 1221, 316, 141, 3005, 2794, 2860, 4484, 16425, 403, 1097, 2748, 146, 227, 324, 256, 215  )
#인구 <- c(1517, 12423, 3330, 2678, 1506, 2469, 1542, 3452, 9941, 187, 1164, 2883, 1797, 1835, 599, 2103, 1589  )
#data2 <- data.frame(시도, 인구)
#data2
#data2[,2]
#str(data2)
#data2_mat <- as.matrix(data2)
#data2_mat
#str(t(data2_mat))
#data2_mat <- t(data2_mat)

#data2_mat[2,]

#시도 <- c('강원', '경기', '경남', '경북', '광주', '대구', '대전', '부산', '서울', '세종', '울산', '인천', '전남', '전북', '제주', '충남', '충북' )
graph1 <- sido_mat[1,] / data2[,2]
str(graph1)
graph_mat <- as.matrix(graph1)
graph_mat
windows()
barplot(graph_mat, beside=T)

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

#51396 / 1517 = 33.8800
#505929 / 12423 = 40.725
#125135 / 3330 = 37.578
# > 117922 / 2678 
# [1] 44.03361
# > 121138 / 1506
# [1] 80.43692
# >  126953 / 2469
# [1] 51.41879
# >   65551 / 1542
# [1] 42.51038
# > 126851 / 3452
# [1] 36.7471
# 437947 / 9941 = 44.05

# 9774 / 187
# [1] 52.26738
# >  23543 / 1164
# [1] 20.22595
# >  77795 / 2883
# [1] 26.98404
# >  109804 / 1797
# [1] 61.10406
# >   95826 / 1835
# [1] 52.22125
# >   62592 / 599
# [1] 104.4942
# >  129807 / 2103
# [1] 61.72468
# >   81490 / 1589
# [1] 51.28383
