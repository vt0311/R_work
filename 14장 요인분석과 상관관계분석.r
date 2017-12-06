# 1121번. 
getwd()

testdata <- read.csv('시험 점수와 공부 시간1.csv')
testdata

# cor() : 상관 계수를 구해준다.
result <- cor(testdata)
result

# 소수점 2째 자리까지 반올림
round(result, digits=2)

install.packages('corrplot')
library(corrplot)
windows()
corrplot(result, addCoef.col='black')

testdata

help(subset)


#----1305번. (12/6 수 오후)




# 요인 적재량 확인
loadings <- result$loadings
dim( loadings ) # 요인 적재량의 차원
dim( subject ) # 6개 과목의 차원

f응용과학 <- numeric() # 벡터 변수
f사회과학 <- numeric()
f자연과학 <- numeric()

# subject의 차원 수만큼 반복하여 개별 요인 점수를 계산한다.
for( i in 1:nrow(subject)){
  fs1_num = 0 ; fs2_num = 0 ; fs3_num = 0 ;
  
  for( k in 1:ncol(subject)){
    fs1_num <- fs1_num + ( loadings[k, 1] * subject[i, k ])
    fs2_num <- fs2_num + ( loadings[k, 2] * subject[i, k ])
    fs3_num <- fs3_num + ( loadings[k, 3] * subject[i, k ])
  } # inner for
  
  # 개별 요인 점수를 저장한다.
  f응용과학[i] <- fs1_num
  f사회과학[i] <- fs2_num
  f자연과학[i] <- fs3_num
  
}# outer for


# 개별 요인 점수를 출력한다.
f응용과학
#  [1]  6.30794815  4.85689420  6.30794815  6.34667792  0.78908700
#  [6] -1.61400801  7.95940729  0.43581831  4.46475636 -0.02728237

f사회과학
#  [1]  6.353084  9.520256  6.353084  8.698168  5.426444  6.876991
#  [7] 12.826835  7.480108 10.444264  4.869055

f자연과학
#  [1] -0.6752039  1.5042186 -0.6752039  0.6659686  3.6391260  5.8440915
#  [7]  0.4541748  4.3357697  3.4862670  5.9732138

