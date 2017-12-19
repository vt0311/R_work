
pres2013$SEX2[pres2013$SEX == '1' ] <- '남자'
pres2013$SEX2[pres2013$SEX == '2' ] <- '여자'

# x <- table(pres2013$SEX2)
# barplot(x)              
# y <- prop.table(x)
# round(y*100, 2)
#N06AB투약 성별비율
# 남자  여자 
# 36.32 63.68 

length(pres2013$AGE_CODE)
nrow(pres2013)

summary(pres2013$AGE_CODE) #AGE_CODE의 기술통계량
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00    9.00   12.00   11.55   15.00   18.00

pres2013$AGE_CODE2[pres2013$AGE_CODE == '1' ] <- '0~04'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '2' ] <- '05~09'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '3' ] <- '10~14'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '4' ] <- '15~19'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '5' ] <- '20~24'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '6' ] <- '25~29'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '7' ] <- '30~34'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '8' ] <- '35~39'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '9' ] <- '40~44'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '10' ] <- '45~49'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '11' ] <- '50~54'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '12' ] <- '55~59'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '13' ] <- '60~64'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '14' ] <- '65~69'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '15' ] <- '70~74'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '16' ] <- '75~79'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '17' ] <- '80~84'
pres2013$AGE_CODE2[pres2013$AGE_CODE == '18' ] <- '85+'

x1 <- table(pres2013$SEX2, pres2013$AGE_CODE2)

x1
windows()
max_y = 3000
bp <- barplot(x1, beside=T, main = '2012년 선택적 세로토닌 재흡수 억제제(N06AB) 나이별 남녀별 처방횟수', xlab = "나이(5세단위)", ylab = "처방횟수(건)", ylim = c(0, max_y), col= c('lightblue', 'pink'), legend = c('남', '여'))

text(x = bp, y =bp + 1000, labels = paste(ATC_N06AB,"건"), col = "black", cex = 0.5)