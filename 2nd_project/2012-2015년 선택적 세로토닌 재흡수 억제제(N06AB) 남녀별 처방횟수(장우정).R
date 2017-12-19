#카이제곱검정, 귀무가설: 처방되는 N06AB(SSRI)의 종류는 성별과 관계가 없다.

setwd('H:\\N06AB')

pres2015 <- read.csv('N06AB_2015_RE.csv', header = TRUE)

pres2015$ATC_CD2[pres2015$ATC_CD == 'N06AB03' ] <- '03 플루옥세틴'
pres2015$ATC_CD2[pres2015$ATC_CD == 'N06AB04' ] <- '04 시탈로프람'
pres2015$ATC_CD2[pres2015$ATC_CD == 'N06AB05' ] <- '05 파록세틴'
pres2015$ATC_CD2[pres2015$ATC_CD == 'N06AB06' ] <- '06 설트랄린'
pres2015$ATC_CD2[pres2015$ATC_CD == 'N06AB08' ] <- '08 플루복사민'
pres2015$ATC_CD2[pres2015$ATC_CD == 'N06AB10' ] <- '10 에스시탈로프람'

pres2015$SEX2[pres2015$SEX == '1' ] <- '남'
pres2015$SEX2[pres2015$SEX == '2' ] <- '여'

table(pres2015$ATC_CD)
table(pres2015$SEX)

chisq.test(pres2015$ATC_CD, pres2015$SEX)
mytable <- xtabs( ~ SEX + ATC_CD, data=pres2015)
mytable

chisq.test( xtabs( ~ SEX + ATC_CD, data=pres2015) )
#########################################################
# 2012년Pearson's Chi-squared test
# data:  xtabs(~SEX + ATC_CD, data = pres2015)
# X-squared = 31.07, df = 5, p-value = 9.075e-06
##########################################################
# 2013년Pearson's Chi-squared test
#data:  xtabs(~SEX + ATC_CD, data = pres2015)
#X-squared = 87.353, df = 5, p-value < 2.2e-16
##########################################################
# 2014년Pearson's Chi-squared test
# data:  xtabs(~SEX + ATC_CD, data = pres2015)
# X-squared = 74.813, df = 5, p-value = 1.018e-14
##########################################################
# 2015년Pearson's Chi-squared test
# data:  xtabs(~SEX + ATC_CD, data = pres2015)
# X-squared = 109.91, df = 5, p-value < 2.2e-16
##########################################################
# p-value(2.2e-16) < 0.05 이므로 귀무 가설을 기각한다.
# 그러므로, 성별에 따라서 다른 종류의 N06AB(SSRI)가 처방된다.
############################################################
gender <- table(pres2015$SEX, pres2015$ATC_CD2)

windows()
max_y = 12000

bp <- barplot(gender, beside=T, main = "2015년 선택적 세로토닌 재흡수 억제제 처방횟수", xlab = "선택적 세로토닌 재흡수 억제제(N06AB)", ylab = "처방횟수(건)", ylim = c(0, max_y), col= c('lightblue', 'pink'))
text(x = bp, y = gender + 300, labels = paste(gender , "건"), col = "black", cex = 1)
