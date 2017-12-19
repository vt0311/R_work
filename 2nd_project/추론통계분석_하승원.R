setwd('C:/work')
getwd()
result <- read.csv('N06AB_2015_RE.csv', header = TRUE)
head(result)

result$SIDO_CODE2[result$SIDO_CODE == 11 ] <- '수도권'
result$SIDO_CODE2[result$SIDO_CODE == 26 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 27 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 28 ] <- '수도권'
result$SIDO_CODE2[result$SIDO_CODE == 29 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 30 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 31 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 36 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 41 ] <- '수도권'
result$SIDO_CODE2[result$SIDO_CODE == 42 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 43 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 44 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 45 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 46 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 47 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 48 ] <- '비수도권'
result$SIDO_CODE2[result$SIDO_CODE == 49 ] <- '비수도권'

result$DOSE_MULTI<- result$DOSE_ONEDAY * result$DOSE_DAYS 
table(result$RECUPERATE_DATE)

result2 <- subset(result, !is.na(result$DOSE_MULTI), c(table(result$RECUPERATE_DATE), SIDO_CODE2 ))
head(result2)
a <- subset(result2, SIDO_CODE2 == '수도권')
b <- subset(result2, SIDO_CODE2 == '비수도권')

a1 <- a$PRICE
b1 <- b$PRICE

length(a1) #14116
length(b1) #17004

mean(a1) # 18791.96
mean(b1) # 16245.21

# 동질성 검정의 귀무가설 : 두 집단(수도권과 비수도권의 항우울제 총 판매액) 간 분포의 모양이 동질적이다.
var.test(a1, b1)

# F test to compare two variances
# 
# data:  a1 and b1
# F = 1.4484, num df = 14115, denom df = 17003, p-value < 2.2e-16
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   1.403465 1.494917
# sample estimates:
#   ratio of variances 
# 1.448436 

# 검정통계량 p-value는 2.2e-16보다 작은 값이므로 두집단간의 분포형태가 동질하다고 볼수 없다.

hist(a1)
plot(result2$PRICE) # 산점도 이용 outlier 확인
barplot(result2$PRICE) # 막대차트 이용
mean(result2$PRICE) #] 17400.41

boxplot(result2$PRICE)

t.test(a1, b1)

# Welch Two Sample t-test
# 
# data:  a1 and b1
# t = 13.097, df = 27447, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   2165.615 2927.889
# sample estimates:
#   mean of x mean of y 
# 18791.96  16245.21 


t.test(a1, b1, alter='two.sided', conf.int=TRUE, conf.level = 0.95)


# Welch Two Sample t-test
# 
# data:  a1 and b1
# t = 13.097, df = 27447, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   2165.615 2927.889
# sample estimates:
#   mean of x mean of y 
# 18791.96  16245.21 

# p-value < 2.2e-16

t.test(a1, b1, alter="greater", conf.int = TRUE, conf.level = 0.95 )

# Welch Two Sample t-test
# 
# data:  a1 and b1
# t = 13.097, df = 27447, p-value < 2.2e-16
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   2226.895      Inf
# sample estimates:
#   mean of x mean of y 
# 18791.96  16245.21 

# p-value < 2.2e-16 : a1을 기준으로 비교 -> a1이 b1보다 크지 않다.


t.test(a1, b1, alter="less", conf.int = TRUE, conf.level = 0.95)

# Welch Two Sample t-test
# 
# data:  a1 and b1
# t = 13.097, df = 27447, p-value = 1
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf 2866.609
# sample estimates:
#   mean of x mean of y 
# 18791.96  16245.21 

# p-value = 1 : a1을 기준으로 비교 -> a1이 b1보다 작다.
