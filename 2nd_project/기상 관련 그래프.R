drug <- read.csv('D:/백업파일/acorn/Project/2차/2012-2015항우울증처방(N06AB)/2012_2015new.csv')

# 계절별 데이터 필터링
#1. spring
spring <- filter(drug, RECUPERATE_DATE >= 20120301 & RECUPERATE_DATE <= 20120531 | RECUPERATE_DATE >= 20130301 & RECUPERATE_DATE <= 20130531 | RECUPERATE_DATE >= 20140301 & RECUPERATE_DATE <= 20140531 | RECUPERATE_DATE >= 20150301 & RECUPERATE_DATE <= 20150531)
#2. summer
summer <- filter(drug, RECUPERATE_DATE >= 20120601 & RECUPERATE_DATE <= 20120831 | RECUPERATE_DATE >= 20130601 & RECUPERATE_DATE <= 20130831 | RECUPERATE_DATE >= 20140601 & RECUPERATE_DATE <= 20140831 | RECUPERATE_DATE >= 20150601 & RECUPERATE_DATE <= 20150831)
#3. fall
fall <- filter(drug, RECUPERATE_DATE >= 20120901 & RECUPERATE_DATE <= 20121130 | RECUPERATE_DATE >= 20130901 & RECUPERATE_DATE <= 20131130 | RECUPERATE_DATE >= 20140901 & RECUPERATE_DATE <= 20141130 | RECUPERATE_DATE >= 20150901 & RECUPERATE_DATE <= 20151130)
#4. winter
winter <- filter(drug, RECUPERATE_DATE >= 20120101 & RECUPERATE_DATE < 20120301 | RECUPERATE_DATE >= 20121201 & RECUPERATE_DATE <= 20121231 | RECUPERATE_DATE >= 20130101 & RECUPERATE_DATE < 20130301 | RECUPERATE_DATE >= 20131201 & RECUPERATE_DATE <= 20131231 | RECUPERATE_DATE >= 20140101 & RECUPERATE_DATE < 20140301 | RECUPERATE_DATE >= 20141201 & RECUPERATE_DATE <= 20141231 | RECUPERATE_DATE >= 20150101 & RECUPERATE_DATE < 20150301 | RECUPERATE_DATE >= 20151201 & RECUPERATE_DATE <= 20151231)

# 필요한 컬럼만 선정
#사실, 파일 불러오기 직후 선행되어야 하나, 그렇지 못해서 코드만 늘어난 결과를 낳음ㅠ
spring2 <- select(spring, DOSE_DAYS_sum:ilsohap_hr_sum)
summer2 <- select(summer, DOSE_DAYS_sum:ilsohap_hr_sum)
fall2 <- select(fall, DOSE_DAYS_sum:ilsohap_hr_sum)
winter2 <- select(winter, DOSE_DAYS_sum:ilsohap_hr_sum)

# 각 계절별 평균 산출
spring3 <- colMeans(spring2)
summer3 <- colMeans(summer2)
fall3 <- colMeans(fall2)
winter3 <- colMeans(winter2)

# dataframe 생성
season <- data.frame(spring3, summer3, fall3, winter3)

# 차트 작성
season2 <- t(season)
season3 <- t(season2)
df <- data.frame(season3)
names(df) <- c('봄','여름','가을','겨울')
df2 <- t(df)
drug2 <- select(df2, DOSE_DAYS_sum)
dfdrug <- data.frame(df2)
dfdrug2 <- select(dfdrug, DOSE_DAYS_sum)
dfdrug3 <- t(dfdrug2)
barplot(dfdrug3, xlab='계절',ylab='총투여일수', space=2, col=rep(c('green','blue','yellow','red'),4), main='계절별 항우울증 약물 충투여량')
#*주석이 필요없는 그래프이므로, 주석 추가 안함

dfdrug4 <- select(dfdrug, temp_diff_sum:ilsohap_hr_sum)
dfdrug5 <- t(dfdrug4)
barplot(dfdrug5, ylim=c(0, 20), beside=T, col=rainbow(3), xlab='계절',ylab='계절별 평균값', main='계절별 기후 변화')
legend(13, 20, c('기온차','일사량','일조량'), cex=0.8, fill=rainbow(3))
