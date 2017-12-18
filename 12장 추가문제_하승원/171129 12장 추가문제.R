#### 추가 문제  ##########

# 문제 1.다음 물음에 답하세요.
# 
# 소괄호 안의 내용은 리코딩된 컬럼의 이름이다.
# 1) 엑셀 파일 somefile.csv를 변수 somefile에 읽어 들이시오.
somefile <- read.csv("somefile.csv", header = TRUE)
somefile

# 2) 세대별(age2)로 브랜드(brand2) 선호도에 차이가 있는 지 분석하시오.
# 수직 막대 그래프를 그리시오.
somefile$age2[somefile$age >= 10 & somefile$age < 20] <- '10대'
somefile$age2[somefile$age >= 20 & somefile$age < 30] <- '20대'
somefile$age2[somefile$age >= 30 & somefile$age < 40] <- '30대'
somefile$age2[somefile$age >= 40 & somefile$age < 50] <- '40대'


somefile$age2 = ifelse(!is.na(somefile$age), somefile$age2, '20대')
somefile
#unique(somefile$age2) 

somefile$brand2[somefile$brand == 1] <- '엘지'
somefile$brand2[somefile$brand == 2] <- '삼성'
somefile$brand2[somefile$brand == 3] <- '현대'
somefile$brand2[somefile$brand < 1 | somefile$brand > 3 ] <- '엘지'
unique(somefile$brand) 

somefile$brand2 = ifelse(!is.na(somefile$brand), somefile$brand2, '현대')
unique(somefile$brand2)
somefile

agebrand <- table(somefile$brand2, somefile$age2)
agebrand

windows()
barplot(agebrand, ylim=c(0, 120), beside=T, col=c( "blue", "red", "green"), legend=row.names(agebrand), main='세대별 브랜드 선호도')


# 3) 성별(gender2)로 브랜드(brand2) 선호도에 차이가 있는 지 분석하시오.
# 수평 막대 그래프를 그리시오.
somefile$gender2[somefile$gender == 1 | somefile$gender == 3] <- '남자'
somefile$gender2[somefile$gender == 2 | somefile$gender == 4] <- '여자'
unique(somefile$gender2) 

genderbrand <- table(somefile$brand2, somefile$gender2)
genderbrand

windows()
barplot(genderbrand, xlim=c(0, 200), horiz=T, beside=T, col=c("blue", "red", "green"), legend=row.names(genderbrand), main='성별 브랜드 선호도')


# 4) 전공별(subject2)로 영화(movie2) 선호도에 차이가 있는 지 분석하시오.
# 수직 누적 막대 그래프를 그리시오.
somefile$subject2[somefile$subject == 1] <- '기계'
somefile$subject2[somefile$subject == 2] <- '전자'
somefile$subject2[somefile$subject == 3] <- '전기'
unique(somefile$subject)
unique(somefile$subject2)


somefile$movie2[somefile$movie == 1] <- '액션'
somefile$movie2[somefile$movie == 2] <- '멜로'
somefile$movie2[somefile$movie == 3] <- '공포'
somefile$movie2[somefile$movie == 4] <- '환타지'
unique(somefile$movie)

somefile$movie2[somefile$movie < 1 | somefile$movie > 4 ] <- '액션'
somefile$movie2 = ifelse(!is.na(somefile$movie), somefile$movie2, '액션')
unique(somefile$movie2)
somefile

subjectmovie <- table(somefile$movie2, somefile$subject2)
subjectmovie

windows()
barplot(subjectmovie, ylim=c(0, 500), col=c("red", "green", "blue", "yellow"),  legend=row.names(subjectmovie), main ='전공별 영화 선호도')
 
# 참고
# (1) gender2 : 성별(남자 : 1, 3, 여자 : 2, 4)
# 
# (2) age2 : 나이는 10대 ~ 40대으로 표현한다.
# 단, 결측치 나이는 20으로 치환하도록 한다.
# 
# (3) brand2 : lg(엘지) ss(삼성) hd(현대)
#     1. lg(엘지) 2. ss(삼성) 3. hd(현대)
# 이상치는 1(엘지), 결측치는 3(현대)로 변경하여 처리하시오.
# unique(somefile$brand) 

# (4) movie2 : (액션 : 1, 멜로 : 2, 공포 : 3, 환타지 : 4)
# 이상치 및 결측치는 (액션 : 1)으로 변경하여 처리하시오.

# (5) subject2 : (기계:1, 전자:2, 전기:3)


