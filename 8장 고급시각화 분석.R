install.packages('lattice')
library(lattice)

install.packages('mlmRev')
library(mlmRev)
data(Chem97)
str(Chem97)
head(Chem97)
Chem97

# 히스토그램
windows()
histogram(~gcsescore, data=Chem97 )
#savePlot('Chem97~gcsescore.png', type='png')

histogram(~gcsescore | score, data=Chem97 )
#savePlot('Chem97~gcsescore_score.png', type='png')

histogram(~gcsescore | factor(score), data=Chem97 )
#savePlot('Chem97~gcsescore_factor_score.png', type='png')

# 밀도 그래프
densityplot(~gcsescore | factor(score), data=Chem97, groups=gender, plot.points=T, auto.key=T )
#savePlot('densityplot~gcsescore.png', type='png')

# 막대 그래프
data(VADeaths)
VADeaths

str(VADeaths)
class(VADeaths)
mode(VADeaths)

dft <- as.data.frame.table(VADeaths)
head(dft)
#    Var1         Var2 Freq
# 1 50-54   Rural Male 11.7
# 2 55-59   Rural Male 18.1
# 3 60-64   Rural Male 26.9
# 4 65-69   Rural Male 41.0
# 5 70-74   Rural Male 66.0
# 6 50-54 Rural Female  8.7

barchart(Var1 ~ Freq | Var2, data=dft, layout=c(4, 1))
savePlot('Var1~Freq_Var2.png', type='png')


barchart(Var1 ~ Freq | Var2, data=dft, layout=c(4, 1), origin=0)
savePlot('Var1~Freq_Var2.png', type='png')

# 점 그래프
dotplot(Var1 ~ Freq | Var2, dft)
savePlot('dotplot_2by2.png', type='png')

dotplot(Var1 ~ Freq | Var2, dft, layout=c(4, 1))
savePlot('dotplot_1by4.png', type='png')

dotplot(Var1 ~ Freq, data=dft, groups=Var2, type='o',
        auto.key=list(space='right', points=T, lines=T))
savePlot('dotplot_grouping.png', type='png')

# 산점도 그래프
library(datasets)
str(airquality)

xyplot(Ozone ~ Wind, data=airquality)
savePlot('xyplot_image_01.png', type='png')

xyplot(Ozone ~ Wind | Month, data=airquality)
savePlot('xyplot_basic_layout.png', type='png')

xyplot(Ozone ~ Wind | Month, data=airquality, layout=c(5,1))
savePlot('xyplot_basic_layout02.png', type='png')

convert <- transform(airquality, Month=factor(Month))
str(convert)
xyplot(Ozone ~ Wind | Month, data=convert, layout=c(5,1))
savePlot('xyplot_basic_layout03.png', type='png')

# 지진 발생 데이터셋 
# quakes으로 산점도 그리기
head(quakes)
str(quakes)

# 지진 발생 진앙지 산점도
# 산점도 그래프를 변수에 저장
tplot <- xyplot(lat ~ long, data=quakes, pch='.')

tplot2 <- update(tplot, main='1964년 이후 태평양에서 발생한 지진 위치')
print(tplot2) # 그래프에 제목 추가

# 수심의 범위 파악
range(quakes$depth)

# 6개의 범주로 리코딩
quakes$depth2[quakes$depth >= 40 & quakes$depth <= 150] <- 1
quakes$depth2[quakes$depth >= 151 & quakes$depth <= 250] <- 2
quakes$depth2[quakes$depth >= 251 & quakes$depth <= 350] <- 3
quakes$depth2[quakes$depth >= 351 & quakes$depth <= 450] <- 4
quakes$depth2[quakes$depth >= 451 & quakes$depth <= 550] <- 5
quakes$depth2[quakes$depth >= 551 & quakes$depth <= 608] <- 6

# 리코딩 변수를 조건으로 산점도 그리기
convert <- transform(quakes, depth2=factor(depth2)) # factor으로 변환
xyplot(lat ~ long | depth2, data=convert)
savePlot('xyplot_basic_with_depth.png', type='png')

# 동일한 패널에 2개의 변수 값 표현
xyplot(Ozone + Solar.R ~ Wind  | factor(Month), 
       col=c('blue', 'red'), data=airquality, layout=c(5,1))
savePlot('y_axis_2_condition.png', type='png')


# equal.count() : 지정된 범위 대상을 범주화 해주는 함수
# 형식) equal.count(data, number, overlap)
# 비율 척도 -> 범주화로 변경할 때 유용하게 사용한다.
numgroup <- equal.count(1:150, number=4, overlap=0)
numgroup

# 지진의 깊이를 5개 영역으로 범주화 
depthgroup=equal.count(quakes$depth, number=5, overlap=0)
depthgroup
# Intervals:
#     min   max count
# 1  39.5  80.5   203
# 2  79.5 186.5   203
# 3 185.5 397.5   203
# 4 396.5 562.5   202
# 5 562.5 680.5   200

# -- depthgroup변수 기준으로 플로팅
xyplot(lat ~ long | depthgroup, data=quakes,
       main="Fiji Earthquakes(depthgruop)",
       ylab="latitude", xlab="longitude", pch="@", col='red' )
savePlot('Fiji Earthquakes(depthgruop).png', type='png') 

# 수심과 리히터 규모 변수를 동시에 적용하기
magnitudegroup=equal.count(quakes$mag, number=2, overlap=0)
magnitudegroup
# Intervals:
#    min  max count
# 1 3.95 4.65   585
# 2 4.55 6.45   516

# magnitudegroup 변수 기준으로 산점도 그리기
xyplot(lat ~ long | magnitudegroup, data=quakes,
       main="Fiji Earthquakes(magnitudegruop)",
       ylab="latitude", xlab="longitude", pch="@", col='red' )
savePlot('Fiji Earthquakes(magnitude).png', type='png') 


# 수심과 리히터 규모를 동시에 표현(2행 5열 패널 구조)
# 수심(빨강), 리히터 규모(파랑)
xyplot(lat ~ long | depthgroup*magnitudegroup, data=quakes,
       main="Fiji Earthquakes(magnitudegruop)",
       ylab="latitude", xlab="longitude", pch="@", col='red' )
savePlot('Fiji Earthquakes(both).png', type='png') 

# 이산형 변수로 리코딩 한 후 factor형으로 변환하여 산점도 그래프 그리기
quakes$depth3[quakes$depth >= 39.5 & quakes$depth < 80.5] <- 'd1'
quakes$depth3[quakes$depth >= 80.5 & quakes$depth < 186.5] <- 'd2'
quakes$depth3[quakes$depth >= 186.5 & quakes$depth < 397.5] <- 'd3'
quakes$depth3[quakes$depth >= 397.5 & quakes$depth < 562.5] <- 'd4'
quakes$depth3[quakes$depth >= 562.5 & quakes$depth < 680.5] <- 'd5'

quakes$mag3[quakes$mag >= 3.95 & quakes$mag < 4.65] <- 'm1'
quakes$mag3[quakes$mag >= 4.65 & quakes$mag < 186.5] <- 'm2'

convert <- transform(quakes, depth3=factor(depth3))

xyplot(lat ~ long | depth3*mag3, data=convert,
       main="Fiji Earthquakes(magnitudegruop)",
       ylab="latitude", xlab="longitude", pch="@", col='red' )
savePlot('After Recoding.png', type='png') 


# 1321 번 : 막대 그래프 그려보기

library(ggplot2)
korean <- read.table('학생별국어성적_new.txt', header=T, sep=',')

ggplot(korean, aes(x=이름, y=점수)) + geom_bar(stat='identity', fill='green', color='red')

gg <-  ggplot(korean, aes(x=이름, y=점수)) + geom_bar(stat='identity', fill='green', color='red')

gg + theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1, colour = 'blue', size=8))

# 그래프에 여러과목 동시 출력하기
score_kem <- read.csv('학생별과목별성적_국영수_new.csv',header=T)
head(score_kem)
#      이름 과목 점수
# 1  박지영 국어   90
# 2  김태함 국어   70
# 3  김효섭 국어   92
# 4  임경희 국어   76
# 5  권혁진 국어   97
# 6  하혜진 국어   80

library( plyr )
# arrange : 데이터를 오름차순이나 내림차순으로 정렬한다.
# 즉, 이름으로 오름차 정렬 후 과목으로 오름차 정렬한다.
sort_kem <- arrange( score_kem, 이름, 과목 ) # 제본 교재 p149
head(sort_kem)
#     이름 과목 점수
# 1 권혁진 국어   97
# 2 권혁진 수학   83
# 3 권혁진 영어   87
# 4 김태함 국어   70
# 5 김태함 수학   80
# 6 김태함 영어   65

sort_kem2 <- ddply( sort_kem, "이름", transform, 누적합계=cumsum(점수) )
head(sort_kem2) # 제본 교재 p145
#     이름 과목 점수 누적합계
# 1 권혁진 국어   97       97
# 2 권혁진 수학   83      180
# 3 권혁진 영어   87      267
# 4 김태함 국어   70       70
# 5 김태함 수학   80      150
# 6 김태함 영어   65      215

# 그래프에서의 수치를 그래프의 정중앙에 출력하기 위하여 label 속성을 다음과 같이 설정한다.
# 예를 들어서 김태함의 수학은 다음과 같다.
# 김태함의 국어(70) + 0.5 * 수학(80) = 70 + 40.0 = 110.0이다.
sort_kem3 <- ddply( sort_kem2, "이름", transform, 누적합계=cumsum(점수), label=cumsum(점수) - 0.5 * 점수 )
head(sort_kem3)
#     이름 과목 점수 누적합계 label
# 1 권혁진 국어   97       97  48.5
# 2 권혁진 수학   83      180 138.5
# 3 권혁진 영어   87      267 223.5
# 4 김태함 국어   70       70  35.0
# 5 김태함 수학   80      150 110.0
# 6 김태함 영어   65      215 182.5

ggplot(sort_kem3, aes(x=이름, y=점수, fill=과목)) + geom_bar(stat='identity') + 
  geom_text(aes(y=label, label=paste(점수, '점')), colour='black', size=4 )

savePlot('두 과목 이상 01.png', type='png')

# 범례의 색상 순서와 실제 데이터의 색상 순서를 동일하게 맞춘다.
gg2 <-
  ggplot(sort_kem3, aes(x=이름, y=점수, fill=과목)) + geom_bar(stat='identity') + 
  geom_text(aes(y=label, label=paste(점수, '점')), colour='black', size=4 ) + 
  guides(fill=guide_legend(reverse=T))

gg2 + theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1, colour='black', size=8))

savePlot('두 과목 이상 02.png', type='png')

# 1324 번 : geom_point 함수

install.packages('gridExtra')
library(gridExtra)

cartest <- mtcars
cartest

windows()
graph1 <- ggplot(cartest, aes(x=hp, y=mpg))
graph1 + geom_point()

graph2 <- graph1 + geom_point(color='blue')
graph2

graph3 <- graph2 + geom_point(aes(color=factor(am)))
graph3

graph4 <- graph1 + geom_point(size=7)
graph4

graph5 <- graph1 + geom_point(aes(size=wt))
graph5

# 종류 별로 크기와 모양 지정하기
graph6 <- graph1 + geom_point(aes(shape=factor(am),size=wt))
graph6
#savePlot('graph6.png', type='png')

# 종류 별로 크기와 모양, 색상 지정하기
graph7 <- graph1 + geom_point(aes(shape=factor(am),color=factor(am),size=wt)) + 
  scale_color_manual(values=c('red', 'green'))
graph7
#savePlot('graph7.png', type='png')

# 선 추가하기
graph8 <- graph1 + geom_point(color='red') + geom_line()
graph8
#savePlot('graph8.png', type='png')

# x축과 y축 이름 바꾸기
graph9 <- graph1 + geom_point(color='blue') + labs(x='마력', y='연비')
graph9
#savePlot('graph9.png', type='png')


# 1260 번
getwd()
setwd('C:/work')
install.packages('ggmap')
library(ggmap)
library(grid)
pop <- read.csv("지역별인구현황_2014_4월기준.csv",header=T)
pop

lon <- pop$LON
lat <- pop$LAT
data <- pop$총인구수

# 위도, 경도, 총인구수를 데이터 프레임으로 만든다.
df <- data.frame(lon, lat, data)
df

map1 <- get_map("Jeonju",zoom=7 , maptype='roadmap')
map1 <- ggmap(map1)
map1 + geom_point(aes(x=lon,y=lat,colour=data,size=data),data=df)
ggsave("pop.png",scale=1,width=7,height=4,dpi=1000)

map2 <- get_map("Jeonju",zoom=7 , maptype='terrain')
map2 <- ggmap(map2)
map2 + geom_point(aes(x=lon,y=lat,colour=data,size=data),data=df)
ggsave("pop2.png",scale=1,width=7,height=4,dpi=1000)

map3 <- get_map("Jeonju",zoom=7 , maptype='satellite')
map3 <- ggmap(map3)
map3 + geom_point(aes(x=lon,y=lat,colour=data,size=data),data=df)
ggsave("pop3.png",scale=1,width=7,height=4,dpi=1000)

map4 <- get_map("Jeonju",zoom=7 , maptype='hybrid')
map4 <- ggmap(map4)
map4 + geom_point(aes(x=lon,y=lat,colour=data,size=data),data=df)
ggsave("pop4.png",scale=1,width=7,height=4,dpi=700)

map5 <- get_map("Jeonju",zoom=7 , maptype='roadmap')
map5 <- ggmap(map5)
map5 + stat_bin2d(aes(x=lon,y=lat,colour=data,fill=factor(data),size=data),data=df)
ggsave("pop5.png",scale=2,width=7,height=4,dpi=700)


