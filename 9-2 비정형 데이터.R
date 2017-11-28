getwd()

# 1185번. Yesterday 워드클라우드
lyrics <- scan('beatles_yesterday.txt', what='character')
str(lyrics)
lyrics

grep('\\,', lyrics)

grep('\\.', lyrics)
grep('\\!', lyrics)
grep('\\?', lyrics)

lyrics2 <- gsub(',', '', lyrics)
lyrics2 <- gsub('\\.', '', lyrics2)
lyrics2 <- gsub('\\!', '', lyrics2)
lyrics2 <- gsub('\\,', '', lyrics2)
lyrics2

result1<-table(lyrics2)
result2<-sort(result1, decreasing = TRUE)

result3 <- result2[result2 > 1]
par(mar=c(4, 6, 4, 4))
windows(height = 12, width = 10)
barplot(rev(result3), horiz = TRUE, las=2, main='Beatles Yesterday', col='lightblue')

#install.packages('wordcloud')
library(wordcloud)
par(mar=c(2,2,2,2))
set.seed(12345)
wordcloud(words=names(result1), freq=result1, scale=c(5,0,5), min.freq = 1, color=rainbow(10), random.color=FALSE, random.order=FALSE, rot.per=0.25  )




# 1539 번 프로포즈 선물 검색
install.packages("KoNLP")
library(KoNLP)

txt <- readLines("propose.txt")
mode(txt)
class(txt)

pro <- sapply(txt, extractNoun, USE.NAMES = F)
head(pro) 
mode(pro) 
class(pro) 
# list 형태로 출력됨을 확인됩니다

# 필터링을 위해 unlist 작업을 해서 저장합니다.
imsi <- unlist(pro) 
pro2 <- gsub("\\.","", imsi)
pro2 <- gsub("\\n","" ,pro2) 
pro2 <- gsub("\\d+","", pro2) 

# 프로포즈gsub.txt : 분석 배제할 단어를 담고 있는 텍스트 파일
txt <- readLines("프로포즈gsub.txt")
txt

cnt_txt <- length(txt)
cnt_txt

for( i in 1:cnt_txt) {
  pro2 <-gsub((txt[i]),"", pro2)
}

# 두 글자 이상 되는 것만 필터링하기(한글자 짜리는 제외한다.)
pro2 <- Filter(function(x) {nchar(x) >= 2}, pro2) 
# 7자 이상도 제외한다.
pro2 <- Filter(function(x) {nchar(x) <= 6} ,pro2) 

head(unlist(pro2), 20)
write(unlist(pro2),"pro_3.txt") 
rev <- read.table("pro_3.txt")
nrow(rev) 

wordcount <- table(rev)
head(wordcount, 20)

# 큰 값을 기준으로 상위 20개만 정렬하여 보여 준다.
head(sort(wordcount, decreasing=T),20)
mydata <- head(sort(wordcount, decreasing=T),10)
windows(height = 12, width = 10)
bp <- barplot(mydata,  main = "프로포즈 선물 TOP 10", col = rainbow(10), cex.names=0.7, las = 2,ylim=c(0,60))

pct <- round(mydata/sum(mydata) * 100 ,1)

# 예시 : 8건
text(x = bp, y = mydata * 1.05, labels = paste(mydata, "건"), col = "black", cex = 0.7)

# 예시 : 4.8%
text(x = bp, y = mydata * 0.85, labels = paste( "(", pct, "%", ")" ), col = "black", cex = 0.7)

#savePlot('프로포즈 선물 top 10_1.png', type='png')

#install.packages(c("KoNLP", "tm", "wordcloud"))



#####################[ Pie chart 소스 코드 ]#########################

install.packages("KoNLP") 
library(KoNLP)  
txt <- readLines("propose.txt") 
pro <- sapply(txt,extractNoun,USE.NAMES=F)
pro # list 형태로 출력됨을 확인됩니다

imsi <- unlist(pro) # 필터링을 위해 unlist 작업을 해서 저장합니다.

pro2 <- gsub("\\.","", imsi)
pro2 <- gsub("\\n","",pro2) 
pro2 <- gsub("\\d+","",pro2) 

txt <- readLines("프로포즈gsub.txt")
txt
cnt_txt <- length(txt)
cnt_txt

for( i in 1:cnt_txt) {
  pro2 <-gsub((txt[i]),"",pro2)     
}

pro2 <- Filter(function(x) {nchar(x) >= 2} ,pro2) # 두 글자 이상 되는 것만 필터링하기
pro2 <- Filter(function(x) {nchar(x) <= 6} ,pro2) 

head(unlist(pro2), 20)
write(unlist(pro2),"pro_3.txt")
rev <- read.table("pro_3.txt")
nrow(rev)

wordcount <- table(rev)
head(sort(wordcount, decreasing=T),20)
mydata <- head(sort(wordcount, decreasing=T),10)

pct <- round(mydata/sum(mydata) * 100 ,1)
names(mydata)
lab <- paste(names(mydata),"\n",pct,"%")
windows(height = 12, width = 10)
pie(mydata,main="프로포즈 선물 TOP 10",col=rainbow(10), cex=0.8,labels = lab)
savePlot('프로포즈 선물 top 10_2.png', type='png')


##########  테스트 ########################


txt <- readLines("propose.txt")
mode(txt)
class(txt)

pro <- sapply(txt, extractNoun, USE.NAMES = F)
head(pro) 
mode(pro) 
class(pro) 
# list 형태로 출력됨을 확인됩니다

# 필터링을 위해 unlist 작업을 해서 저장합니다.
imsi <- unlist(pro) 
pro2 <- gsub("\\.","", imsi)
pro2 <- gsub("\\n","" ,pro2) 
pro2 <- gsub("\\d+","", pro2) 

# 프로포즈gsub.txt : 분석 배제할 단어를 담고 있는 텍스트 파일
txt <- readLines("프로포즈gsub.txt")
txt

cnt_txt <- length(txt)
cnt_txt

for( i in 1:cnt_txt) {
  # pro2 중에서 txt중에 일치하는 것이 있으면 그것을 제외처리한다.
  pro2 <-gsub((txt[i]),"", pro2)
}

pro2

# poham.txt : 분석 포함할 단어를 담고 있는 텍스트 파일
txt2 <- readLines("poham.txt")
txt2

cnt_txt2 <- length(txt2)
cnt_txt2
library(stringr)

for( i in 1:cnt_txt2) {
  # pro2 중에서 txt2중에 일치하는 것이 있으면 그것을 처리한다.
  # pro3 <-gsub((txt2[i]),"", pro2)
  pro3 <- str_extract_all( pro2, (txt2[i]))
}

pro3

# 두 글자 이상 되는 것만 필터링하기(한글자 짜리는 제외한다.)
pro2 <- Filter(function(x) {nchar(x) >= 2}, pro2) 
# 7자 이상도 제외한다.
pro2 <- Filter(function(x) {nchar(x) <= 6} ,pro2) 

head(unlist(pro2), 20)
write(unlist(pro2),"pro_3.txt") 
rev <- read.table("pro_3.txt")
nrow(rev) 

wordcount <- table(rev)
head(wordcount, 20)

# 큰 값을 기준으로 상위 20개만 정렬하여 보여 준다.
head(sort(wordcount, decreasing=T),20)
mydata <- head(sort(wordcount, decreasing=T),10)
windows(height = 12, width = 10)
bp <- barplot(mydata,  main = "프로포즈 선물 TOP 10", col = rainbow(10), cex.names=0.7, las = 2,ylim=c(0,60))

pct <- round(mydata/sum(mydata) * 100 ,1)

# 예시 : 8건
text(x = bp, y = mydata * 1.05, labels = paste(mydata, "건"), col = "black", cex = 0.7)

# 예시 : 4.8%
text(x = bp, y = mydata * 0.85, labels = paste( "(", pct, "%", ")" ), col = "black", cex = 0.7)
