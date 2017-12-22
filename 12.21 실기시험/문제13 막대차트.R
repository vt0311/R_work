board <- read.csv('board.csv', header = TRUE)
member <- read.csv('member.csv', header = TRUE)

mydata <- merge(board, member, by='아이디')
head(mydata)



mytable2 <- table(mydata$이름)
mytable2
myframe <- sort(mytable2, decreasing = TRUE)
windows()
barplot(myframe, xlab = '회원 이름', ylab = '게시물 건수', main = '회원별 게시물 작성 건수', col = rainbow(4))
