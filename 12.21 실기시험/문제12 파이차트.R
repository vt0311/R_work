getwd()
setwd('C:/Users/acorn/Desktop')

board <- read.csv('board.csv', header = TRUE)
member <- read.csv('member.csv', header = TRUE)
head(board)
member


mydata <- merge(board, member, by='아이디')
head(mydata)

mytable <- table(mydata$성별)


#mytable2 <- table(board$이름)
#myframe <- sort(mytable2, decreasing = TRUE)
#barplot(myframe, xlab = '회원 이름', ylab = '게시물 건수', main = '회원별 게시물 작성 건수', col = rainbow(4))


windows()
pie(mytable, label = paste(paste(names(mytable), mytable), "건"), col = c("red","cyan"), main = "성별 게시물 작성 비율")
