member <- read.csv('member.csv',  header = TRUE)

mymember <- data.frame(급여 = member$급여, 포인트 = member$적립포인트*1000)

x11()
boxplot(mymember, xlab = '급여', ylab = '포인트', col = c("red","cyan"))
