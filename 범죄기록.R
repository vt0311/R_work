women
d
name <- c('kim', 'lee', 'choi', 'park')
which(name == 'choi')
no <- c(1:5)
name <- c('홍길동', '이순신', '강감찬', '유관순', '김유신')
score <- c(85, 78, 89, 90, 74)
exam <- data.frame(학번=no, 이름=name, 성적=score)
exam
which(exam$이름 == '유관순')
exam[4,]
exam[4,which(exam$이름 == '유관순')]
which(exam$이름 == '유관순')
exam[which(exam$이름 == '유관순'),]
load("C:\\Users\\acorn\\Documents\\111.r.RData")
load("C:\\work\\조건문실습.RData")
exam
myfunction <- function(){
cat('매개변수')
}
myfunc <- function(x){
imsi <- 2*x^2 - 3*x +1
}
myfunc(2)
myfunc <- function(x){
imsi <- 2*x^2 - 3*x +1
return(imsi)
}
myfunc(2)
x< - 1
x<- 1
result = myfunc(x)
result
x<-2
result
result = myfunc(x)
result
x<-3
result = myfunc(x)
result
calc <- function(x,y) {
imsi <- 2*x + 3*y +5
return(imsi)
}
calc(1,1)
calc(1,2)
calc(2,2)
q()
header <- c('마약', '방화', '살인', '절도', '폭력')
somedata <- c(10, 6, 8, 9, 7)
testdata <- xtabs(somedata ~ header) ;
testdata
pie(testdata, main='범죄건수', col=cols, cex=0.8, labels=label)
label <- paste(names(testdata), '\n', testdata, '건')
pie(testdata, main='범죄건수', col=cols, cex=0.8, labels=label)
pie(testdata, main='범죄건수', col=colors, cex=0.8, labels=label)
pie(testdata, main='범죄건수', col=rainbow(8), cex=0.8, labels=label)
