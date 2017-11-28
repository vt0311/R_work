# 1.dataset2의 직급(position) 칼럼을 대상으로 
# 1급 -> 5급, 5급 -> 1급 형식으로 역코딩하여 position2 칼럼에 추가하시오.
#dataset <- subset(dataset)
dataset2 <- subset(dataset, price >= 2 & price <=8)
dataset2
length(dataset2$price)
