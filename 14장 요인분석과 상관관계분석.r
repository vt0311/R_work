# 1121��. 
getwd()

testdata <- read.csv('���� ������ ���� �ð�1.csv')
testdata

# cor() : ��� ����� �����ش�.
result <- cor(testdata)
result

# �Ҽ��� 2° �ڸ����� �ݿø�
round(result, digits=2)

install.packages('corrplot')
library(corrplot)
windows()
corrplot(result, addCoef.col='black')

testdata

help(subset)
