#1593�� ���˿� ���ܱ����� ������� �м�

setwd('C:/work')
data <- read.csv('���˿Ϳܱ�����Ϲ�.csv')
data
head(data)

summary(data)

sd(data$����)
sd(data$�ܱ��ε��)
sd(data$�Ϲ��ֹ�)
 
# cor() : ��� ����� �����ش�.
# �Ǿ ��� ����� �̿��Ͽ� ������ ���� ��� ���踦 ���� ������ �Ѵ�.
cor(product$��ǰġ��)