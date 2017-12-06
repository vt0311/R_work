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


#----1305��. (12/6 �� ����)




# ���� ���緮 Ȯ��
loadings <- result$loadings
dim( loadings ) # ���� ���緮�� ����
dim( subject ) # 6�� ������ ����

f������� <- numeric() # ���� ����
f��ȸ���� <- numeric()
f�ڿ����� <- numeric()

# subject�� ���� ����ŭ �ݺ��Ͽ� ���� ���� ������ ����Ѵ�.
for( i in 1:nrow(subject)){
  fs1_num = 0 ; fs2_num = 0 ; fs3_num = 0 ;
  
  for( k in 1:ncol(subject)){
    fs1_num <- fs1_num + ( loadings[k, 1] * subject[i, k ])
    fs2_num <- fs2_num + ( loadings[k, 2] * subject[i, k ])
    fs3_num <- fs3_num + ( loadings[k, 3] * subject[i, k ])
  } # inner for
  
  # ���� ���� ������ �����Ѵ�.
  f�������[i] <- fs1_num
  f��ȸ����[i] <- fs2_num
  f�ڿ�����[i] <- fs3_num
  
}# outer for


# ���� ���� ������ ����Ѵ�.
f�������
#  [1]  6.30794815  4.85689420  6.30794815  6.34667792  0.78908700
#  [6] -1.61400801  7.95940729  0.43581831  4.46475636 -0.02728237

f��ȸ����
#  [1]  6.353084  9.520256  6.353084  8.698168  5.426444  6.876991
#  [7] 12.826835  7.480108 10.444264  4.869055

f�ڿ�����
#  [1] -0.6752039  1.5042186 -0.6752039  0.6659686  3.6391260  5.8440915
#  [7]  0.4541748  4.3357697  3.4862670  5.9732138

