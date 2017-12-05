# 1309��: ��ٱ��� ���� ��ǰ�� ������ �м�

# ���� ��Ģ ����
install.packages("arules")
library(arules)

setwd('C:/work')
# transaction ��ü ����(���� �̿�)
tran = read.transactions('tran.txt', format = "basket", sep=",")
tran


# transaction ������ ����
# Ʈ����� �׸��� ����Ѵ�.
inspect(tran)
#     items           
# [1] {���,����,����}
# [2] {���,���,����}
# [3] {���,����,���}
# [4] {���,����,����}
# [5] {���,���,����}
# [6] {����,����}     

# ��Ģ �߰�
# �������� �ŷڵ��� ���� ���� �߰ߵǴ� ��Ģ�� ���� ��������.
rule= apriori(tran, parameter = list(supp=0.3, conf=0.1))
rule= apriori(tran, parameter = list(supp=0.1, conf=0.1))

inspect(rule) # ���� ��Ģ ��� Ȯ���ϱ�
#      lhs            rhs    support   confidence lift  count
# �� �߷�
# [12] {����}      => {���} 0.1666667 0.5000000  0.750 1    
# �� �߷�
# [30] {���,����} => {����} 0.1666667 1.0000000  1.200 1    
# �� �߷�

# �ο� ����
# ���� �� ����� ������ �ŷ��� [12]������ Ȯ���� �� �ִ�.
# �������� 0.1666667�ε�, �ٸ� ����({����} => {����}, {���} => {���} )�� ���Ͽ� ���� ����.
# ��, �ش� �������� �ǸŵǴ� ����� ���� �ٸ� �ͺ��� ��������� ���ٴ� �ǹ��̴�.
# ���ָ� �����ϴ� ����� ��ü������ ��⸦ �� ���� �ʴ´�.

# ���, ���� �� ����� ������ �ŷ��� [30]������ Ȯ���� �� �ִ�.



#---------------------------------
# 1311��: �ķ�ǰ ����(Groceries) ���� �м�

library(arules)
data('Groceries')

str(Groceries)
Groceries

Groceries.df <- as(Groceries, 'data.frame')
str(Groceries.df)

head(Groceries.df)

rules = apriori(Groceries, parameter = list(supp=0.001, conf=0.8))

install.packages('arulesViz')
library(arulesViz)

# ��Ģ�� �����ϴ� ���� -> �������� item �󵵼� ����
windows()
plot(rules, method='grouped')


rules = apriori(Groceries, parameter = list(supp=0.001, conf=0.8, maxlen=3))
inspect(rules)


# �߰ߵ� ��Ģ �ð�ȭ
library(arulesViz)
plot(rules, method='graph', control = list(type='items'))
# 