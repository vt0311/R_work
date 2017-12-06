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

### 1621�� �ܼ� ���� ȸ�� �м� ����

product <- read.csv('product.csv', header=T)
head(product)

str(product)

y <- product$��ǰ_������
x <- product$��ǰ_������
df <- data.frame(x,y)

result.lm <- lm(formula = y ~ x, df)

result.lm

# Call:
#   lm(formula = y ~ x, data = df)
# 
# Coefficients:
#   (Intercept)            x  
# 0.7789       0.7393  

# ���� ���հ��� ���� ����
names(result.lm)

# ���� ���� �� ����
fitted.values(result.lm)[1:2]

# ���� ��
head(df, 1)

# ���� ���
3-3.735963

# ���� ������ residuals() �Լ��� ����Ͽ� ���� �� �ִ�.
residuals(result.lm)[1:2]

-0.735963 + 3.735963

## ���� ȸ�� �м� �� �ð�ȭ
#�ܰ�1. x, y�� ���� ������ �׸���
windows()
plot(formula = y ~ x, data = df)

# �ܰ�2. ���� ȸ�� �� ����
result.lm <- lm(formula = y ~ x, df)

# �ܰ�3. ȸ�� �� �׸���
abline(result.lm, col='red')

savePlot('���� ȸ�� �м� �� �ð�ȭ.png', type='png')

# ���� ȸ�� �м� ��� ����
summary(result.lm)




### 1622�� ���� ȸ�� �м�
# ���� ȸ�� �м��� ���� ���� ���� ������ �ϳ��� ���� ������ ��ġ�� ������ �м��� �� �̿��ϴ� �м� ��� �̴�.

str(product)
# 'data.frame':	264 obs. of  3 variables:
#   $ ��ǰ_ģ�е�: int  3 3 4 2 2 3 4 2 3 4 ...
# $ ��ǰ_������: int  4 3 4 2 2 3 4 2 2 2 ...
# $ ��ǰ_������: int  3 2 4 2 2 3 4 2 3 3 ...

head(product)

y = product$��ǰ_������  # ���� ����
x1 = product$��ǰ_ģ�е� # ���� ����1
x2 = product$��ǰ_������ # ���� ����2

df = data.frame(x1, x2, y)

result.lm = lm(formula=y ~ x1 + x2, data=df) 

# ����� ���� Ȯ��
result.lm 
# Call:
# lm(formula = y ~ x1 + x2, data = df)
# 
# Coefficients:
# (Intercept)           x1           x2  
#     0.66731      0.09593      0.68522  

# ���� ������ ���� Ȯ��
install.packages('car')
library(car)

# �л� ��â ������ ���� 10 �̻��� ��쿡�� ���� �������� ������ �ǽ��غ� �ʿ䰡 �ִ�.
vif(result.lm) # �л� ��â ���� Ȯ���ϱ�
#       x1       x2 
# 1.331929 1.331929 

summary(result.lm)





