# 7.6.4 파생변수 합치기
library(plyr)

use_pay_data <- join(user_data, product_price, by='user_id')

