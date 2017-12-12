library(DBI)
Sys.setenv(JAVA_HOME='c:/program files/Java/jre1.8.0_144')
library(rJava)
library(RJDBC)

driver <- 'oracle.jdbc.driver.OracleDriver'
jarpath <- 'C:/oraclexe/app/oracle/product/11.2.0/server/jdbc/lib/ojdbc6.jar'
#jarpath <- 'C:/oraclexe/app/oracle/product/10.2.0/server/jdbc/lib/ojdbc14.jar'


drv <- JDBC(driver, jarpath)
class( drv )