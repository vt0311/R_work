load data
infile 'test2.csv'
insert into table mytable2
FIELDS TERMINATED BY ','
TRAILING NULLCOLS(
	eventid integer,
	iyear char,
	imonth char,
	iday char,
	country char,
	country_txt char,
	summary char 
)