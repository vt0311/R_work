load data
infile 'test1.csv'
insert into table mytable1
FIELDS TERMINATED BY ','
TRAILING NULLCOLS(
	eventid integer,
	iyear char,
	imonth char,
	iday char,
	country char,
	country_txt char,
	region char,
	region_txt char,
	provstate char,
	city char,
	latitude char,
	longitude char
)