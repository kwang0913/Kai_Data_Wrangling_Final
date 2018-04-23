library(tidyverse)
library(lubridate)
library(sqldf)

# 2013
citibike2013 <-
  bind_rows(
    citibike201307,
    citibike201308,
    citibike201309,
    citibike201310,
    citibike201311,
    citibike201312
  )

citibike2013$starttime  <- citibike2013$starttime  %>% ymd_hms()
citibike2013$stoptime   <- citibike2013$stoptime   %>% ymd_hms()
citibike2013$birth.year <- citibike2013$birth.year %>% as.character() %>% as.integer()
citibike2013$usertype   <- citibike2013$usertype   %>% as.character()

# 2014
citibike201401$birth.year <- citibike201401$birth.year %>% as.integer()
citibike201402$birth.year <- citibike201402$birth.year %>% as.integer()
citibike201403$birth.year <- citibike201403$birth.year %>% as.integer()
citibike201404$birth.year <- citibike201404$birth.year %>% as.integer()
citibike201405$birth.year <- citibike201405$birth.year %>% as.integer()
citibike201406$birth.year <- citibike201406$birth.year %>% as.integer()
citibike201407$birth.year <- citibike201407$birth.year %>% as.integer()
citibike201408$birth.year <- citibike201408$birth.year %>% as.integer()
citibike201409$starttime  <- citibike201409$starttime  %>% mdy_hms()
citibike201409$stoptime   <- citibike201409$stoptime   %>% mdy_hms()
citibike201410$starttime  <- citibike201410$starttime  %>% mdy_hms()
citibike201410$stoptime   <- citibike201410$stoptime   %>% mdy_hms()
citibike201411$starttime  <- citibike201411$starttime  %>% mdy_hms()
citibike201411$stoptime   <- citibike201411$stoptime   %>% mdy_hms()
citibike201412$starttime  <- citibike201412$starttime  %>% mdy_hms()
citibike201412$stoptime   <- citibike201412$stoptime   %>% mdy_hms()

citibike2014 <-
  bind_rows(
    citibike201401,
    citibike201402,
    citibike201403,
    citibike201404,
    citibike201405,
    citibike201406,
    citibike201407,
    citibike201408,
    citibike201409,
    citibike201410,
    citibike201411,
    citibike201412
  )

# 2015
citibike201501$starttime  <- citibike201501$starttime %>% mdy_hm()
citibike201501$stoptime   <- citibike201501$stoptime  %>% mdy_hm()
citibike201502$starttime  <- citibike201502$starttime %>% mdy_hm()
citibike201502$stoptime   <- citibike201502$stoptime  %>% mdy_hm()
citibike201503$starttime  <- citibike201503$starttime %>% mdy_hm()
citibike201503$stoptime   <- citibike201503$stoptime  %>% mdy_hm()
citibike201504$starttime  <- citibike201504$starttime %>% mdy_hms()
citibike201504$stoptime   <- citibike201504$stoptime  %>% mdy_hms()
citibike201505$starttime  <- citibike201505$starttime %>% mdy_hms()
citibike201505$stoptime   <- citibike201505$stoptime  %>% mdy_hms()
citibike201506$starttime  <- citibike201506$starttime %>% mdy_hm()
citibike201506$stoptime   <- citibike201506$stoptime  %>% mdy_hm()
citibike201507$starttime  <- citibike201507$starttime %>% mdy_hms()
citibike201507$stoptime   <- citibike201507$stoptime  %>% mdy_hms()
citibike201508$starttime  <- citibike201508$starttime %>% mdy_hms()
citibike201508$stoptime   <- citibike201508$stoptime  %>% mdy_hms()
citibike201509$starttime  <- citibike201509$starttime %>% mdy_hms()
citibike201509$stoptime   <- citibike201509$stoptime  %>% mdy_hms()
citibike201510$starttime  <- citibike201510$starttime %>% mdy_hms()
citibike201510$stoptime   <- citibike201510$stoptime  %>% mdy_hms()
citibike201511$starttime  <- citibike201511$starttime %>% mdy_hms()
citibike201511$stoptime   <- citibike201511$stoptime  %>% mdy_hms()
citibike201512$starttime  <- citibike201512$starttime %>% mdy_hms()
citibike201512$stoptime   <- citibike201512$stoptime  %>% mdy_hms()

citibike2015 <-
  bind_rows(
    citibike201501,
    citibike201502,
    citibike201503,
    citibike201504,
    citibike201505,
    citibike201506,
    citibike201507,
    citibike201508,
    citibike201509,
    citibike201510,
    citibike201511,
    citibike201512
  )

#2016
citibike201601$starttime  <- citibike201601$starttime %>% mdy_hms()
citibike201601$stoptime   <- citibike201601$stoptime  %>% mdy_hms()
citibike201602$starttime  <- citibike201602$starttime %>% mdy_hms()
citibike201602$stoptime   <- citibike201602$stoptime  %>% mdy_hms()
citibike201603$starttime  <- citibike201603$starttime %>% mdy_hms()
citibike201603$stoptime   <- citibike201603$stoptime  %>% mdy_hms()
citibike201604$starttime  <- citibike201604$starttime %>% mdy_hms()
citibike201604$stoptime   <- citibike201604$stoptime  %>% mdy_hms()
citibike201605$starttime  <- citibike201605$starttime %>% mdy_hms()
citibike201605$stoptime   <- citibike201605$stoptime  %>% mdy_hms()
citibike201606$starttime  <- citibike201606$starttime %>% mdy_hms()
citibike201606$stoptime   <- citibike201606$stoptime  %>% mdy_hms()
citibike201607$starttime  <- citibike201607$starttime %>% mdy_hms()
citibike201607$stoptime   <- citibike201607$stoptime  %>% mdy_hms()
citibike201608$starttime  <- citibike201608$starttime %>% mdy_hms()
citibike201608$stoptime   <- citibike201608$stoptime  %>% mdy_hms()
citibike201609$starttime  <- citibike201609$starttime %>% mdy_hms()
citibike201609$stoptime   <- citibike201609$stoptime  %>% mdy_hms()

colnames(citibike201610) <- colnames(citibike201609)
colnames(citibike201611) <- colnames(citibike201609)
colnames(citibike201612) <- colnames(citibike201609)

citibike2016 <-
  bind_rows(
    citibike201601,
    citibike201602,
    citibike201603,
    citibike201604,
    citibike201605,
    citibike201606,
    citibike201607,
    citibike201608,
    citibike201609,
    citibike201610,
    citibike201611,
    citibike201612
  )

# 2017
citibike201704$birth.year <- citibike201704$birth.year %>% as.integer()
citibike201705$birth.year <- citibike201705$birth.year %>% as.integer()
citibike201706$birth.year <- citibike201706$birth.year %>% as.integer()
citibike201707$birth.year <- citibike201707$birth.year %>% as.integer()
citibike201708$birth.year <- citibike201708$birth.year %>% as.integer()
citibike201709$birth.year <- citibike201709$birth.year %>% as.integer()
citibike201710$birth.year <- citibike201710$birth.year %>% as.integer()
citibike201711$birth.year <- citibike201711$birth.year %>% as.integer()
citibike201712$birth.year <- citibike201712$birth.year %>% as.integer()

colnames(citibike201701) <- colnames(citibike2016)
colnames(citibike201702) <- colnames(citibike2016)
colnames(citibike201703) <- colnames(citibike2016)
colnames(citibike201704) <- colnames(citibike2016)
colnames(citibike201705) <- colnames(citibike2016)
colnames(citibike201706) <- colnames(citibike2016)
colnames(citibike201707) <- colnames(citibike2016)
colnames(citibike201708) <- colnames(citibike2016)
colnames(citibike201709) <- colnames(citibike2016)
colnames(citibike201710) <- colnames(citibike2016)
colnames(citibike201711) <- colnames(citibike2016)
colnames(citibike201712) <- colnames(citibike2016)

citibike2017 <-
  bind_rows(
    citibike201701,
    citibike201702,
    citibike201703,
    citibike201704,
    citibike201705,
    citibike201706,
    citibike201707,
    citibike201708,
    citibike201709,
    citibike201710,
    citibike201711,
    citibike201712
  )

# The Dataset is about 5GB. If your RAM cannot support such a big file, build a local SQL is ideal.
citi <- dbConnect(SQLite(), dbname="Final.sqlite")

dbWriteTable(conn = citi, name = "citibike2013", value = citibike2013, row.names = FALSE)
dbWriteTable(conn = citi, name = "citibike2014", value = citibike2014, row.names = FALSE)
dbWriteTable(conn = citi, name = "citibike2015", value = citibike2015, row.names = FALSE)
dbWriteTable(conn = citi, name = "citibike2016", value = citibike2016, row.names = FALSE)
dbWriteTable(conn = citi, name = "citibike2017", value = citibike2017, row.names = FALSE)

dbDisconnect(citi)