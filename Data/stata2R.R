library(readstata13)
library(here)

#finaldat <- read.dta13(here("Data", "Final_database.dta"))
finaldat <- read.dta13(here("Data", "wd_basefinal1718.dta"))
save(finaldat,file="finaldat.rda")


