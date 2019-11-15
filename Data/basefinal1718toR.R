library(readstata13)
library(here)

basefinal1718 <- read.dta13(here("Data", "basefinal1718.dta"))
save(basefinal1718,file="basefinal1718.rda")


