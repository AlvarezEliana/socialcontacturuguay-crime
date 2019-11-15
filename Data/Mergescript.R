## Combine the neighborhood survey data with the farmacy level data.

library(here)
library(readstata13)
library(tidyverse)

basevecinos <- read.dta13(here("Data","base vecinos con factores PUBLICS y STIGM.dta"), convert.factors = F)

basefarmacias <- read.dta13(here("Data","base farmacias.dta"), convert.factors = F)

which(names(basefarmacias)=="Q56")

#Acá estoy creando una observación para la farmacia 24 que no teníamos datos, así la base queda con 60 unidades
basefarmacias[60,6]<- 24

dat <- inner_join(x=basevecinos, y=basefarmacias, by="Q56", suffix = c("i","p"))

## Adding a test of the merge
stopifnot(nrow(dat)==nrow(basevecinos))

head(dat[,128:135])
## View(dat)
tail(basefarmacias[,1:10])

write.csv(dat,file=here("Data","merged_ip.csv"))
save(dat,file=here("Data","merged_ip.rda")) ## This will be easier to use with R
