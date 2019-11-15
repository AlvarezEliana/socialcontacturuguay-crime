##  Recode and impute and clean

library(here)
library(optmatch)
library(tidyverse)

load(here::here("Data", "finaldat.rda"))
## Merge the new indices (Created in file Analysis/Factorial analisis_Modelo definitivo.do) onto the old baseline data

## Notes on data coding
## **88 and 99**
## All "No sabe" were recode as 88
## All "No responde" were recode as 99
##
## Exception: Vote (Q46) This one has been tricky because it´s in a string format (i don´t know why) and i´m having trouble to recode it. Working on it!
##
## **In the database** Always remember:
## - variables ending in "i" means that they were asked in the Neighbors surey. "i" as individuals
## - variables ending in "p" means that they were asked in the Employees survey. "p" as pharmacies
##

## names(dat)

stigmavars <- grep("stigma1_i|stigma2_i|stigma3_i|stigma4_i|stigma4_i|stigma6_i|stigma7_i|stigma8_i", names(finaldat), value = TRUE)

outcomes <- unique(c(stigmavars, "ps1718", "st1718", "law1_i", "law2_i", "n_sec_i", "c_sec_i", "pstigma1_i", "pstigma2_i", "pstigma3_i", "pstigma4_i", "pstigma5_i", "pstigma6_i", "pstigma7_i", "pstigma8_i", "stigma1_i", "stigma2_i", "stigma3_i", "stigma4_i", "stigma4_i", "stigma6_i", "ph_impact_i", "ps_impact_i", "dt_impact_i", "if_impact_i", "rp_m1_i", "rp_m3_i", "rp_m2_i", "vic12_i", "vic12_n_i", "crime_t_i", "neigh2_i", "neigh3_i", "neigh4_i", "neigh5_i", "neigh6_i", "neigh7_i", "neigh8_i", "dis1_i", "dis2_i", "dis3_i", "dis4_i", "dis5_i", "dis6_i", "op1_m_i", "op2_m_i", "op3_m_i", "op4_m_i", "op5_m_i", "op6_m_i", "sec_mea1_p", "sec_mea2_p", "sec_mea3_p", "sec_mea4_p", "social_dis", "activities_index"))

covs <- c("vic12_n_i", "sex_i", "age_i", "ideol_si_i", "educ_i", "prev_lt_i", "ffuse_i", "mvd_int")

covsCensus <- c("age_av", "educ_av", "h_owners", "rent_per", "pop", "dens", "ubn_no", "ubn_one", "ubn_two", "ubn_more", "cat_value", "fa_per", "pn_per", "pc_per", "pi_per", "ap_per", "peri_per", "pt_per", "vrobb_2016", "robb_2016")

## Found some recoded NA variables
nanms <- sapply(unique(c(covs, outcomes)), function(x) {
  foundnms <- grep(paste(x, ".NA", sep = ""), names(finaldat), value = TRUE)
  if (length(foundnms) > 0) {
    return(foundnms)
  }
})
nanms <- simplify(nanms[!sapply(nanms, is.null)])
covs2 <- sort(unique(c(covs, outcomes, nanms)))
names(covs2) <- NULL
stopifnot(length(unique(covs2)) == length(covs2))

allcovs <- unique(c(covs2, covsCensus))

## Categories are:
## 1 "Control" (pharmacies that never sold)
## 2 "Wholetime" (pharmacies that sold during the entire research)
## 3 "Drop out" (pharmacies that registered and started to sell but drop out the sale)
## 4 "Newcomers" (pharmacies that started to sell after baseline --- no baseline data for their neighbors)
## 5 "Placebo" (pharmacies that registered but never sold)


# > phtab <- table(wrkdat$ph_type,wrkdat$Q56,exclude=c())
# > apply(phtab,1,function(x){ sum(x!=0) })
# 1  2  3  4  5
# 42 10  6  4  2

designvars <- c("id","Q56", "treat", "ronda", "ph_type")

allvars <- unique(c(outcomes, allcovs, designvars))

## Boca1_i was  not in the initial set of variables and it is too late to  redo  the matching
allvars2  <- unique(c(allvars,"boca1_i"))

## Stop if there is a mismatch of variables to the column names in the dataset
stopifnot(all(allvars2 %in% names(finaldat)))

whichchar <- sapply(finaldat[, allvars2], class) == "character"

## The following variables should be character (see Issue ##31)
shouldbechar <- c(
  "address_i", "corner_p", "date_i", "date_p", "law3_text_i", "law3_text_p",
  "obser_i", "obser_p", "prob_nei2text_p", "sell_prob1text_p", "stock9_p",
  "street_p", "users2_p", "add_ph_p", "corner_p2", "neighbor_p"
)

## But none are used in the matching and analysis. And  thus are excluded from the working data file.
stopifnot(any(!(shouldbechar %in% allvars2)))

shouldbenum <- whichchar[!(names(whichchar) %in% shouldbechar)]

finaldat2 <- finaldat %>%
  select(allvars2) %>%
  mutate_if(is.character, as.numeric)

stopifnot(nrow(finaldat2)==nrow(finaldat))

replace_NA_0 <- function(x) {
  ifelse(x %in% c(88, 99), NA, x)
}

## Make all 88 and 99 responses into NA

dat2 <- finaldat2 %>% mutate_at(vars(allvars2), replace_NA_0)

## Fill all NA with the column mean and make a new variable recording TRUE if the original variable was missing and FALSE otherwise.

dat3 <- optmatch::fill.NAs(dat2[, allvars2], all.covs = TRUE)

## Check the recoding
stopifnot(sum(is.na(dat2$robb_2016)) == sum(dat3$robb_2016.NA))
stopifnot(nrow(finaldat2)==nrow(dat2))

wrkdat <- dat3

## An error miscoded the treatment status of two neighborhoods. Fixing theem here following Issue #34.

wrkdat$treat[wrkdat$Q56 == 32] <- 0 ## should be control
wrkdat$treat[wrkdat$Q56 == 37] <- 1 ## should be treatment

treattab <- with(wrkdat, table(treat, Q56, exclude = c()))
testassignment <- apply(treattab, 2, function(x) {
  sum(x == 0)
})
stopifnot(all(testassignment == 1))
stopifnot(nrow(finaldat2)==nrow(wrkdat))

save(wrkdat, allvars2, allvars,  allcovs, designvars, outcomes, covs, covsCensus, file = here::here("Data", "wrkdat.rda"))
