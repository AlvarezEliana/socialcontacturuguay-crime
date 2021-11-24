
library(here)
library(arm)
library(optmatch)
library(tidyverse)
library(RItools)
library(coin)
library(formula.tools)
library(estimatr)
source(here("Analysis","rmarkdownsetup.R"))

## ------------------------------------------------------------------------
load(here::here("Analysis", "match_data_prep.rda"), verbose = TRUE)
load(here::here("Analysis", "initial_balance.rda"), verbose = TRUE)


## ------------------------------------------------------------------------
## The individual level data
table(wdat17i$soldvsnot17, exclude = c())
## The pharmarcy level data
table(wdat17p$soldvsnot17, exclude = c())

## Exclude people in the neighborhoods for pharmacies which have NA for
## soldvsnot17
dat17i <- wdat17i %>% filter(!is.na(soldvsnot17))
table(dat17i$soldvsnot17, exclude = c())

dat17p <- wdat17p %>% filter(!is.na(soldvsnot17)) %>% as.data.frame()
table(dat17p$soldvsnot17, exclude = c())
row.names(dat17p) <- dat17p$Q56 ## needed to add this for later use even those tidyverse doesn't like it

## ------------------------------------------------------------------------
tmpnms <- names(dat17p)[!names(dat17p) %in% designvars]
matchfmla <- reformulate(tmpnms, response = "soldvsnot17")
matchfmla
### Propensity score distances
## Since we worry about separation in logistic regressions, we try a couple of
## bayesian approaches following Gelman et al on Separation problems and Weakly
## informative priors in logistic regression. We worry about this in particular
## with such a small data set and so many covariates. We also want to be
## matching on both pharmacy and individual level characteristics, so we try to
## create scores that capture some possible systematic differences between
## pharmacies here.

## library(brms)
## library(cmdstanr)
## library(posterior)
## library(bayesplot)
## color_scheme_set("brightblue")
## library(rstanarm)
##library(future)
## plan(multicore)
## options(mc.cores=4)

setdiff(all.vars(baselineFmla), names(dat17i))
## Remove variables that don't vary that slipped through match_data_prep.Rmd.
## Remove census variables: we will match at pharmacy level on them, too

#matchfmla_i <- update(baselineFmla, . ~ . - neigh6_i.NA - vic12_i - vic12_n_i.NA
#    - age_av      - educ_av     - h_owners    - rent_per    - pop         - dens
#    - ubn_no      - ubn_one     - ubn_two  - ubn_more    - cat_value   - fa_per
#    - pn_per      - pc_per      - pi_per      - ap_per      - peri_per    -
#        pt_per  - vrobb_2016  - robb_2016 - sec_mea1_p + sec_mea2_p + sec_mea3_p + sec_mea4_p)

matchfmla_i <- update(baselineFmla, . ~ . - neigh6_i.NA - vic12_i - vic12_n_i.NA)

## Make sure all variables in the formula are actually in the dataset
setdiff(all.vars(matchfmla_i), names(dat17i))
## Add a random effect to capture neighborhood average differences.
#refmla <- update(matchfmla_i,.~(1|Q56)+.)

##standardize <- function(x){
##  (x - mean(x))/sd(x)
##}

### dat17i_stand <- dat17i %>% mutate(across(where(is.numeric),.fns=standardize))
### refmla <- update(matchfmla_i,.~(1|Q56)+activities_index + age_i + c_sec_i + crime_t_i + sex_i + vic12_n_i)
### ## Remove census variables
### ## no reason to believe the the covariates effect a24 exposure radically differently across regions or psus
### brm_mod <- brm(refmla,data=dat17i_stand,iter=2000,chains=6,cores=6,future=FALSE,normalize=TRUE,
###     prior=c(set_prior("student_t(3,0,2.5)",class="Intercept"),set_prior("student_t(3, 0, 2.5)", class = "b")),  control=list(max_treedepth=15,adapt_delta=.98),backend="cmdstanr")
### np <- nuts_params(brm_mod)
### pairs(brm_mod,variable=variables(brm_mod)[1:12],np=np)
###
### ps_brm_mod <- fitted(brm_mod,scale="linear")
### save(brm_mod,file="brm_mod.rda")


## glm1 <- glm(matchfmla,data=dat17p,family=binomial())
bglm1 <- bayesglm(matchfmla, data = dat17p, family = binomial(link = "logit"))
glm1 <- glm(matchfmla, data = dat17p, family = binomial(link = "logit"))
bglm1mat <- summary(bglm1)$coef
stopifnot(min(abs(bglm1mat)) > 0)
dat17p$pscore1 <- predict(bglm1)

## TODO: Stop matching at individual level. Change this in utility functions
## too.
## Now fit the model at the individual level and predict to the cluster level
bglm2 <- bayesglm(update(matchfmla_i, . ~ . + Q56F), data = droplevels(dat17i), family = binomial(link = "logit"))

dat17pSimp <- dat17i %>%
  dplyr::select(unique(c("Q56", all.vars(formula(bglm2))))) %>%
  group_by(Q56) %>%
  summarise_if(is.numeric, list(mean = mean))
names(dat17pSimp) <- gsub("_mean", "", names(dat17pSimp))
dat17pSimp$Q56F <- factor(dat17pSimp$Q56)

setdiff(all.vars(formula(bglm2)), names(dat17pSimp))
stopifnot(setdiff(names(dat17pSimp), all.vars(formula(bglm2))) == "Q56")

changevars <- grep(".NA", names(dat17pSimp), value = TRUE)
dat17pSimp <- dat17pSimp %>% mutate_at(changevars, as.logical)

## Make a propensity score using the individual level data set to the
## neighborhood level means
dat17p$pscore2 <- predict(bglm2, newdata = dat17pSimp)

## Make an individual level propensity score distance matrix
psdist_i <- match_on(bglm2,data=dat17i)

## Combine the two bayesian propensity scores in a mahalanobis distance
## psdist <- match_on(soldvsnot17~pscore1+pscore2,data=dat17p,method="rank_mahalanobis")
psdist <- match_on(soldvsnot17 ~ pscore1 + pscore2, data = dat17p) ## want  pscore2 to play a bigger role, method = "rank_mahalanobis")
## Example few entries from the distance matrix
as.matrix(psdist)[1:10, 1:5]

### Absolute differences on a scalar: neighborhood insecurity perception
## summary(dat17p$robb_2016_mean)
## robbdist <- match_on(soldvsnot17 ~ robb_2016_mean, data = dat17p, method = "rank_mahalanobis")
## robbdist[1:5, 1:5]
summary(dat17p$n_sec_i_mean)
crimedist <- match_on(soldvsnot17 ~ n_sec_i_mean + vrobb_2016_mean + robb_2016_mean, data = dat17p, method = "rank_mahalanobis")
crimedist[1:5, 1:5]

## Make distances  on raw  crime
## the _mean is not meaningful. robb and vrobb are neighborhood level variables.
tmp_robb <- dat17p$robb_2016_mean
names(tmp_robb) <- rownames(dat17p)
robbdist <- match_on(tmp_robb, z = dat17p$soldvsnot17)

tmp_vrobb <- dat17p$vrobb_2016_mean
names(tmp_vrobb) <- rownames(dat17p)
vrobbdist <- match_on(tmp_vrobb, z = dat17p$soldvsnot17)

## What kinds of differences do we see in general on these variables?
### So, if we insist on perfect matches we will throw away nearly all of our
### data. It seems to me that we can restrict the matches
## > quantile(robbdist,seq(0,1,.1))
##   0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
##    0    5   11   17   21   29   37   50   66   87  201
## > quantile(vrobbdist,seq(0,1,.1))
##   0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
##    0    0    1    2    3    5    8   10   14   18   25

quantile(robbdist,seq(0,1,.1))
quantile(vrobbdist,seq(0,1,.1))



### Match on covariate distances alone, trying to soldvsnot17 each covariate equally
### using the rank based mahalanobis distance: See Rosenbaum 2010, Chap 8
mhdist <- match_on(matchfmla, data = dat17p, method = "rank_mahalanobis")
mhdist[1:5, 1:5]


## ------------------------------------------------------------------------
quantile(as.vector(psdist), seq(.9, 1, .01))
quantile(as.vector(mhdist), seq(.9, 1, .01))
quantile(as.vector(crimedist), seq(.9, 1, .01))

pcal <- quantile(as.vector(psdist), .98)
mcal <- quantile(as.vector(mhdist), .98)
crimecal <- quantile(as.vector(crimedist), .98)

## The maximum distances for use in penalties
maxmh <- max(mhdist)
maxps <- max(psdist)
maxyd <- max(crimedist)


## ----penalizedistmats----------------------------------------------------
mhdistPen <- mhdist + (psdist > pcal) * maxmh + (mhdist > mcal) * maxmh + (crimedist > crimecal) * 100 * maxmh ## Penalty like Rosenbaum
psdistPen <- psdist + (psdist > pcal) * maxps + (mhdist > mcal) * maxps + (crimedist > crimecal) * 100 * maxps ## Penalty like Rosenbaum
crimedistPen <- crimedist + (psdist > pcal) * maxyd + (mhdist > mcal) * maxyd ## Penalty like Rosenbaum

summary(mhdist)
## summary(mhdistPen)

## Rescale for combining later

summary(as.vector(psdistPen))
summary(as.vector(mhdistPen))
psdistPen <- psdistPen * (1 / (max(psdistPen) / max(mhdistPen)))
summary(as.vector(psdistPen))

psdist2 <- psdist * (1 / (max(psdist) / max(mhdist)))
summary(as.vector(psdist2))
summary(as.vector(mhdist))

matchfmla_iCluster <- update(matchfmla_i, . ~ . + cluster(Q56))


## ------------------------------------------------------------------------
source(here::here("Analysis", "utilityfns.R"))


## ----setupdesignfinding--------------------------------------------------
# Test the function.

search_space <- as.matrix(expand.grid(
  mhcal = quantile(mhdist, seq(.5, 1, length.out = 10)),
  pscal = quantile(psdist2, seq(.5, 1, length.out = 10)),
  robbcal = quantile(robbdist, seq(.2, 1, length.out = 10)),
  vrobbcal = quantile(vrobbdist, seq(.2, 1, length.out = 10))
))

dat17p$soldvsnot17F <- factor(dat17p$soldvsnot17)

starting_par <- apply(search_space,2,mean)
lower_par <-apply(search_space,2,min)
upper_par <-apply(search_space,2,max)

find_design2(
  #x = search_space[which.max(rowSums(search_space)),],
  #x = search_space[1,],
  #  x=search_space[samp_search_space[2],],
      x = upper_par,
  thebalfmla_b = matchfmla,
  thebalfmla_i = matchfmla_i,
  themhdist = mhdist,
  thepsdist = psdist2,
  dista = robbdist,
  distb = vrobbdist,
  datb = dat17p,
  dati = dat17i
)

find_design2(
  #x = search_space[which.max(rowSums(search_space)),],
  x = upper_par,
  thebalfmla_b = matchfmla,
  thebalfmla_i = matchfmla_i,
  themhdist = mhdist,
  thepsdist = psdist2,
  dista = robbdist,
  distb = vrobbdist,
  datb = dat17p,
  dati = dat17i,
  return_score=TRUE,
  thelower=lower_par,
  theupper=upper_par
)

## Try an optimization approach first:
library(GA)

opt_res <- ga(type = "real-valued", fitness = find_design2, lower = lower_par, upper = upper_par,
  thebalfmla_b = matchfmla,
  thebalfmla_i = matchfmla_i,
  themhdist = mhdist,
  thepsdist = psdist2,
  dista = robbdist,
  distb = vrobbdist,
  datb = dat17p,
  dati = dat17i,
  return_score=TRUE,
  thelower=lower_par,
  theupper=upper_par,
  suggestions = upper_par,
  popSize = 100, maxiter = 1000, run = 10,parallel=16,seed=12345)


find_design2(
  #x = search_space[which.max(rowSums(search_space)),],
  x = opt_res@solution,
  thebalfmla_b = matchfmla,
  thebalfmla_i = matchfmla_i,
  themhdist = mhdist,
  thepsdist = psdist2,
  dista = robbdist,
  distb = vrobbdist,
  datb = dat17p,
  dati = dat17i,
  return_score=FALSE,
  thelower=lower_par,
  theupper=upper_par
)

save(opt_res,file=here("Analysis","opt_res.rda"))


## ----matchsearch, cache=FALSE--------------------------------------------
##ncores <- parallel::detectCores() - 1
ncores <- round(parallel::detectCores()/2)

options(future.globals.maxSize = +Inf)
## This should work on all platforms.
library(future.apply)
plan(multicore, workers = ncores)

samp_search_space <- sample(1:nrow(search_space),10)

system.time(
  results <- future_mapply(
    function(x1, x2, x3, x4) {
      parms <- c(x1,x2,x3,x4)
      res <- try(find_design2(
          x = c(x1, x2, x3, x4),
          thebalfmla_b = matchfmla,
          thebalfmla_i = matchfmla_i,
          themhdist = mhdist,
          thepsdist = psdist2,
          dista = robbdist,
          distb = vrobbdist,
          datb = dat17p,
          dati = dat17i
          ),silent=TRUE)
      if(inherits(res,"try-error")){
        return(parms)
      } else {
        return(res)
      }
    },
    x1 = search_space[samp_search_space, 1],
    x2 = search_space[samp_search_space, 2],
    x3 = search_space[samp_search_space, 3],
    x4 = search_space[samp_search_space, 4] ,future.seed=TRUE
  )
)

plan(sequential)

## len_res <- sapply(results,length)
## table(len_res)

save(results, search_space,  matchfmla, matchfmla_i, mhdist, psdist2, psdist_i,
    robbdist, vrobbdist, datb = dat17p, dati = dat17i, file =
        here::here("Analysis", "design_soldvsnot_search_res2.rda"))

