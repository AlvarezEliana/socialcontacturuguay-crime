
source("rmarkdownsetup.R")

## ------------------------------------------------------------------------
load(here::here("Analysis","match_data_prep.rda"),verbose=TRUE)
load(here::here("Analysis","initial_balance.rda"),verbose=TRUE)


## ------------------------------------------------------------------------
table(wdat17i$soldvsnot17,exclude=c())
table(wdat17p$soldvsnot17,exclude=c())

dat17i <- wdat17i %>% filter(!is.na(soldvsnot17))
table(dat17i$soldvsnot17,exclude=c())

dat17p <- wdat17p %>% filter(!is.na(soldvsnot17))
table(dat17p$soldvsnot17,exclude=c())


## ------------------------------------------------------------------------
tmpnms <- names(dat17p)[!names(dat17p) %in% designvars]
matchfmla <- reformulate(tmpnms, response = "soldvsnot17")
matchfmla
### Propensity score distances
## Since we worry about separation in logistic regressions, we try a couple of
## bayesian approaches following Gelman et al on Separation problems and Weakly
## informative priors in logistic regression. We worry about this in particular
## with such a small data set and so many covariates
## glm1 <- glm(matchfmla,data=dat17p,family=binomial())
bglm1 <- bayesglm(matchfmla, data = dat17p, family = binomial(link = "logit"))
glm1 <- glm(matchfmla, data = dat17p, family = binomial(link = "logit"))
bglm1mat <- summary(bglm1)$coef
stopifnot(min(abs(bglm1mat))>0)
dat17p$pscore1 <- predict(bglm1)

setdiff(all.vars(baselineFmla),names(dat17i))
matchfmla_i <-  update(baselineFmla,.~.-neigh6_i.NA-vic12_i-vic12_n_i.NA)
setdiff(all.vars(matchfmla_i),names(dat17i))

## Now fit the model at the individual level and predict to the cluster level
bglm2 <- bayesglm(update(matchfmla_i,.~.+Q56F),data=droplevels(dat17i), family = binomial(link = "logit"))

dat17pSimp <- dat17i %>% dplyr::select(unique(c("Q56", all.vars(formula(bglm2))))) %>%
  group_by(Q56) %>% summarise_if(is.numeric,funs( mean = mean))
names(dat17pSimp) <-  gsub("_mean","",names(dat17pSimp))
dat17pSimp$Q56F <- factor(dat17pSimp$Q56)

setdiff(all.vars(formula(bglm2)),names(dat17pSimp))
setdiff(names(dat17pSimp),all.vars(formula(bglm2)))

changevars <- grep(".NA",names(dat17pSimp),value=TRUE)
dat17pSimp <- dat17pSimp %>%  mutate_at(changevars,as.logical)

dat17p$pscore2 <- predict(bglm2,newdata = dat17pSimp )

## Also perhaps just predict at individual level and average to farmacia

## Combine the two bayesian propensity scores in a mahalanobis distance
## psdist <- match_on(soldvsnot17~pscore1+pscore2,data=dat17p,method="rank_mahalanobis")
psdist <- match_on(soldvsnot17 ~ pscore1+pscore2, data = dat17p) ## want  pscore2 to play a bigger role, method = "rank_mahalanobis")
## Example few entries from the distance matrix
as.matrix(psdist)[1:10,1:5]

### Absolute differences on a scalar: neighborhood insecurity perception
##summary(dat17p$robb_2016_mean)
##robbdist <- match_on(soldvsnot17 ~ robb_2016_mean, data = dat17p, method = "rank_mahalanobis")
##robbdist[1:5, 1:5]
summary(dat17p$n_sec_i_mean)
crimedist <- match_on(soldvsnot17 ~ n_sec_i_mean + vrobb_2016_mean + robb_2016_mean , data = dat17p, method="rank_mahalanobis")
crimedist[1:5, 1:5]

## Make distances  on raw  crime
tmp_robb <- dat17p$robb_2016_mean
names(tmp_robb) <- rownames(dat17p)
robbdist <- match_on(tmp_robb, z = dat17p$soldvsnot17)

tmp_vrobb <- dat17p$vrobb_2016_mean
names(tmp_vrobb) <- rownames(dat17p)
vrobbdist <- match_on(tmp_vrobb, z = dat17p$soldvsnot17)


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
mhdistPen <- mhdist + (psdist > pcal) * maxmh + (mhdist > mcal) * maxmh + (crimedist > crimecal)*100*maxmh ## Penalty like Rosenbaum
psdistPen <- psdist + (psdist > pcal) * maxps + (mhdist > mcal) * maxps + (crimedist >  crimecal)*100*maxps ## Penalty like Rosenbaum
crimedistPen <- crimedist + (psdist > pcal) * maxyd + (mhdist > mcal) * maxyd  ## Penalty like Rosenbaum

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

matchfmla_iCluster <-  update(matchfmla_i,.~.+cluster(Q56))


## ------------------------------------------------------------------------
source(here::here("Analysis","utilityfns.R"))


## ----setupdesignfinding--------------------------------------------------
# Test the function.

find_design(
  x = c(0, max(mhdist), max(psdist2)),
  thebalfmla_b = matchfmla,
  thebalfmla_i=matchfmla_iCluster,
  themhdist = mhdist,
  thepsdist = psdist2,
  ydist = crimedist,
  datb = dat17p,
  dati = dat17i
)


find_design(
  x = c(0, max(mhdist), max(psdist2)),
  thebalfmla_b = matchfmla,
  thebalfmla_i=matchfmla_iCluster,
  matchdist=crimedist,
  themhdist = mhdist,
  thepsdist = psdist2,
  ydist = robbdist,
  datb = dat17p,
  dati = dat17i
)


search_space <- as.matrix(expand.grid(
  mix = 1, ##seq(0, 1, length.out = 10),
  mhcal = quantile(mhdist, seq(.5, 1, length.out = 20)),
  ## pscal = quantile(psdistPen, seq(.5, 1, length.out = 40))
  pscal = quantile(psdist2, seq(.5, 1, length.out = 20)),
  robbcal= quantile(robbdist,seq(.5,1,length.out=5)),
  vrobbcal= quantile(vrobbdist,seq(.5,1,length.out=5))
))

## ensure that it runs on a small subset of the search_space
##search_space <- search_space[sample(1:nrow(search_space),10),]

find_design(x=search_space[1,],
	    thebalfmla_b = matchfmla,
  thebalfmla_i=matchfmla_iCluster,
  matchdist=crimedistPen,
  themhdist = mhdist,
  thepsdist = psdist2,
  ydist = crimedist,
  datb = dat17p,
  dati = dat17i)


find_design2(x=search_space[1,],
	    thebalfmla_b = matchfmla,
  thebalfmla_i=matchfmla_iCluster,
  matchdist=crimedist,
  themhdist = mhdist,
  thepsdist = psdist2,
  ydist = robbdist,
  dista = robbdist,
  distb = vrobbdist,
  datb = dat17p,
  dati = dat17i)


set.seed(12345)
find_design(x=search_space[sample(1:nrow(search_space),1),],
	      thebalfmla_b = matchfmla,
  thebalfmla_i=matchfmla_iCluster,
  themhdist = mhdistPen,
  thepsdist = psdistPen,
  ydist = crimedist,
  datb = dat17p,
  dati = dat17i)



## ----matchsearch, cache=FALSE--------------------------------------------
ncores <- parallel::detectCores()

options(future.globals.maxSize = +Inf)
## This should work on all platforms.
library(future.apply)
plan(multiprocess, workers = ncores)
system.time(
	    results <- future_mapply(
				     function(x1, x2, x3, x4, x5) {
					     find_design2(x = c(x1, x2, x3, x4, x5),
							  thebalfmla_b = matchfmla,
							  thebalfmla_i = matchfmla_iCluster,
							  matchdist = crimedist,
							  themhdist = mhdist,
							  thepsdist = psdist2,
							  ydist = robbdist,
							  dista = robbdist,
							  distb = vrobbdist,
							  datb = dat17p,
							  dati = dat17i)
				     },
				     x1 = search_space[, 1],
				     x2 = search_space[, 2],
				     x3 = search_space[, 3],
				     x4 = search_space[, 4],
				     x5 = search_space[, 5]
	    )
)
plan(sequential)
save(results, search_space, file = here::here("Analysis", "design_soldvsnot_search_res.rda"))

