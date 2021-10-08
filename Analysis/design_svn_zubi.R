## Follow the multilevel matching ideas from Zubizarreta, Keele, and Pimental
## focusing on sold_vs_not comparison

source("rmarkdownsetup.R")
library(matchMulti)

load(here::here("Analysis", "match_data_prep.rda"), verbose = TRUE)
load(here::here("Analysis", "initial_balance.rda"), verbose = TRUE)
load(here::here("Data", "wrkdat.rda"))

table(wdat17i$soldvsnot17, exclude = c())
table(wdat17p$soldvsnot17, exclude = c())

## the i data is individual level and p data is pharmacy or neighborhood level
dat17i <- wdat17i %>% filter(!is.na(soldvsnot17))
table(dat17i$soldvsnot17, exclude = c())

dat17p <- wdat17p %>%
  filter(!is.na(soldvsnot17)) %>%
  as.data.frame()
table(dat17p$soldvsnot17, exclude = c())
row.names(dat17p) <- dat17p$Q56

## ------------------------------------------------------------------------
tmpnms <- names(dat17p)[!names(dat17p) %in% c(designvars, "soldvsnot17", "placebovsactive", "placebovscontrol")]
matchfmla <- reformulate(tmpnms, response = "soldvsnot17")
matchfmla

stopifnot(length(setdiff(all.vars(baselineFmla), names(dat17i))) == 0)
matchfmla_i <- update(baselineFmla, . ~ . - neigh6_i.NA - vic12_i - vic12_n_i.NA)
stopifnot(length(setdiff(all.vars(matchfmla_i), names(dat17i))) == 0)

## Q56F is cluster or pharmacy indicator
## Key variables about neighborhood security n_sec_i_mean , vrobb_2016_mean , robb_2016_mean

dat17i$vrobb_2016_cat <- cut(dat17i$vrobb_2016, breaks = 4)
dat17i$robb_2016_cat <- cut(dat17i$robb_2016, breaks = 4)
dat17i$n_sec_i_cat <- cut(dat17i$n_sec_i, breaks = 3)
dat17i$dens_cat <- cut(dat17i$dens, breaks = 4)

indiv_covs0 <- all.vars(matchfmla_i)[-1]
matchfmlacovs <- all.vars(matchfmla)[-1]

## Find pharm/neigh level covs
tmp <- sapply(covsCensus, function(nm) {
  grep(nm, matchfmlacovs)[1]
})
tmp2 <- matchfmlacovs[tmp]
names(dat17p)[names(dat17p) %in% tmp2] <- gsub("_mean", "", names(dat17p)[names(dat17p) %in% tmp2])
pharm_covs <- names(tmp)

indiv_covs <- indiv_covs0[!indiv_covs0 %in% pharm_covs]

## Try mm1 and theen also try full matching on the weights mat below
mm1 <- matchMulti(
  data = dat17i, treatment = "soldvsnot17", school.id = "Q56F",
  match.students = TRUE,
  student.vars = indiv_covs,
  school.fb = list(
    c(
      "vrobb_2016_cat",
      "robb_2016_cat",
      "n_sec_i_cat"
    ),
    c(
      "vrobb_2016_cat",
      "robb_2016_cat",
      "n_sec_i_cat", "dens_cat"
    )
  ),
  student.penalty.qtile = .95,
  verbose = TRUE,
  keep.target = 16,
  tol = .01,
  save.first.stage = TRUE
)

## Not sure how this is calculating everything (or why it doesn't match up with the better tested RItools) so skipping it.
mm1_bal1 <- balanceMulti(mm1, student.cov = indiv_covs, school.cov = pharm_covs)
## mm1_bal1_out <- data.frame(varnm=rep(row.names(mm1_bal1$schools),2),
## 		  stdiff=abs(c(mm1_bal1$schools[,3], mm1_bal1$schools[,6])),
## 	     des=rep(c("Before","After"),each=nrow(mm1_bal1$schools)))
##
## g_mm1_bal1_out <- ggplot(data=mm1_bal1_out,aes(x=stdiff,y=varnm, color=des, group=varnm))+
## 	geom_point() +
## 	geom_path(arrow=arrow(length=unit(.2,"cm"))) +
## 	theme_bw()
## print(g_mm1_bal1_out)

## Which pharmacies dropped
mm1$dropped$schools.c
mm1_data <- droplevels(as.data.frame(mm1$matched))
## out <- lme(mathach ~ sector, random = ~ 1 | pair.id/school, data=match.data)
## summary(out)

##  Doesn't really drop many  people within neighborhoods
with(mm1_data, table(soldvsnot17, pair.id, exclude = c()))
with(mm1_data, table(soldvsnot17, Q56F, exclude = c()))
with(dat17i, table(soldvsnot17, Q56F, exclude = c()))

## Match at school level using the matrix produced by matchMulti
pharm_score_mat <- mm1$student.matches$schools.matrix
quantile(as.vector(pharm_score_mat[lower.tri(pharm_score_mat)]), seq(0, 1, .1))
pharm_score_cal <- quantile(as.vector(pharm_score_mat[lower.tri(pharm_score_mat)]), .95)

## Make distances  on raw  crime
tmp_robb <- dat17p$robb_2016_mean
names(tmp_robb) <- rownames(dat17p)
robbdist <- match_on(tmp_robb, z = dat17p$soldvsnot17)
## And on violent crime
tmp_vrobb <- dat17p$vrobb_2016
names(tmp_vrobb) <- rownames(dat17p)
vrobbdist <- match_on(tmp_vrobb, z = dat17p$soldvsnot17)

matchfmla_iCluster <- update(matchfmla_i, . ~ . + cluster(Q56))

matchfmla_covs_idx <- sapply(covsCensus, function(nm) {
  grep(nm, matchfmlacovs)[1]
})
newcovs <- matchfmlacovs[matchfmla_covs_idx]
names(dat17p)[names(dat17p) %in% tmp2] <- gsub("_mean", "", names(dat17p)[names(dat17p) %in% newcovs])
matchfmlacovs[tmp] <- gsub("_mean", "", matchfmlacovs[matchfmla_covs_idx])
matchfmla2 <- reformulate(matchfmlacovs, response = "soldvsnot17")

## Assess mm1
### At pharmacy level
mm1p <- mm1_data %>%
  group_by(Q56) %>%
  summarize(mm1 = unique(pair.id))
d17p <- left_join(dat17p, mm1p, by = "Q56", suffix = c("", ".y"))
row.names(d17p) <- d17p$Q56
stopifnot(all.equal(row.names(d17p), row.names(dat17p)))
xb_mm1 <- balanceTest(update(matchfmla2, . ~ . + strata(mm1)), data = d17p, report = "all", 
                      subset = !is.na(mm1), p.adjust.method = "holm")
xb_mm1$overall[, ]
xb_mm1vars <- as_tibble(xb_mm1$results[, , "mm1"], rownames = "vars")
xb_mm1vars %>%
  arrange(desc(abs(std.diff))) %>%
  head(x, n = 10)

### At individual level 
ndati1 <- nrow(dat17i)
dat17i$mm1 <- NULL
dat17i <- left_join(dat17i, d17p[, c("Q56", "mm1")], by = "Q56")
stopifnot(ndati1 == nrow(dat17i))
balfmla_i <- update(matchfmla_iCluster, . ~ . + strata(mm1))
xb_imm1 <- balanceTest(balfmla_i, data = dat17i, report = "all", p.adjust.method = "holm")
xb_imm1$overall[, ]
xb_imm1vars <- as_tibble(xb_imm1$results[, , "mm1"], rownames = "vars")
xb_imm1vars %>%
  arrange(desc(abs(std.diff))) %>%
  head(x, n = 10)
mm1_bal1_farmacias <- as.data.frame(mm1_bal1$schools)
mm1_bal1_farmacias$var <- row.names(mm1_bal1_schools)
## What vars have the worst balance?
mm1_bal1_farmacias %>%
  arrange(desc(abs(`SDiff After`))) %>%
  dplyr::select(matches("SDiff|var")) %>%
  head(n = 10)

## What do the balance statistics look like for the worst balanced variables after matching?
xb_mm1vars %>% dplyr::filter(grepl("pt_per|age_av|educ_av", vars))

## Simple full matching using  only the schools chosen by the matchMulti approach and using the scores created there
## as opposed to the pair matching done above
fm1 <- fullmatch(pharm_score_mat, data = d17p)
summary(fm1, min.controls = 0, max.controls = Inf)

d17p$fm1 <- factor(fm1)
xb1 <- balanceTest(update(matchfmla2, . ~ . + strata(fm1)), data = d17p, report = "all",
                   subset = !is.na(fm1), p.adjust.method = "holm")
xb1$overall[, ]
## What vars have the worst balance?
xb1vars <- as_tibble(xb1$results[, , "fm1"], rownames = "vars")
xb1vars %>%
  arrange(desc(abs(std.diff))) %>%
  head(x, n = 10)
ydiffs1 <- unlist(matched.distances(fm1, distance = robbdist))
summary(ydiffs1)
quantile(ydiffs1, seq(0, 1, .1))
## Do individual level balance test
ndati1 <- nrow(dat17i)
dat17i$fm1 <- NULL
dat17i <- inner_join(dat17i, d17p[, c("Q56", "fm1")], by = "Q56")
stopifnot(ndati1 == nrow(dat17i))
balfmla_i <- update(matchfmla_iCluster, . ~ . + strata(fm1))
xb_i1 <- balanceTest(balfmla_i, data = dat17i, report = "all", p.adjust.method = "holm")
xb_i1$overall[, ]
xb_i1vars <- as_tibble(xb_i1$results[, , "fm1"], rownames = "vars")
xb_i1vars %>%
  arrange(desc(abs(std.diff))) %>%
  head(x, n = 10)

## full matching allowing some neighrborhoods to drop out
fm2 <- fullmatch(pharm_score_mat + caliper(pharm_score_mat, pharm_score_cal), data = d17p, mean.controls = 2)
summary(fm2, min.controls = 0, max.controls = Inf)

d17p$fm2 <- factor(fm2)
xb2 <- balanceTest(update(matchfmla2, . ~ . + strata(fm2)), data = d17p, report = "all", subset = !is.na(fm2), p.adjust.method = "holm")
xb2$overall[, ]
## What vars have the worst balance?
xb2vars <- as_tibble(xb2$results[, , "fm2"], rownames = "vars")
xb2vars %>%
  arrange(desc(abs(std.diff))) %>%
  head(x, n = 10)
ydiffs1 <- unlist(matched.distances(fm2, distance = robbdist))
summary(ydiffs1)
quantile(ydiffs1, seq(0, 1, .1))
## Do individual level balance test
ndati1 <- nrow(dat17i)
dat17i$fm2 <- NULL
dat17i <- inner_join(dat17i, d17p[, c("Q56", "fm2")], by = "Q56")
stopifnot(ndati1 == nrow(dat17i))
balfmla_i <- update(matchfmla_iCluster, . ~ . + strata(fm2))
xb_i2 <- balanceTest(balfmla_i, data = dat17i, report = "all", p.adjust.method = "holm")
xb_i2$overall[, ]
xb_i2vars <- as_tibble(xb_i2$results[, , "fm2"], rownames = "vars")
xb_i2vars %>%
  arrange(desc(abs(std.diff))) %>%
  head(x, n = 10)

save(pharm_score_mat, file = "pharm_score_mat.rda")
