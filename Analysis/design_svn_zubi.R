## Follow the multilevel matching ideas from Zubizarreta, Keele, and Pimental


source("rmarkdownsetup.R")

load(here::here("Analysis","match_data_prep.rda"),verbose=TRUE)
load(here::here("Analysis","initial_balance.rda"),verbose=TRUE)
load(here::here("Data", "wrkdat.rda"))

table(wdat17i$soldvsnot17,exclude=c())
table(wdat17p$soldvsnot17,exclude=c())

## the i data is individual level and p data is pharmacy or neighborhood level
dat17i <- wdat17i %>% filter(!is.na(soldvsnot17))
table(dat17i$soldvsnot17,exclude=c())

dat17p <- wdat17p %>% filter(!is.na(soldvsnot17))
table(dat17p$soldvsnot17,exclude=c())
row.names(dat17p) <- dat17p$Q56

## ------------------------------------------------------------------------
tmpnms <- names(dat17p)[!names(dat17p) %in% designvars]
matchfmla <- reformulate(tmpnms, response = "soldvsnot17")
matchfmla

setdiff(all.vars(baselineFmla),names(dat17i))
matchfmla_i <-  update(baselineFmla,.~.-neigh6_i.NA-vic12_i-vic12_n_i.NA)
setdiff(all.vars(matchfmla_i),names(dat17i))

## Q56F is cluster or pharmacy indicator
## Key variables about neighborhood security n_sec_i_mean + vrobb_2016_mean + robb_2016_mean

dat17i$vrobb_2016_cat <- cut(dat17i$vrobb_2016,breaks = 4)
dat17i$robb_2016_cat <- cut(dat17i$robb_2016,breaks = 4)
dat17i$n_sec_i_cat<- cut(dat17i$n_sec_i,breaks = 3)

library(matchMulti)

indiv_covs <- all.vars(matchfmla_i)[-1]
matchfmlacovs <- all.vars(matchfmla)[-1]

## Find pharm/neigh level covs
tmp <- sapply(covsCensus,function(nm){ grep(nm,matchfmlacovs)[1] })
tmp2 <- matchfmlacovs[tmp]
names(dat17p)[names(dat17p) %in% tmp2] <- gsub("_mean","",names(dat17p)[names(dat17p) %in% tmp2])
pharm_covs <- names(tmp)



mm1 <- matchMulti(data=dat17i, treatment="soldvsnot17", school.id="Q56F",
		  match.students=TRUE,
		  student.vars=indiv_covs,
		  school.fb = list(c('vrobb_2016_cat',
				   'robb_2016_cat',
				   'n_sec_i_cat')),
		  verbose=TRUE,
		  keep.target=16,
		  save.first.stage=TRUE)

mm1_bal1 <- balanceMulti(mm1, student.cov = indiv_covs, school.cov=pharm_covs)
mm1_bal1_out <- data.frame(varnm=rep(row.names(mm1_bal1$schools),2),
		  stdiff=abs(c(mm1_bal1$schools[,3], mm1_bal1$schools[,6])),
	     des=rep(c("Before","After"),each=nrow(mm1_bal1$schools)))

g_mm1_bal1_out <- ggplot(data=mm1_bal1_out,aes(x=stdiff,y=varnm, color=des, group=varnm))+
	geom_point() +
	geom_path(arrow=arrow(length=unit(.2,"cm"))) +
	theme_bw()
print(g_mm1_bal1_out)

## Which schools dropped
mm1$dropped$schools.c
mm1_data <- as.data.frame(mm1$matched)
##out <- lme(mathach ~ sector, random = ~ 1 | pair.id/school, data=match.data)
##summary(out)

with(mm1_data,table(soldvsnot17,pair.id,exclude=c()))

## Match at school level using the matrix produced by matchMulti
pharm_score_mat <- mm1$student.matches$schools.matrix

fm1 <- fullmatch(pharm_score_mat,data = dat17p)
pm1 <- pairmatch(pharm_score_mat)


