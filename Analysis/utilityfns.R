## Functions for the paper
invlogit <- function(x) {
  exp(x) / (1 + exp(x))
}

design_score <- function(maxTp, d2p_i, vrobbdiff, robbdiff, crime_p, maxrobbdiff, maxvrobbdiff, n_trt, effn) {
  ## We are going to *maximize* the score function. So, lower is better
  ## Put everything on the same scale. Using invlogit for convenience.
    bad_value <- -999999

  ## First, require no dropped treated obs
  if (n_trt != 16) {
    return(bad_value)
  }

  ## Next, calculate the score: bigger is better
  score_maxTp <-  maxTp
  score_crime_p <-  crime_p
  score_d2_p_i_p <-  d2p_i
  score_vrobbdiff <- 1-invlogit(abs(vrobbdiff))
  score_robbdiff <- 1-invlogit(abs(robbdiff))
  score_maxrobbdiff <- 1-invlogit(abs(maxrobbdiff))
  score_maxvrobbdiff <- 1-invlogit(abs(maxvrobbdiff))
  score_effn <-  invlogit(abs(effn))

  thescore <- 100 * (score_maxTp + score_d2_p_i_p  + score_crime_p + score_vrobbdiff + score_robbdiff + score_maxrobbdiff + score_maxvrobbdiff + score_effn)
  return(thescore)
}

find_design2 <- function(x, thebalfmla_b, thebalfmla_i, thepsdist, themhdist, dista, distb, datb, dati, return_full_objs = FALSE, return_score = FALSE, thelower = NULL, theupper = NULL, nestedmatching = FALSE) {
  ## return_score is TRUE when we are optimizing, and thelower and theupper
  ## are bounds on the x vector
  require(optmatch)
  require(RItools)
  require(coin)
  require(formula.tools)
  require(estimatr)

   bad_value <- -999999

  stopifnot(length(x) == 4)

  if (return_score & !is.null(thelower) & !is.null(theupper)) {
    bad_parms <- any(x < thelower) | any(x > theupper)
    if (bad_parms) {
      return(bad_value)
    }
  }

  newdist <- thepsdist + caliper(themhdist, x[1]) + caliper(thepsdist, x[2]) + caliper(dista, x[3]) + caliper(distb, x[4])

  return_obj_template <- c(
    x = x,
    d2p = NA,
    d2p_i = NA,
    maxTp = NA,
    vrobbdiff = NA,
    robbdiff = NA,
    crime_p = NA,
    maxadiff = NA,
    maxbdiff = NA,
    n = NA,
    n_trt = NA,
    n_ctrl = NA,
    effn = NA
  )

  return_score_template <- c(bad_value)

  sum_newdist <- summary(newdist)

  if (sum_newdist$total$matchable == 0 & !return_score) {
    return(return_obj_template)
  }
  if (sum_newdist$total$matchable == 0 & return_score) {
    return(return_score_template)
  }


  ctrl_trt_ratio <- length(sum_newdist$matchable$control) / length(sum_newdist$matchable$treatment)
  if (ctrl_trt_ratio < 1) {
    ctrl_trt_ratio <- 1
  }

  thefm <- try(fullmatch(newdist,
    data = datb, tol = .00001,
    min.controls = 1, max.controls = Inf, mean.controls = ctrl_trt_ratio
  ))

  if ( ( inherits(thefm, "try-error") | all(matchfailed(thefm)) )  & !return_score) {
    return(return_obj_template)
  }
  if ( ( inherits(thefm, "try-error") | all(matchfailed(thefm)) ) & return_score) {
    return(return_score_template)
  }

  datb$thefm <- factor(thefm)

  xb <- try(xBalance(thebalfmla_b,
    strata = list(thefm = ~thefm),
    data = datb[!is.na(datb$thefm), ],
    report = "all",
  ), silent = TRUE)

  if (inherits(xb, "try-error") & !return_score) {
    return(return_obj_template)
  }
  if (inherits(xb, "try-error") & return_score) {
    return(return_score_template)
  }

  ## Mean diffs in violence within set, weighted by set:
  robb_mod <- lm_robust(robb_2016_mean ~ soldvsnot17, fixed_effects = ~thefm, data = droplevels(datb[!is.na(datb$thefm), ]))
  vrobb_mod <- lm_robust(vrobb_2016_mean ~ soldvsnot17, fixed_effects = ~thefm, data = droplevels(datb[!is.na(datb$thefm), ]))

  ## Using coin::independence_test with maxT test statistic when p>>n for now until we iron out some stuff
  ## in xBalance.

  coin_fmla <- ~ soldvsnot17F | thefm
  lhs(coin_fmla) <- rhs(thebalfmla_b)

  coin_obj <- try(independence_test(coin_fmla, data = droplevels(datb[!is.na(datb$thefm), ]), teststat = "maximum"), silent = TRUE)
  if (inherits(coin_obj, "try-error") & !return_score) {
    return(return_obj_template)
  }
  if (inherits(coin_obj, "try-error") & return_score) {
    return(return_score_template)
  }

  coin_p <- try(pvalue(coin_obj))
  if (inherits(coin_p, "try-error") & !return_score) {
    return(return_obj_template)
  }
  if (inherits(coin_p, "try-error") & return_score) {
    return(return_score_template)
  }

  ## Do individual assessment:
  ndati <- nrow(dati)
  dati <- inner_join(dati, datb[, c("Q56", "thefm")], by = "Q56")
  stopifnot(nrow(dati) == ndati)

  if (nestedmatching) {
    not_missing_trts <- row.names(dati)[!is.na(dati$thefm) & dati$soldvsnot17 == 1]
    not_missing_ctrls <- row.names(dati)[!is.na(dati$thefm) & dati$soldvsnot17 == 0]

    new_psdisti <- thepsdisti[not_missing_trts, not_missing_ctrls]

    ## disti <- new_psdisti + exactMatch(soldvsnot17 ~ thefm, data = dati[!is.na(dati$thefm), ])# + caliper(new_psdisti, quantile(new_psdisti, .9))

    ## Match people across neighborhoods within set
    match_within <- function(theset) {
      ## theset is the name of a matched set
      dat_i_b <- droplevels(dati[dati$thefm == theset & !is.na(dati$thefm), ])
      trts <- row.names(dat_i_b)[dat_i_b$soldvsnot17 == 1]
      ctrls <- row.names(dat_i_b)[dat_i_b$soldvsnot17 == 0]
      match_mat <- thepsdisti[trts, ctrls]
      match_mat_w_cal <- match_mat + caliper(match_mat, quantile(match_mat, .95))
      fm_i_b <- try(pairmatch(match_mat_w_cal, data = dat_i_b, remove.unmatchable = TRUE, tol = .00001), silent = TRUE)
      if (inherits(fm_i_b, "try-error") | all(matchfailed(fm_i_b))) {
        fm_i_b <- try(pairmatch(match_mat, data = dat_i_b, remove.unmatchables = TRUE, tol = .00001), silent = TRUE)
      }
      return(fm_i_b)
    }

    ## Do the matches within each level of the fullmatch
    within_set_matches <- lapply(levels(thefm[!is.na(thefm)]), function(theset) {
      match_within(theset)
    })
    ## names(within_set_matches) <- levels(thefm[!is.na(thefm)])

    all_errors <- sapply(within_set_matches, function(x) {
      inherits(x, "try_error")
    })
    if (all(all_errors) & !return_score) {
      return(return_obj_template)
    }
    if (all(all_errors) & return_score) {
      return(return_score_template)
    }

    thefm_i <- do.call("c", within_set_matches)
    if (inherits(thefm_i, "try-error") & !return_score) {
      return(return_obj_template)
    }
    if (inherits(thefm_i, "try-error") & return_score) {
      return(return_score_template)
    }

    dati[names(thefm_i), "thefm_i"] <- factor(thefm_i)

    xb_i <- try(xBalance(thebalfmla_i,
      strata = list(thefm = ~thefm_i),
      data = droplevels(dati[!is.na(dati$thefm_i), ]), report = "all", # c("chisquare.test", "p.values")
    ), silent = TRUE)
    if (inherits(xb_i, "try-error") & !return_score) {
      return(return_obj_template)
    }
    if (inherits(xb_i, "try-error") & return_score) {
      return(return_score_template)
    }

    ## Do the key crime variable predict treatment neighborhood conditional on set?
    ## Here we can use a nice cluster robust test.

    bal_lm <- lm_robust(soldvsnot17 ~ vrobb_2016 + robb_2016 + n_sec_i, fixed_effects = ~thefm_i, clusters = Q56, se_type = "CR2", data = droplevels(dati[!is.na(dati$thefm_i), ]))
    "thef <- bal_lm$proj_fstatistic
    p_thef <- pf(thef["value"], df1 = thef["numdf"], df2 = thef["dendf"], lower.tail = FALSE)
  } else {

     
    ## Idea 1: not used
    ## Only assess balance at individual level on variables that are least
    ## balanced at pharmacy level. Since the matching is primarily at the
    ## pharmacy level, the idea is to ask which among them are better at the
    ## individual level. But we have degenerate overall p-values here. So, need
    ## to have, say, only 20 covariates.
    ## worst_covs_std_diffs <- sort(abs(xb$results[,"std.diff",]),decreasing=TRUE)
    ## worst_covs_nms0 <- names(worst_covs_std_diffs)[1:20]
    ## worst_covs_nms1 <- unique(gsub("_mean|_median|_loq","",worst_covs_nms0))
    ## worst_covs_nms2 <- worst_covs_nms1[worst_covs_nms1 %in% names(dati)]
    ## thebalfmla_i <- reformulate(worst_covs_nms2,response="soldvsnot17")

    ## Idea 2: SVD
    ## Assess balance on only the top 10 singular values of the whole n x 81
      ## column covariate matrix. This should help avoid the degeneracy problem

      newdati <- droplevels(dati[!is.na(dati$thefm), ])

   mm <-  model.matrix(update(thebalfmla_i,.~.-1),data=newdati)
   thesvd <- svd(mm,nu=10)$u
   colnames(thesvd) <- paste("V",1:10,sep="")
   newdat <- data.frame(newdati,thesvd)
   xb_i_fmla <- reformulate(colnames(thesvd),response="soldvsnot17")

    xb_i <- try(balanceTest(update(xb_i_fmla, . ~ . + strata(thefm) + cluster(Q56)),
      data = newdat), silent = TRUE)

    if (inherits(xb_i, "try-error") & !return_score) {
      return(return_obj_template)
    }
    if (inherits(xb_i, "try-error") & return_score) {
      return(return_score_template)
    }

    bal_lm <- lm_robust(soldvsnot17 ~ vrobb_2016 + robb_2016 + n_sec_i, fixed_effects = ~thefm, clusters = Q56, se_type = "CR2", 
        data = newdat)
    thef <- bal_lm$proj_fstatistic
    p_thef <- pf(thef["value"], df1 = thef["numdf"], df2 = thef["dendf"], lower.tail = FALSE)

  }

  if (!is.null(dista)) {
    maxadiff <- max(unlist(matched.distances(thefm, distance = dista)))
  } else {
    maxadiff <- NA
  }

  if (!is.null(distb)) {
    maxbdiff <- max(unlist(matched.distances(thefm, distance = distb)))
  } else {
    maxbdiff <- NA
  }

  crime_i_res <- c(robb=coef(robb_mod)[[1]],vrobb=coef(vrobb_mod)[[1]])

  if (return_full_objs) {
    return(list(
      fm_p = thefm,
      xb_p = xb,
     xb_i = xb_i,
      maxTp = coin_obj,
      crime_lm = bal_lm,
      robb_mod = robb_mod,
      vrobb_mod = vrobb_mod,
      crime_p = p_thef,
      parms = x,
      dista = dista,
      distb = distb,
      thepsdist = thepsdist,
      themhdist = themhdist,
      newdist = newdist
    ))
  }

  if (return_score) {
    design_score(
      maxTp = coin_p[1],
      d2p_i = xb_i$overall["thefm", "p.value"],
      vrobbdiff = crime_i_res[["vrobb"]],
      robbdiff = crime_i_res[["robb"]],
      crime_p = p_thef,
      maxrobbdiff = maxadiff,
      maxvrobbdiff = maxbdiff,
      n_trt = sum(datb$soldvsnot17[!is.na(datb$thefm)]),
      effn = summary(thefm)$effective.sample.size
    )
  } else {
    return(c(
      x = x,
      d2p = xb$overall["thefm", "p.value"],
      d2p_i = xb_i$overall["thefm", "p.value"],
      maxTp = coin_p[1],
      vrobbdiff = crime_i_res[["vrobb"]],
      robbdiff = crime_i_res[["robb"]],
      crime_p = p_thef,
      maxadiff = maxadiff,
      maxbdiff = maxbdiff,
      n = sum(!is.na(thefm)),
      n_trt = sum(datb$soldvsnot17[!is.na(datb$thefm)]),
      n_ctrl = sum((1 - datb$soldvsnot17[!is.na(datb$thefm)])),
      effn = summary(thefm)$effective.sample.size
    ))
  }
}
