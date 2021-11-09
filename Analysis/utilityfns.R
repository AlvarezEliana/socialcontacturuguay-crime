## Functions for the paper


find_design <- function(x, thebalfmla_b, thebalfmla_i, matchdist = NULL, thepsdist, themhdist, ydist, datb, dati) {
  ## message(paste(x,collapse=" "))
  if (is.null(matchdist)) {
    newdist <- (thepsdist * x[1] + themhdist * (1 - x[1])) + caliper(themhdist, x[2]) + caliper(thepsdist, x[3])
  } else {
    newdist <- matchdist + caliper(themhdist, x[2]) + caliper(thepsdist, x[3])
  }
  sum_newdist <- summary(newdist)
  if (sum_newdist$total$matchable == 0) {
    return(c(x = x, d2p = NA, d2p_i = NA, maxydiff = NA, n = NA, effn = NA))
  }

  ctrl_trt_ratio <- length(sum_newdist$matchable$control) / length(sum_newdist$matchable$treatment)

  thefm <- try(fullmatch(newdist,
    data = datb, tol = .00001,
    min.controls = 1, max.controls = Inf, mean.controls = ctrl_trt_ratio,
  ))

  if (inherits(thefm, "try-error")) {
    return(c(x = x, d2p = NA, d2p_i = NA, maxydiff = NA, n = NA, effn = NA))
  }

  datb$thefm <- factor(thefm)

  xb <- try(xBalance(thebalfmla_b,
    strata = list(thefm = ~thefm),
    data = datb,
    report = c("chisquare.test", "p.values")
  ), silent = TRUE)

  if (inherits(xb, "try-error")) {
    return(c(x = x, d2p = NA, d2p_i = NA, maxydiff = NA, n = NA, effn = NA))
  }

  ## Do individual level balance test
  ndati <- nrow(dati)
  dati <- inner_join(dati, datb[, c("Q56", "thefm")], by = "Q56")
  stopifnot(nrow(dati) == ndati)

  xb_i <- try(xBalance(thebalfmla_i,
    strata = list(thefm = ~thefm),
    data = dati, report = c("chisquare.test", "p.values")
  ), silent = TRUE)
  if (inherits(xb_i, "try-error")) {
    return(c(x = x, d2p = NA, d2p_i = NA, maxydiff = NA, n = NA, effn = NA))
  }

  if (!is.null(ydist)) {
    maxydiff <- max(unlist(matched.distances(thefm, distance = ydist)))
  } else {
    maxydiff <- NA
  }

  return(c(
    x = x,
    d2p = xb$overall["thefm", "p.value"],
    d2p_i = xb_i$overall["thefm", "p.value"],
    maxydiff = maxydiff,
    n = sum(!is.na(thefm)),
    effn = summary(thefm)$effective.sample.size
  ))
}



find_design2 <- function(x, thebalfmla_b, thebalfmla_i, matchdist = NULL, thepsdist, thepsdisti, themhdist, dista, distb, datb, dati, return_full_obs = FALSE) {
  require(optmatch)
  require(RItools)
  require(coin)
  require(formula.tools)
  ## message(paste(x,collapse=" "))
  ## If no matchdist then match on propensity score
  if (is.null(matchdist)) {
    newdist <- (thepsdist * x[1] + themhdist * (1 - x[1])) + caliper(themhdist, x[2]) + caliper(thepsdist, x[3]) + caliper(dista, x[4]) + caliper(distb, x[5])
  } else {
    newdist <- matchdist + caliper(themhdist, x[2]) + caliper(thepsdist, x[3]) + caliper(dista, x[4]) + caliper(distb, x[5])
  }

return_obj_template <-   c( x = x, d2p = NA, maxTp=NA, d2p_i = NA, vrobbdiff = NA,
    robbdiff = NA, vrobbp = NA, robbp = NA, crime_p = NA, maxadiff = NA,
    maxbdiff = NA, n = NA, effn = NA, effn_i = NA)


  sum_newdist <- summary(newdist)
  if (sum_newdist$total$matchable == 0) {
      return(return_obj_template)
  }

  ctrl_trt_ratio <- length(sum_newdist$matchable$control) / length(sum_newdist$matchable$treatment)
  if(ctrl_trt_ratio < 1){ ctrl_trt_ratio <- 1}

  thefm <- try(fullmatch(newdist,
    data = datb, tol = .00001,
    min.controls = 1, max.controls = Inf, mean.controls = ctrl_trt_ratio,
  ))

  if (inherits(thefm, "try-error")) {
            return(return_obj_template)
  }

  datb$thefm <- factor(thefm)

  xb <- try(xBalance(thebalfmla_b,
    strata = list(thefm = ~thefm),
    data = datb,
    report = c("chisquare.test", "p.values")
  ), silent = TRUE)


  if (inherits(xb, "try-error")) {
            return(return_obj_template)
  }

## Using coin::independence_test when p>>n for now until we iron out some stuff
  ## in xBalance.

  coin_fmla <- ~ soldvsnot17F | thefm
  lhs(coin_fmla) <- rhs(thebalfmla_b)

  coin_obj <- try(independence_test(coin_fmla,data=droplevels(datb[!is.na(datb$thefm),]),teststat="maximum"),silent=TRUE)

  if (inherits(coin_obj, "try-error")) {
            return(return_obj_template)
  }

  ## Do individual level matching:
  ndati <- nrow(dati)
  dati <- inner_join(dati, datb[, c("Q56", "thefm")], by = "Q56")
  stopifnot(nrow(dati) == ndati)

  not_missing_trts <- row.names(dati)[!is.na(dati$thefm) & dati$soldvsnot17 == 1]
  not_missing_ctrls <- row.names(dati)[!is.na(dati$thefm) & dati$soldvsnot17 == 0]

  new_psdisti <- thepsdisti[not_missing_trts, not_missing_ctrls]

  disti <- new_psdisti + exactMatch(soldvsnot17 ~ thefm, data = dati[!is.na(dati$thefm), ]) + caliper(new_psdisti, quantile(new_psdisti, .9))

  thefm_i <- try(fullmatch(disti, data = dati, min.controls = 1, max.controls = Inf, tol = .00001), silent = TRUE)
  if (inherits(thefm_i, "try-error")) {
            return(return_obj_template)
  }
  dati$thefm_i <- factor(thefm_i)

  xb_i <- try(xBalance(thebalfmla_i,
    strata = list(thefm = ~thefm_i),
    data = dati, report = "all", # c("chisquare.test", "p.values")
  ), silent = TRUE)

  if (inherits(xb_i, "try-error")) {
            return(return_obj_template)
  }

  ## Do the key crime variable predict treatment neighborhood conditional on set?
  ## Here we can use a nice cluster robust test.

  bal_lm <- lm_robust(soldvsnot17 ~ vrobb_2016 + robb_2016 + n_sec_i, fixed_effects = ~thefm_i, clusters = Q56, se_type = "CR2", data = droplevels(dati[!is.na(dati$thefm_i), ]))

  thef <- bal_lm$proj_fstatistic
  p_thef <- pf(thef["value"], df1 = thef["numdf"], df2 = thef["dendf"], lower.tail = FALSE)

  ## if (!is.null(ydist)) {
  ##     maxydiff <- max(unlist(matched.distances(thefm, distance = ydist)))
  ## } else {
  ##     maxydiff <- NA
  ## }

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

  crime_i_res <- xb_i$results[c("vrobb_2016", "robb_2016"), c("adj.diff", "p"), ]

  if (return_full_obs) {
    return(list(
      fm_p = thefm,
      fm_i = thefm_i,
      xb_p = xb,
      maxTp = coin_obj,
      xb_i = xb_i,
      parms = x,
      dista = dista,
      distb = distb,
      thepsdist = thepsdist,
      themhdist = themdhist,
      disti = disti,
      new_psdisti = new_psdisti,
      psdisti = psdisti,
      newdist = newdist
    ))
  } else {
    return(c(
      x = x,
      d2p = xb$overall["thefm", "p.value"],
      d2p_i = xb_i$overall["thefm", "p.value"],
      maxTp = pvalue(coin_obj)[1],
      vrobbdiff = crime_i_res["vrobb_2016", "adj.diff"],
      robbdiff = crime_i_res["robb_2016", "adj.diff"],
      vrobbp = crime_i_res["vrobb_2016", "p"], ## this will be too low, not adjusted for clusters, but higher better for purpose of finding designs
      robbp = crime_i_res["robb_2016", "p"],
      crime_p = p_thef,
      maxadiff = maxadiff,
      maxbdiff = maxbdiff,
      n = sum(!is.na(thefm)),
      effn = summary(thefm)$effective.sample.size,
      effn_i = summary(thefm_i)$effective.sample.size
    ))
  }
}
