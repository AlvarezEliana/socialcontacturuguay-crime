## Functions for the paper


find_design <- function(x, thebalfmla_b, thebalfmla_i, matchdist=NULL,  thepsdist, themhdist, ydist, datb,dati) {
  ## message(paste(x,collapse=" "))
	if(is.null(matchdist)){
		newdist <- (thepsdist * x[1] + themhdist * (1 - x[1])) + caliper(themhdist, x[2]) + caliper(thepsdist, x[3])
	} else  {
		newdist  <-  matchdist + caliper(themhdist, x[2]) + caliper(thepsdist, x[3])
	}
  sum_newdist <- summary(newdist)
  if (sum_newdist$total$matchable == 0) {
    return(c(x = x, d2p = NA, d2p_i = NA, maxydiff = NA, n = NA, effn = NA))
  }

  ctrl_trt_ratio <- length(sum_newdist$matchable$control)/length(sum_newdist$matchable$treatment)

  thefm <- try(fullmatch(newdist,
    data = datb, tol = .00001,
    min.controls = 1,  max.controls = Inf, mean.controls=ctrl_trt_ratio,
  ))

  if (inherits(thefm, "try-error")) {
    return(c(x = x, d2p = NA, d2p_i = NA, maxydiff = NA, n = NA, effn = NA))
  }

  datb$thefm <- factor(thefm)

  xb <- try(balanceTest(update(thebalfmla_b, . ~ . + strata(thefm)),
    data = datb,
    report = c("chisquare.test", "p.values")
  ), silent = TRUE)

  if (inherits(xb, "try-error")) {
    return(c(x = x, d2p = NA, d2p_i = NA, maxydiff = NA, n = NA, effn = NA))
  }

  ## Do individual level balance test
  ndati <-  nrow(dati)
  dati <- inner_join(dati, datb[,c("Q56","thefm")], by = "Q56")
  stopifnot(nrow(dati)==ndati)

  balfmla_i <- update(thebalfmla_i, . ~ . + strata(thefm))
  xb_i <- try(balanceTest(balfmla_i, data =dati , report = c("chisquare.test", "p.values"),
			 p.adjust.method = "holm"),silent=TRUE)
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


