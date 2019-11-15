*Creating Figure 5: Differences in differences estimation for impact of drug trafficking

collapse (mean) dt_impact_i, by (ronda treat)
reshape wide dt_impact_i, i(ronda) j(treat)
label var dt_impact_i0 Control
label var dt_impact_i1 Treatment
label var ronda Year
twoway line dt_impact_i0 dt_impact_i1 ronda, sort
