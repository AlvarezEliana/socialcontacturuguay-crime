*Creating Figure 4: Perceived impact of marijuana regulation on drug trafficking by round - Treatment andControl group

histogram dt_impact_i if treat==1, discrete percent by(ronda) ylabel (0 "0" 50 "50") ymtick(0 (10) 50) xlabel (1 "Better" 2 "Same" 3 "Worst")
histogram dt_impact_i if treat==0, discrete percent by(ronda) ylabel (0 "0" 50 "50") ymtick(0 (10) 50) xlabel (1 "Better" 2 "Same" 3 "Worst")
