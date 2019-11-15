---
title: Pre-Analysis Plan for 'The Impact of Marijuana Sale at Pharmacies on Crime Victimization and Insecurity Perceptions.'
author: |
 - Jake Bowers
 - Rosario Queirolo
 - Eliana Álvarez
 - Lorena Repetto
date: 23 August 2019
---


This is a brief pre-analysis plan for the paper to be presented at APSA  'The Impact of Marijuana Sale at Pharmacies on Crime Victimization and Insecurity Perceptions.' We aim to assess the effect of living near a pharmacy that sells marijuana on the attitudes and perceptions of the neighbors after exposure to selling pharmacy for one year.

# Study Design Overview

## Design 

We take advantage of the fact that our team could survey neighborhoods before local pharmacies in Uruguay began to sell marijuana.

### Pharmacy categorization: The intervention

Like all field studies, this one involves units changing status through the period. Here are the categories of pharmacies:

  - PLACEBO: 1 is placebo pharmacies (N=2)
  - CONTROL: 1 is control pharmacies (N=42)
  - ACTIVE: 1 is pharmacies that sell marijuana between the two rounds of the survey (June 2017 to August 2018), no matter if they dropped out or stated later (N=20)
  - WHOLETIME: 1 is pharmacies that sold the entire time of the study (N=10)
  - PARTIME: 1 is pharmacies that sold at some point, either because they dropped out or started later (N=10)
  - BASELINE: 1 is pharmacies that sold at baseline (N=16)
  - ENDLINE: 1 is pharmacies that sold at second round (N=14)

The `ph_type` variable identifies the different situations of the pharmacies regarding the marijuana sale.

Categories are: 1 "Control" 2 "Wholetime" (pharmacies that sold during the entire research) 3 "Drop out" (pharmacies that drop out the sale 4 "Newcomers" (begin to sell after we run the first round of the survey) 5 "Placebo"

### Baseline and Outcome Data Collection via Face-to-Face Survey

A baseline survey  of 10 neighbors of each of 60 pharmacies was collected during June 2017, one month before pharmacies were allowed to sell marijuana. At that time the list of pharmacies that were going to sell was not public, as a result, neighbors did not know if the pharmacy close by was going to be a marijuana selling pharmacy. Our field team visited and surveyed 10 neighbors in each of the 60 pharmacies: 16 which had registered to sell marijuana, 42 that had not registered to sell marijuana, and 2 pharmacies that initially were willing to join the sale but never actually did it (placebos). 

### Stratification for adjustment 

We create a stratified research design to compare the survey responses of the neighbors of the selling with the non-selling pharmacies. We search for a stratification using the `fullmatch` function in the `optmatch` package for R (code as of commit cb8de833805a47d569c5fe8b98bd9b7304d296a7, August 23, 2019). 

We used the following covariates used in the stratification:

```r 
> load("Analysis/match_data_prep.rda")
> covs3
 [1] "activities_index" "age_i"            "c_sec_i"          "crime_t_i"       
 [5] "dis1_i"           "dis2_i"           "dis3_i"           "dis4_i"          
 [9] "dis5_i"           "dis6_i"           "dt_impact_i"      "educ_i"          
[13] "ffuse_i"          "ideol_si_i"       "if_impact_i"      "law1_i"          
[17] "law2_i"           "mvd_int"          "n_sec_i"          "neigh2_i"        
[21] "neigh3_i"         "neigh4_i"         "neigh5_i"         "neigh6_i"        
[25] "neigh7_i"         "neigh8_i"         "op1_m_i"          "op2_m_i"         
[29] "op3_m_i"          "op4_m_i"          "op5_m_i"          "op6_m_i"         
[33] "ph_impact_i"      "prev_lt_i"        "ps_impact_i"      "ps1718"          
[37] "pstigma1_i"       "pstigma2_i"       "pstigma3_i"       "pstigma4_i"      
[41] "pstigma5_i"       "pstigma6_i"       "pstigma7_i"       "pstigma8_i"      
[45] "rp_m1_i"          "rp_m2_i"          "rp_m3_i"          "sec_mea1_p"      
[49] "sec_mea2_p"       "sec_mea3_p"       "sec_mea4_p"       "sex_i"           
[53] "social_dis"       "st1718"           "stigma1_i"        "stigma2_i"       
[57] "stigma3_i"        "stigma4_i"        "stigma6_i"        "vic12_i"         
[61] "vic12_n_i"        "age_av"           "educ_av"          "h_owners"        
[65] "rent_per"         "pop"              "dens"             "ubn_no"          
[69] "ubn_one"          "ubn_two"          "ubn_more"         "cat_value"       
[73] "fa_per"           "pn_per"           "pc_per"           "pi_per"          
[77] "ap_per"           "peri_per"         "pt_per"           "vrobb_2016"      
[81] "robb_2016"       
> 
```

The matched design that we chose was the following:

```r
> summary(fm2,min.controls=0,max.controls=Inf)
Structure of matched sets:
1:1 1:3 1:6 0:1 
 11   3   2  10 
Effective Sample Size:  18.9 
(equivalent number of matched pairs).

```

### Assessment of the stratified design

The chosen research design design compares favorably with the standard for unconfounded comparison --- in this case, it would be a block-randomized experiment.

At the pharmacy level with 89 terms (the 80 covariates  above aggregated using means, medians, and 20th ptiles, keeping only those aggregates that were not constant when collapsing  from 600 rows to 60).

```r
> xb2$overall[,]
    chisquare df p.value
fm2   0.08382  2 0.95896
--    7.02220  3 0.07119
```

At the individual level with  80 terms with cluster adjustment (following Hansen and Bowers 2008):

```r
> xb_i2$overall[,]
    chisquare df p.value
fm2     2.155  3 0.54094
--      7.121  3 0.06814
```

## Outcomes

We also have a social disorder index: `social_dis` made from  "dis4_i" "dis5_i" "dis6_i" Higher values of the index indicates higher disorder.

Finally, we have created the "activities_index" that show how much people are involved in their neighborhood´s daily life.  This index has a 0-1 scale, 1 meaning greater frequency of activities in the neighborhood and 0 meaning none.  "neigh2_i" "neigh3_i" "neigh4_i" "neigh5_i" "neigh6_i" "neigh7_i" "neigh8_i".

Our outcome variables are the following:

```r
outcomes <- c("n_sec_i","c_sec_i","vic12","dt_impact_i","ps_impact_i","boca1_i","social_dis","activities_index")
```


# Hypotheses

Our  two main confirmatory hypotheses are:
  -   H1: *The sale of marijuana at pharmacies will not have significant
      effects on crime victimization of pharmacies' neighbors*
  
  -   H2: *The sale of marijuana at pharmacies will not have significant
      effects on neighbors' insecurity perceptions*
 
 In adittion we have five exploratory hypotheses:
 
  -   H3: *The sale of marijuana at pharmacies will push "bocas" outside the
      neighborhood, and by doing that, neighbors' insecurity perception
      might be reduced*
  
  -   H4: *The sale of marijuana at pharmacies will increase social disorder
      in the neighborhood, that would lead to higher levels of public
      insecurity perception among neighbors*
  
  -   H5: *The sale of marijuana at pharmacies will diminish citizens'
      insertion in the neighborhood*
  
  -   H6: *The sale of marijuana at pharmacies will not change citizens'
      evaluations about the impact of marijuana legalization on public
      security*
  
  -   H7: *The sale of marijuana at pharmacies will change citizens'
      evaluations about the impact of marijuana legalization on drug
      trafficking*

# Measures and Index Construction

Include the description of all measures, either with the table or text. 

The social disorder index was built using three variables: presence of young people loitering, presence of drunk or stoned people in the streets, and presence of people arguing with each other. Each of these variables have the following values: 1 (Not at all), 2 (Little), 3 (Somewhat), and 4 (Very much). In order to construct the index we assume intermediate substitutability among variables and assign the mean value \citep{goertz2006social}.

Citizens' Insertion on their Neighborhood is an index constructed with eight variables grouped in four dimensions. The dimensions are: use of services (education and health) in the neighborhood, contact among neighbors (chat and/or meet for collective action activities), perform recreational activities in the neighborhood, and shopping in the neighborhood. We assume intermediate substitutability among dimensions and assign the mean value. The use of services dimension has two indicators: use of educational services (kindergarten, school,high school) in the neighborhood and use of health services (doctor, hospital) in the neighborhood. We assume total substitutability among the two indicators and assign the maximum value  \citep{goertz2006social}.Contact among neighbors dimension also has two indicators: talk with your neighbors and meets/ organizes with the neighbors for any improvement activity for the neighborhood. We also assume total substitutability among the two indicators and assign the maximum value. Perform recreational activities in the neighborhood is measured by the question: do you perform any recreational activities in the neighborhood? Finally, shopping in the neighborhood is measured by the question: do you purchase in warehouses or stores in the neighborhood?

We document our index construction in the github repository in `datasetup.do` (date stamped using the commit and date mentioned above).

Table 1. Outcomes variables description

Variable	|	Question	|	Scale	|	Minimum	|	Maximun|
--------	|	--------	|	-----	|	:--------:|	:-------:|
Country insecurity perception | In general, in your country, do you feel very safe somewhat unsafe or very unsafe? |1. Very safe; 2.Somewhat safe; 3. Somewhat unsafe; 4. Very unsafe; 88 - Don\'t know; 99 - No answer |	1	|	4
Neighborhood insecurity perception | And in the neighborhood where you live, do you feel very safe, somewhat safe, somewhat unsafe or very unsafe? |1. Very safe; 2.Somewhat safe; 3. Somewhat unsafe; 4. Very unsafe; 88 - Don\'t know; 99 - No answer |	1	|	4
Crime victimization in the last 12 months | Now, changing the subject, have you been a victim of any type of crime in the past 12 months? That is, have you been a victim of robbery, burglary, assault, fraud, blackmail, extortion, violent threats or any other type of crime in the past 12 months? | 1. Yes; 2. No; 88 - Don\'t know | 1	|	2
Perceived impact on public security |	Now regarding to public safety, because of this law, do you think the country will be better, will remain the same or will be worst?	|	1. Yes;	2. No; 88 - Don\'t know;	99 - No answer| 1	|	3
Perceived impact on drug-trafficking | And regarging the fight against drug-trafficking, do you think the country will be better, will remain the same or will be worst? |	1. Yes;	2. No; 88 - Don\'t know;	99 - No answer| 1	|	2
Reported existence of bocas	|	Based on what you know or hear, there is a any boca in this area? | 1. Yes;	2. No; 88 - Don\'t know;	99 - No answer| 1	|	2
Social Disorder Index	|	Index | 1. Nothing; 2. A little; 3. Something; 4. A lot | 4 | 1
Presence of young people loitering | Observation | 1. Nothing; 2. A little; 3. Something; 4. A lot | 4 | 1
Presence of drunk or stoned people in the streets | Observation | 1. Nothing; 2. A little; 3. Something; 4. A lot | 4 | 1
Presence of people arguing with each other | Observation | 1. Nothing; 2. A little; 3. Something; 4. A lot | 1 | 4
Citizens insertion in the neighborhood | Index | - | 0 | 1
Talk with your neighbors| Finally, thinking about the activities you do in this neighborhood, please tell me how many times you: talk with your neighbors |1. Once a week; 2. Once or twice a week; 3. Once or twice a year; 4. Never; 88. Don\'t know; 99. No answer; |4 | 1
Chat and/or meet for collective action activities | Finally, thinking about the activities you do in this neighborhood, please tell me how many times you: chat and/or meet for collective action activities|1. Once a week; 2. Once or twice a week; 3. Once or twice a year; 4. Never; 88. Don\'t know; 99. No answer; | 4 | 1
Use educational services (kindergarten, school, high school) | Finally, thinking about the activities you do in this neighborhood, please tell me how many times you: use educational services (kindergarten, school, high school)|1. Once a week; 2. Once or twice a week; 3. Once or twice a year; 4. Never; 88. Don\'t know; 99. No answer; | 4 | 1
Use health services (doctor, hospital) | Finally, thinking about the activities you do in this neighborhood, please tell me how many times you: use health services (doctor, hospital) |1. Once a week; 2. Once or twice a week; 3. Once or twice a year; 4. Never; 88. Don\'t know; 99. No answer; | 4 | 1
Buy in local stores | Finally, thinking about the activities you do in this neighborhood, please tell me how many times you: buy in local stores|1. Once a week; 2. Once or twice a week; 3. Once or twice a year; 4. Never; 88. Don\'t know; 99. No answer; | 4 | 1
Perform recreational activities | Finally, thinking about the activities you do in this neighborhood, please tell me how many times you: perform recreational activities|1. Once a week; 2. Once or twice a week; 3. Once or twice a year; 4. Never; 88. Don\'t know; 99. No answer; | 4 | 1


# Estimation Procedure and Inference Criteria

We will report both unadjusted and covariance adjusted results --- focusing on appropriately weighted differences of means as estimators for the average treatment effect. We will analyze the study as-if-randomized --- using Neyman's standard errors and Fisher's hypothesis testing approaches.


## Approaches to estimation

Our covariance adjustment will follow Lin's method (Lin 2013). And we will estimate the ATE (i.e. using block-size weights) but report confidence intervals and standard errors based on precision weighting because of our diversity in block  size (see Hansen and Bowers 2008 among other places showing the precision gains from this approach).

## Approaches to Testing

We will use both direct permutation  based approaches to testing as well as asymptotic approaches since we have only 16 "treated" clusters.

We will also use both differences of means as  test statistics as well as rank-based test statistics given our small sample.

## Approach to multiple testing

We have 8 outcomes and one comparison (Selling versus Not Selling pharmacy). If
we were going to make a decision about the effect of the pharmacies if *any one
of the 8 comparisons yielded a p<.05*  then  we would make a false positive
error not 1 in 20 tests, but $1 − (1 − .05)^8 \approx 1/3$  or almost 7/20.

However,  if each analysis stands alone in terms of our  decision making : the
assessment of perceptions of insecurity can be null  or significant
independently of the assessment of reports of experiences with crime, then we
do not need to adjust the tests.

In the case of this project, we do not make any adjustment because decisions
regarding the effect of the pharmacy stand alone.

## Covariates for covariance adjustment

We are matching to balance covariates measured at both individual level as well as at the neighborhood level (including both aggregates of the individual survey level measures and administrative data). Yet, we do not make person-to-person sets across neighborhood strata. For this reason, and also following Rubin and Thomas, 1991, we adjust our estimates of average effects and our tests following Rosenbaum 2002 using the following short list of background covariates.

```r
covariates <- c("ideol_si_i","educ_i","sex_i","age_i")
```

# Missing  Data  and Extreme Data Points

Since our data are from a survey we do not anticipate extreme data points in the endline --- our  work with the baseline  data supports this idea that we should not worry about overly influential points 


# Sensitivity Analysis

We will execute both a Rosenbaum style and a Hosman, Hansen and Holland 2010 style sensitivity analysis.

# References  
