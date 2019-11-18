*****************************
**** socialcontact & crime
*****************************

**Update: 11.18.2019

*database
cd "\\tsclient\C\Users\ealvarez\Dropbox\LAMRI\Proyecto Farmacias\Proyecto\Bases y análisis\Bases\Base merge madre\Base merge versión final"
use basefinal1718

**Missings
recode c_sec_i 99=. 88=.
recode n_sec_i 99=. 88=.
recode dt_impact_i 99=. 88=. 4=.
recode ps_impact_i 99=. 88=. 4=.

recode ideol_si_i 99=. 88=.
recode educ_i 99=. 

*gen boca_4
gen boca_rec= boca1_i
recode boca_rec 88=2 99=.

**Inverting variables
***Higher values mean positve effect of the policy  
recode c_sec_i 1=4 2=3 3=2 4=1 
recode n_sec_i 1=4 2=3 3=2 4=1 
recode ps_impact_i 3=1 2=2 1=3
recode dt_impact_i 3=1 2=2 1=3
recode vic12_i 1=0 2=1 
recode boca_rec 1=0 2=1 

**Social_dis 
**Delete the orginal formulation of the index, reverse the variables and generate it again 
drop social_dis

recode dis4_i 1=4 2=3 3=2 4=1 
recode dis5_i 1=4 2=3 3=2 4=1 
recode dis6_i 1=4 2=3 3=2 4=1 

egen social_dis = rowmean(dis4_i dis5_i dis6_i)

*Labels 
label define YES 0 "Yes" 1 "No"
label values vic12_i YES
label values boca_rec YES

label define SAFE 4 "Very safe" 3 "Somewhat safe" 2 "Somewhat unsafe" 1 "Very unsafe" 
label values c_sec_i SAFE
label values n_sec_i SAFE

label define IMPACT 3 "Better (is already better)" 2 "Same" 1 "Worst (is already worst)"
label values dt_impact_i IMPACT
label values ps_impact_i IMPACT

label define DISORDER 4 "Nothing" 1 "A lot"
label value social_dis DISORDER

