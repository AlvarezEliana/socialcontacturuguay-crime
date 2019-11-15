******************
*** DATASETUP ****
******************

*database
cd "\\tsclient\C\Users\ealvarez\Dropbox\LAMRI\Proyecto Farmacias\Proyecto\Bases y análisis\Bases\Base merge madre\Base merge versión final"
use basefinal1718

***********************************
**** CREATING TYPE OF PHARMACY ****
***********************************

*Creating a variable which identifies the type of pharmacies
gen ph_type = Q56
recode ph_type 2/20=1 25=1 27=1 28=1 31=1 33=1 32=1 35=1 36=1 38=1 39=1 41=1 42=1 44/45=1 49=1 50/51=1 53/54=1 56=1 57=1 59=1 60=1 24=5 47=5 21=2 22=2 23=2 29=2 37=2 40=2 43=2 48=2 52=2 58=2 1=3 26=3 30=3 34=3 46=3 55=3 61=4 62=4 63=4 64=4

label define PHTYPE 1 "Control" 2 "Wholetime" 3 "Drop out" 4 "Newcomers" 5 "Placebo"
labe values ph_type PHTYPE

**************************
**** CREATING DUMMIES ****
**************************
 
*DUMMY PLACEBO: Pharmacies that expressed interest of selling marijuana but never did
*Scale: 1 Placebo | 0 Rest
gen placebo = ph_type
recode placebo 1/4=0 5=1

label define placebo 1 "Placebo" 0 "Other"
label values placebo placebo

*DUMMY ACTIVE: Pharmacies that sell marijuana, no matter when they started
*Scale: 1 Active | 0 Other 

gen active = ph_type
recode active 2/4=1 1=0 5=0

label define active 1 "Active" 0 "Other"
label values active active

*DUMMY CONTROL: Pharmacies that never sold marijuana
*Scale: 1 "Control" | 0 "Other"
gen control = ph_type
recode control 2/5=0 1=1 

label define control 1 "Control" 0 "Other"
label values control control

*DUMMY BASELINE: Pharmacies that sold when baseline
*Scale: 1 "Baseline" | 0 "Other"
gen baseline = ph_type
recode baseline 2 3=1 else=0

label define baseline 1 "Baseline" 0 "Other"
label values baseline baseline  

*DUMMY WHOLETIME: Pharmacies that always sold
*Scale: 1 "Wholetime" | 0 "Other"
gen wholetime = ph_type
recode wholetime 2=1 else=0

label define wholetime 1 "Wholetime" 0 "Other"
label values wholetime wholetime  

*DUMMY PARTIME: Pharmacies that either dropped out or started later
*Scale: 1 "Partime" | 0 "Other"
gen partime = ph_type
recode partime 3 4=1 else=0

label define partime 1 "Partime" 0 "Other"
label values partime partime 

*DUMMY ENDLINE: Pharmacies that sold when endline
*Scale: 1 "Endline" | 0 "Other"
gen endline = ph_type
recode endline 2 4=1 else=0

label define endline 1 "Endline" 0 "Other"
label values endline endline  

******************************************
***** CREATING SOCIAL DISORDER INDEX *****
******************************************

*Variables:
*dis4_i
*dis5_i
*dis6_i

**Scale:
*1. Nothing
*2. A Little
*3. Something
*4. A lot

** Mean (SUSTITUBILIDAD INTERMDEDIA)
egen social_dis = rowmean(dis4_i dis5_i dis6_i)

********************************************
*** ACTIVITIES IN THE NEIGHBORHOOD INDEX ***
********************************************
*Variables:
*Talk: neigh2_i
*Meetings: neigh3_i
*Education: neigh4_i
*Health: neigh5_i
*Shops: neigh6_i
*Recerational: neigh8_i

*Scale: 
*1. Once a week
*2. Once or twice a month
*3. Once or twice a year
*4. Never
*88. Don´t know 
*99. No answer

*Removing missing data
recode neigh2_i 88=. 99=.
recode neigh3_i 88=. 99=.
recode neigh4_i 88=. 99=.
recode neigh5_i 88=. 99=.
recode neigh6_i 88=. 99=.
recode neigh8_i 88=. 99=.

*Inverse scale:
recode neigh2_i 1=4 2=3 3=2 4=1 
recode neigh3_i 1=4 2=3 3=2 4=1 
recode neigh4_i 1=4 2=3 3=2 4=1 
recode neigh5_i 1=4 2=3 3=2 4=1 
recode neigh6_i 1=4 2=3 3=2 4=1 
recode neigh8_i 1=4 2=3 3=2 4=1 

*Standarization of the sacale
* replace "i" for "st" in the name, for reconizaing the standarized variables

gen neigh2_st = neigh2_i
gen neigh3_st = neigh3_i
gen neigh4_st = neigh4_i
gen neigh5_st = neigh5_i
gen neigh6_st = neigh6_i
gen neigh8_st = neigh8_i

recode neigh2_st 1=0 2=0.33 3=0.66 4=1
recode neigh3_st 1=0 2=0.33 3=0.66 4=1
recode neigh4_st 1=0 2=0.33 3=0.66 4=1
recode neigh5_st 1=0 2=0.33 3=0.66 4=1
recode neigh6_st 1=0 2=0.33 3=0.66 4=1
recode neigh8_st 1=0 2=0.33 3=0.66 4=1

*Subdimensions: 
**Services use: Health & Education -- maximun value
**Contact with neighbors: Talk & Meetings -- maximun value 

gen ser_max=max(neigh4_st, neigh5_st)
gen contact_max=max(neigh2_st, neigh3_st)

*Creating the index 
egen activities_index = rowmean (contact_max ser_max neigh6_st neigh8_st)

********************************************************************************
*Saving data
save basefinal1718, replace




