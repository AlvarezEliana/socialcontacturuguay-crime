*****************************
**********PAPER APSA*********
*****************************

*database
cd "\\tsclient\C\Users\ealvarez\Dropbox\LAMRI\Proyecto Farmacias\Proyecto\Bases y an�lisis\Bases\Base merge madre\Base merge versi�n final"
use basefinal17-18

*outcomes
*Neighbors insecurity perception: n_sec_i
*Country insecurity perception: c_sec_i
*Crime victimization: vic12
*Percieved impact on drug trafficking: dt_impact_i
*Percieved impact on public safety: ps_impact_i
*Presence of illegal selling ponits "bocas": boca1_i

*controles
*Ideology: ideol_si_i
*Education: educ_i
*Sex: sex_i
*Age: age_i

**labels & missings
recode c_sec_i 99=. 88=.
recode n_sec_i 99=. 88=.
recode dt_impact_i 99=. 88=. 4=.
recode ps_impact_i 99=. 88=. 4=.

recode ideol_si_i 99=. 88=.
recode educ_i 99=. 

label define VIC 1 "Yes" 2 "No"
label values vic12_i VIC

label define SAFE 1 "Very safe" 2 "Somewhat safe" 3 "Somewhat unsafe" 4 "Very unsafe" 88 "Don�t know" 99 "No answer"
label values c_sec_i SAFE
label values n_sec_i SAFE

label define TRAFFICK 1 "Better (is already better)" 2 "Same" 3 "Worst (is already worst)" 4 "[Don�t read] Fight Drug Trafficking has nothing to do with this law" 88 "Don�t know" 99 "No answer"
label values dt_impact_i TRAFFICK

label define PSAFE 1 "Better (is already better)" 2 "Same" 3 "Worst (is already worst)" 4 "[Don�t read] Public safety has nothing to do with this law" 88 "Don�t know" 99 "No answer"
label values ps_impact_i PSAFE

*gen boca_4
gen boca_rec= boca1_i
recode boca_rec 88=2 99=.

label define sip 1 "Yes" 2 "No"
label values boca_rec sip

tab boca_rec 

**groups comparability (Table 1)
ttest n_sec_i if ronda==2017, by (treat)
ttest c_sec_i if ronda==2017, by (treat)
ttest vic12_i if ronda==2017, by (treat)
ttest ps_impact_i if ronda==2017, by (treat)
ttest dt_impact_i if ronda==2017, by (treat)
ttest boca_rec if ronda==2017, by (treat)
ttest ideol_si_i if ronda==2017, by (treat)
ttest educ_i if ronda==2017, by (treat)
ttest sex_i if ronda==2017, by (treat)
ttest age_i if ronda==2017, by (treat)

**diff in diff (Table 2)
*controls: 
*n_sec_i: sexo, edad, educaci�n
*c_sec_i: sexo, edad, educaci�n
*vic12: sexo, edad, educaci�n
*dt_impact_i: ideolog�a, educaci�n
*ps_impact_i: ideolog�a, educaci�n
*boca_rec: sexo, edad, educaci�n

gen time = ronda
recode time 2017=0 2018=1
gen did = time*treat

reg n_sec_i sex_i age_i educ_i time treat did, r
reg c_sec_i sex_i age_i educ_i time treat did, r
reg vic12_i sex_i age_i educ_i time treat did, r
reg dt_impact_i ideol_si_i educ_i time treat did, r
reg ps_impact_i ideol_si_i educ_i time treat did, r
reg boca_rec sex_i age_i educ_i time treat did, r
