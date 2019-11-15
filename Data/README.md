
# Notes on Data Set Creation


The stata file,  `basefinal1718.dta` contains the final  data used in our analysis. 
basefinal.1718 is created in this order of files:
 1. mergescript.r
 2. PS and ST creation.do
 3. datasetup.do
NOTE: PS and ST (stigmatization indexes) are not primary outcomes for the APSA paper but they are in basefinal.dta

## Design Variables

  - PLACEBO: 1 is placebo pharmacies (N=2)
  - CONTROL: 1 is control pharmacies (N=42)
  - ACTIVE: 1 is pharmacies that sell marijuana, no matter if they dropped out or stated later (N=20)
  - WHOLETIME: 1 is pharmacies that sold the entire time of the study (N=10)
  - PARTIME: 1 is pharmacies that sold at some point, either because they dropped out or started later (N=10)
  - BASELINE: 1 is pharmacies that sold at baseline (N=16)
  - ENDLINE: 1 is pharmacies that sold at second round (N=14)

New variable in the updated basefinal1718.dta file.
`ph_type` identifies the different situations of the pharmacies regarding the marijuana sale.

Categories are:
1 "Control"
2 "Wholetime" (pharmacies that sold during the entire research)
3 "Drop out" (pharmacies that drop out the sale
4 "Newcomers"
5 "Placebo"

Do file named "Creating dummies" show this change.

## Outcomes

These are the main outcomes measured at both baseline and endline.

Also, we´ve created the "Social Disorder Index" (social_dis)

This index uses variables dis4_i dis5_i and dis_6.

Higher values of the index indicates higher disorder.

"Social_dis" si a control variable that should replace dis4_i, dis5_i and dis6_i

Do file named "Social disorder index" show this change.

Finally, we have created the "activities_index" that show how much people are involved in their neighborhood´s daily life (better name will come up!)

This index has a 0-1 scale, 1 meaning greater frequency of activities in the neighborhood and 0 meaning none.

This is also a control variable that shoul replace neigh2_i neigh3_i neigh4_i neigh5_i neigh8_i neigh8_i

NOTE: neigh7_i which is the question about if people goes to the pharmacy is not part of the index. We believe that this variable has an especial value so we should use it as a control separtly from the rest.

Do file named "Activities in the heighborhood index" show these changes.

