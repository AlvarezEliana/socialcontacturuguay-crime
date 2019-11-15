********************************************************************************
*****************************ANÁLISIS FACTORIAL*********************************
********************************************************************************

* Bibliografía: 
* CFA in Stata 12 (DROPBOX)
* EFA en Stata 12 (DROPBOX)
* Matsunaga_FactorA(DROPBOX)

*Hago "set more off" para no tener que poner MORE cada vez
set more off

* Preparando conceptualmente los Factores

*PUBLIC STIGMA, incluye: 
* Q31_1m_17
* Q31_2m_17
* Q31_3m_17
* Q31_4m_17
* Q31_5m_17
* Q31_6m_17
* Q31_7m_17
* Q31_8m_17

* STIGMATIZATION, incluye
* Q32_1m_17
* Q32_2m_17
* Q32_3m_17
* Q32_4m_17
* Q32_5m_17
* Q32_6m_17

***************
******EFA******
***************

* PRIMERO: Hago un "Reliability Analysis" (CRONBACH ALPHA). 
//Incluyo todas las variables del factorial seguido de ", item"

*alpha VARIABLES, item

* LUEGO: Tiro el factor
*factor VARIABLES

* Tengo que mirar el valor Eigenvalues y el valor Uniqueness
* Stata identifies factors that have Eigenvalues greater than 0.0. Of course 
// this criterion is often not particularly useful and it is recommended that 
// you specify more useful criteria. In order to use Eigenvalues > 1.0, 
// you can specify this after the comma

*factor VARIABLES, mineigen (1)

*Entonces: 
* PUBLIC STIGMA EFA
alpha Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_5m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17, item
factor Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_5m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17
factor Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_5m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17, mineigen (1)

* STIGMATIZATION EFA
alpha Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17, item
factor Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17
factor Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17,mineigen (1)

* JUNTOS
factor Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_5m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17 Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17, mineigen (1)
rotate
* q31_5rec no sirve como variable a ningun factor. La removemos.

* SIN Q31_5rec
factor Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17 Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17, mineigen (1)
rotate, varimax
estat kmo
* CON ROTATE: removes la idea de que los factores son ortogonales y removes el orden de los factores. 
//Los resultados ganan en claridad. Lo simplificas para entender mejor 

alpha Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17, item

*EFA Conclusión: Existen dos factores bien definidos. 

*************************
***********CFA***********
*************************


*******MÉTODO MLMV Y EXCLUSIÓN DE Q31_5*******
sem (PS_MLMV -> Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17)(ST_MLMV ->  Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17), latent (PS_MLMV ST_MLMV) method(mlmv) stand
estat gof, stats(all)
estat mindices

* Corregimos por las covariaciones altas (.9 y .8)
sem (PS_MLMV -> Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17)(ST_MLMV ->  Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17), cov(e.Q31_1m_17*e.Q31_2m_17) cov(e.Q31_6m_17*e.Q31_7m_17) latent (PS_MLMV ST_MLMV) method(mlmv) stand
estat gof, stats(all)
estat mindices

*cov(e.Q31_1m*e.Q31_2m) cov(e.Q31_6m*e.Q31_7m)

predict PS_MLMV ST_MLMV , latent(PS_MLMV ST_MLMV)

*******MÉTODO Y EXCLUSIÓN DE Q31_5*******
sem (PS_MISS-> Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17)(ST_MISS -> Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17), latent (PS_MISS ST_MISS) stand
estat gof, stats(all)
estat mindices

* Corregimos por las covariaciones altas (.9 y .8)
sem (PS_MISS-> Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17)(ST_MISS -> Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17), cov(e.Q31_1m_17*e.Q31_2m_17) cov(e.Q31_6m_17*e.Q31_7m_17) latent (PS_MISS ST_MISS)
estat gof, stats(all)
estat mindices

*cov(e.Q31_1m*e.Q31_2m) cov(e.Q31_6m*e.Q31_7m)
predict PS_MISS ST_MISS, latent(PS_MISS ST_MISS)

____________________________________________________
*****
*EFA*
*****

global ps Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17
tabstat $ps, stat(mean) save
tabstatmat mean
tabstat $ps, stat(sd) save
tabstatmat stdev

polychoric $ps
display r(sum_w)
global N = r(sum_w)
matrix R = r(R)
factormat R, n($N) mineigen(1) blanks(.4) sds(stdev) means(mean) pcf
rotate, oblique promax blanks(.40)

**PROBAR SIN SACAR Q31_5 

***MISSINGS***

*con Q31_5
sem (PS_MISS-> Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_5m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17)(ST_MISS -> Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17), latent (PS_MISS ST_MISS) stand
estat gof, stats(all)
estat mindices

*cov más alta: cov(e.Q31_6m_17,e.Q31_7m_17)
sem (PS_MISS-> Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_5m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17)(ST_MISS -> Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17), cov(e.Q31_6m_17*e.Q31_7m_17) latent (PS_MISS ST_MISS)
estat gof, stats(all)
estat mindices

*segunda cov más alta: 
sem (PS_MISS-> Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_5m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17)(ST_MISS -> Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17), cov(e.Q31_1m_17*e.Q31_2m_17) cov(e.Q31_6m_17*e.Q31_7m_17) latent (PS_MISS ST_MISS)
estat gof, stats(all)
estat mindices

*sin Q31_5
sem (PS_MISS-> Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17)(ST_MISS -> Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17), latent (PS_MISS ST_MISS) stand
estat gof, stats(all)
estat mindices

*sin Q31_5 y una cov
sem (PS_MISS-> Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17)(ST_MISS -> Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17), cov(e.Q31_6m_17*e.Q31_7m_17) latent (PS_MISS ST_MISS)
estat gof, stats(all)
estat mindices

*sin Q31_5 y las cov más altas
sem (PS_MISS-> Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17)(ST_MISS -> Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17), cov(e.Q31_1m_17*e.Q31_2m_17) cov(e.Q31_6m_17*e.Q31_7m_17) latent (PS_MISS ST_MISS)
estat gof, stats(all)
estat mindices

predict PS_MLMV ST_MLMV , latent(PS_MLMV ST_MLMV)


***MLVM***
*con Q31_5
sem (PS_MISS-> Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_5m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17)(ST_MISS -> Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17), latent (PS_MISS ST_MISS) method(mlmv) stand
estat gof, stats(all)
estat mindices

*cov más alta: cov(e.Q31_6m_17,e.Q31_7m_17)
sem (PS_MISS-> Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_5m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17)(ST_MISS -> Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17), cov(e.Q31_6m_17*e.Q31_7m_17) latent (PS_MISS ST_MISS) method(mlmv) stand
estat gof, stats(all)
estat mindices

*segunda cov más alta: 
sem (PS_MISS-> Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_5m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17)(ST_MISS -> Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17), cov(e.Q31_1m_17*e.Q31_2m_17) cov(e.Q31_6m_17*e.Q31_7m_17) latent (PS_MISS ST_MISS) method(mlmv) stand
estat gof, stats(all)
estat mindices

*sin Q31_5
sem (PS_MISS-> Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17)(ST_MISS -> Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17), latent (PS_MISS ST_MISS) method(mlmv) stand
estat gof, stats(all)
estat mindices

*sin Q31_5 y una cov
sem (PS_MISS-> Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17)(ST_MISS -> Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17), cov(e.Q31_6m_17*e.Q31_7m_17) latent (PS_MISS ST_MISS) method(mlmv) stand
estat gof, stats(all)
estat mindices

*sin Q31_5 y las cov más altas
sem (PS_MISS-> Q31_1m_17 Q31_2m_17 Q31_3m_17 Q31_4m_17 Q31_6m_17 Q31_7m_17 Q31_8m_17)(ST_MISS -> Q32_1m_17 Q32_2m_17 Q32_3m_17 Q32_4m_17 Q32_5m_17 Q32_6m_17), cov(e.Q31_1m_17*e.Q31_2m_17) cov(e.Q31_6m_17*e.Q31_7m_17) latent (PS_MISS ST_MISS) method(mlmv) stand
estat gof, stats(all)
estat mindices

predict PS_MLMV ST_MLMV , latent(PS_MLMV ST_MLMV)














****Notas en Palamar et al. (2011) para comparar resultados****

*Less than 5% of cases had any missing data; therefore, missing values 
// were not imputed. Cases that were missing more than two items were deleted 
// before analyses, and when a case was missing one or two items, 
//the mean value was used to replace the missing value(s).

*EFA
** Reliability: .82 en PUBLIC STIGMA Y .88 en STIGMATIZATION

**Before factor extraction, each model was set to retain factors with 
//eigenvalues greater than 1 to ensure that all values represent substantial 
//variation --- POR LO TANTO, Eigenvalues MAYORES A 1

*CFA
** The RMSEA for each model was below the cutoff of .10, suggesting acceptable 
// fit. The marijuana RMSEA was below .8, suggesting better fit
** All RMR values were below .8
** the CFI and IFI indices were above the cutoff of .90, 
// many of which were .95, also suggesting good fit.
** TABLE 4. Confirmatory factor analysis of two-factor solutions
//              χ2       RMSEA      RMR       CFI      IFI
// Marijuana  420.48     0.078     0.067     0.96      0.96
