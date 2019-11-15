**************************
****FACTORIAL ANALYSIS****
**************************







***CFA***
*Usamos el método MLMV incluyendo la variable Q31_5. Los pasos seguidos para encontrar el mejor modelo 
///pueden verse en el archivo "Proceso factorial" en la carpeta Análisis factorial el Dropbox.

**CFA PARA RONDA 1 (2017)
sem (ps-> pstigma1_miss_i pstigma2_miss_i pstigma3_miss_i pstigma4_miss_i pstigma5_miss_i pstigma6_miss_i pstigma7_miss_i pstigma8_miss_i) (st -> stigma1_miss_i stigma2_miss_i stigma3_miss_i stigma4_miss_i stigma5_miss_i stigma6_miss_i) if ronda==2017, cov(e.pstigma6_miss_i*e.pstigma7_miss_i) cov(e.pstigma1_miss_i*e.pstigma2_miss_i) latent (ps st) method(mlmv) stand
estat gof, stats(all)
estat mindices

predict ps2017 st2017 if ronda==2017, latent(ps st)

**CFA PARA RONDA 2 (2018)
sem (ps-> pstigma1_miss_i pstigma2_miss_i pstigma3_miss_i pstigma4_miss_i pstigma5_miss_i pstigma6_miss_i pstigma7_miss_i pstigma8_miss_i) (st -> stigma1_miss_i stigma2_miss_i stigma3_miss_i stigma4_miss_i stigma5_miss_i stigma6_miss_i) if ronda==2018, cov(e.pstigma6_miss_i*e.pstigma7_miss_i) cov(e.pstigma1_miss_i*e.pstigma2_miss_i) latent (ps st) method(mlmv) stand
estat gof, stats(all)
estat mindices

predict ps2018 st2018 if ronda==2018, latent(ps st)



