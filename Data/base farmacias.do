************************
*SCRIPT BASE FARMACIAS*
*Creación: 09.10.2019
*Última modificación: 11.11.2019
*Autor modificación: EA
************************

*database
cd "\\tsclient\C\Users\ealvarez\Dropbox\LAMRI\Proyecto Farmacias\Proyecto\Redesdal"
use Base_farmacias

*CREACIÓN DE VARIABLE VENDEDEDORAS vs. CONTROL
gen treat = Q56
recode treat 2/20 = 0 25=0 27=0 28=0 31=0 33=0 32=0 35=0 36=0 38=0 39=0 41=0 42=0 44/45=0 49=0 50/51=0 53/54=0 56=0 57=0 59=0 60=0 else=1

label define TREAT 0 "Control" 1 "Vendedoras"
label values treat TREAT

*CREACIÓN DE VARIABLE TIPO DE FARMACIA
gen ph_type = Q56
recode ph_type 2/20=1 25=1 27=1 28=1 31=1 33=1 32=1 35=1 36=1 38=1 39=1 41=1 42=1 44/45=1 49=1 50/51=1 53/54=1 56=1 57=1 59=1 60=1 24=5 47=5 21=2 22=2 23=2 29=2 37=2 40=2 43=2 48=2 52=2 58=2 1=3 26=3 30=3 34=3 46=3 55=3 61=4 62=4 63=4 64=4

label define PHTYPE 1 "Control" 2 "Wholetime" 3 "Drop out" 4 "Newcomers" 5 "Placebo"
labe values ph_type PHTYPE

*CREACIÓN DE VARIABLE mvd-int
gen mvd_int = Q56
recode mvd_int 2=1 3=1 4=1 5=1 6=1 7=1 8=1 9=1 10=1 11=1 12=1 13=1 14=1 15=1 16=1 17=1 18=1 19=1 20=1 24=1 25=1 21=1 22=1 23=1 1=1 61=1 62=1 63=1 64=1 else=0
label define mvd 1 "Montevideo" 0 "Interior"
label values mvd_int mvd

*CREACIÓN DE VARIABLE DE NOMBRE DE LA FARMACIA
gen ph_name = Q56
label define NAMES 1 "Pitagoras" 2 "Galena" 3	"Roosevelt" 4	"Belvedere" 5	"Mastil" 6	"Brito del Pino" 7	"Farmacia Ariel" 8	"La caja" 9	"Lulisan" 10	"Farmashop 50" 11	"Cielmar"  12	"Milena" 13	"Farmashop 58" 14	"Farmacia Goñi central" 15	"Sangar" 16	"Guarani" 17	"FARMASHOP 52" 18	"Pigalle" 19	"El tunel" 20	"San Roque" 21	"Antartida" 22	"CACERES" 23	"Tapie" 24	"Sildia" 25	"Quintela" 26	"Saga" 27	"Santa Cecilia" 28	"Horandre" 29	"Las toscas" 30	"miguel" 31	"bologna" 32	"Pirujas" 33	"Farmacia central" 34	"Carmelo"  35	"Arrieta"  36	"Ferrer" 37	"Nueva Brun" 38	"Osta" 39	"Vidal" 40	"Gortari" 41	"Idamar" 42	"Williman 2" 43	"La Cabina" 44	"Alvariza" 45	"Maldonado"  46	"Medicci" 47	"Guaviyu" 48	"Termal guaviyu" 49	"Lombardi/Guichón" 50	"San Roque" 51	"Dorotte II" 52	"Albisu Termal" 53	"Farmacia Pasteur" 54	"Nueva Republica" 55	"Bidegain" 56	"Del 26" 57	"Bellini" 58	"Bengoechea" 59	"Demilton" 60	"Dini"  61	"Camaño"  62	"Silleda"  63	"Constitución Sur" 64	"Lilen"
label values ph_name NAMES

*IDENTIFICANDO VARIABLES DE INTERÉS
**Seguridad
*Q36: conocimiento de bocas
*Q13: percepción de inseguridad sobre el barrio donde está la farmacia
*Q79: victimizanción de la farmacia
*Q82: número de robos
*Q9: confianza interpersonal

**Percepciones sobre la ley
*Q22: efectividad en seguridad pública
*Q23: efectividad en combate al narcotráfico
*Q21: efectividad en salud
*Q24: efectividad en libertades individuales

**Apoyo a la regulación
*Q17: apoyo a la regulación
*Q19: apoyo a la venta en farmacias

**Percepción sobre los usuarios y la sustancia
*Q25_6: son uan amenaza
*Q25_2: gateway
*Q25_1: perjudicial para la salud

**Descripción de la farmacia
*Q62: años de funcionamiento
*Q63: sucursales
*Q64: empresa familiar
*Q67_1: alarma con respuesta
*Q67_2: servicio 222 o guardias de seguridad de día
*Q67_3: camaras
*Q67_4: guardias de seguridad de noche
*Q54:distribución del local
*Q57: medidas de seguridad en la puerta
*Q68: n° de personas trabajando en la sucursal
*Q79_1 - Q79_4: con quién se habló la decisión de vender
*Q71_1 - Q71_5: pertenencia a asociaciones de farmacias


*MISSINGS
recode Q36 3=.
recode Q21 4=. 5=. 6=. 
recode Q22 4=. 5=. 6=. 
recode Q23 4=. 5=. 6=. 
recode Q24 4=. 5=. 6=. 
*(en estos casos también saco la opción "se mantendrá igual")
recode Q13 4=.
recode Q17 7=. 8=. 
recode Q19 7=. 8=. 
recode Q25_1 8=0 9=.
recode Q25_2 8=0 9=.
recode Q25_3 8=0 9=.
recode Q25_4 8=0 9=.
recode Q25_5 8=0 9=.
recode Q25_6 8=0 9=.
recode Q9 5=. 6=.
recode Q63 5=. 6=.
recode Q64 3=. 4=.

*ANÁLISIS DESCRIPTIVO PRELIMINAR
**SEGURIDAD
*Percepción de inseguridad de las farmacias vendedoras vs. las no vendedoras
tab Q13 treat if ronda==2017, col
tab Q13 treat if ronda==2018, col

*Victimización de las farmacias vendedoras vs. las no vendedoras
tab Q79 treat if ronda==2017, col
tab Q79 treat if ronda==2018, col

*Victimización según ronda por tipo de farmacia
tab Q79 ronda if treat==0, col
tab Q79 ronda if treat==1, col

*Conocimiento de bocas de las farmacias vendedoras vs. las no vendedoras
tab Q36 treat if ronda==2017, col
tab Q36 treat if ronda==2018, col

*Conocimiento de bocas según ronda por tipo de farmacia
tab Q36 ronda if treat==0, col
tab Q36 ronda if treat==1, col
*El reporte de la "no existencia" de bocas crece de una ronda a la otra entre los dos grupos de farmacias pero de una forma mucho más importante
//entre las vendedoras (13,4% en 2017 y 53.8% en 2018)

*Confianza interpersonal
tab Q9 treat if ronda==2017, col
tab Q9 treat if ronda==2018, col

**PERCEPCIONES SOBRE LA LEY
*Percepciones sobre seguridad pública según ronda por tipo de farmacia
tab Q22 ronda if treat==0, col
tab Q22 ronda if treat==1, col

tab Q22 treat if ronda==2017, col

*Percepciones sobre combate al narcotráfico según ronda por tipo de farmacia
tab Q23 ronda if treat==0, col
tab Q23 ronda if treat==1, col

tab Q23 treat if ronda==2017, col

*Percepciones sobre salud pública según ronda por tipo de farmacia
tab Q21 ronda if treat==0, col
tab Q21 ronda if treat==1, col

tab Q21 treat if ronda==2017, col

*Percepciones sobre libertades según ronda por tipo de farmacia
tab Q24 ronda if treat==0, col
tab Q24 ronda if treat==1, col

tab Q24 treat if ronda==2017, col

*ACUERDO CON LA REGULACIÓN
*Acuerdo con la ley según ronda por tipo de farmacia
tab Q17 treat, col
tab Q17 ronda if treat==0, col
tab Q17 ronda if treat==1, col
*acuerdo mayor en las vendedoras. Si se mira por ronda, las no vendedoras aumentan sus opiniones postivas de la regulación en la ronda 2018. 

tab Q17 treat if ronda==2017, col

*Acuerdo con la venta en farmacias según ronda por tipo de farmacia
tab Q19 ronda if treat==0, col
tab Q19 ronda if treat==1, col

tab Q19 treat if ronda==2017, col

*USUARIOS
*Acuerdo con que son una amenaza para la sociedad
tab Q25_6 treat if ronda==2017, col

*Acuerdo con que son la marihuana es una puerta
tab Q25_2 treat if ronda==2017, col

*Acuerdo con que son la marihuana es perjudicial
tab Q25_1 treat if ronda==2017, col

*FARMACIAS
*Años de funcionamiento
gen años=Q62
destring años, replace
by treat: sum años if ronda==2017

*Sucursales
tab Q63 treat if ronda==2017, col

*Empresa familiar
tab Q64 treat if ronda==2017, col

*Número de personas trabajando en la sucursal
destring Q68, replace
by treat: sum Q68 if ronda==2017

*Alarma con respuesta
recode Q67_1 4=.
tab Q67_1 treat if ronda==2017, col

*Servicio 222
recode Q67_2 4=.
tab Q67_2 treat if ronda==2017, col

*Camaras
recode Q67_3 4=.
tab Q67_3 treat if ronda==2017, col

*Guardias de seguridad
recode Q67_4 4=.
tab Q67_4 treat if ronda==2017, col

*Distribución del local
tab Q54 treat if ronda==2017, col

*Seguridad en la puerta
tab Q57 treat if ronda==2017, col

*Decisión de vender: vecinos
tab Q79_3 treat if ronda==2017, col

*Decisión de vender: dueños
tab Q79_4 treat if ronda==2017, col

*Decisión de vender: químico
tab Q79_1 treat if ronda==2017, col

*No pertenece a ninguna asociación
tab Q71_1 treat if ronda==2017, col

*Pertenece a alguna asociación


*Q71_1 - Q71_5: pertenencia a asociaciones de farmacias







*save
guardar la base
