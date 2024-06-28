library(tidyverse)
library(dplyr)


###############################################################################
#IMPORTAMOS DATOS
###############################################################################

setwd("C:/Users/Admin/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")

sinadef<- read.csv("fallecidos_sinadef.csv", encoding="UTF-8")
sinadef<- sinadef[,-1] #Delete first column

panel_nacional<- read.csv("nacional_panel_.csv")
panel_nacional<- panel_nacional[,-1] #Delete first column

panel_departamento<- read.csv("departamento_panel_.csv")
panel_departamento<- panel_departamento[,-1] #Delete first column

panel_distrito<- read.csv("distrito_panel_.csv", encoding = "UTF-8")
panel_distrito <- panel_distrito[,-1] #Delete first column

panel_provincia<- read.csv("provincia_panel_.csv", encoding = "UTF-8")
panel_provincia<- panel_provincia[,-1] #Delete first column


#Acotamos la base sinadef para solo quedarnos con las muertes del año 2019
sinadef <- subset(sinadef, sinadef$FECHA_MUERTE >= 201901 & sinadef$FECHA_MUERTE <= 201912 )

unique(sinadef$FECHA_MUERTE)


###############################################################################
#A NIVEL NACIONAL
###############################################################################
#muertes mensuales en el 2019 
fallecidos_nac_sinadef <- data.frame(table(sinadef$FECHA_MUERTE))

nrow(fallecidos_nac_sinadef )
#12 meses

colnames(fallecidos_nac_sinadef) = c("MES", "FALLECIDOS_2019")
fallecidos_nac_sinadef$MES <- substr(fallecidos_nac_sinadef$MES,5,6)

#arreglos en la base panel
panel_nacional$MES <- substr(panel_nacional$FECHA,5,6)

#cruzamos bases
panel_nacional_ <-merge(x = panel_nacional, y = fallecidos_nac_sinadef, by = c("MES"), all.x = TRUE)

head(panel_nacional_)

#Ordamos los datos por fecha
panel_nacional_ <- panel_nacional_[ order( panel_nacional_$FECHA ) , ]  

#No se perdieron 
sum(panel_nacional_$CASOS) #1637744
sum(panel_nacional$CASOS)

sum(panel_nacional_$FALLECIDOS) #54285
sum(panel_nacional$FALLECIDOS)

sum(panel_nacional_$FALLECIDOS_SINADEF) #287728
sum(panel_nacional$FALLECIDOS_SINADEF)

#eliminaos la columna MES
panel_nacional_ <- panel_nacional_[,-1]

#creamos columna de exceso de muertes
panel_nacional_$EXCESO_MUERTES <- panel_nacional_$FALLECIDOS_SINADEF - panel_nacional_$FALLECIDOS_2019

#################
# Guardamos base
#################

write.csv(panel_nacional_,'_nacional_panel_.csv')

###############################################################################
#A NIVEL DEPARTAMENTAL
###############################################################################

#muertes mensuales en el 2019 por departamento
fallecidos_dep_sinadef <- data.frame(table(sinadef$DEPARTAMENTO, sinadef$FECHA_MUERTE))

nrow(fallecidos_dep_sinadef )
#300 (25 departamentos x 12 meses)

colnames(fallecidos_dep_sinadef) = c("DEPARTAMENTO", "MES", "FALLECIDOS_2019")
fallecidos_dep_sinadef$MES <- substr(fallecidos_dep_sinadef$MES,5,6)

#arreglos en la base panel
panel_departamento$MES <- substr(panel_departamento$FECHA,5,6)

#cruzamos bases
panel_departamento_ <-merge(x = panel_departamento, y = fallecidos_dep_sinadef, by = c("DEPARTAMENTO", "MES"), all.x = TRUE)

head(panel_departamento_)

#Ordamos los datos primero por departamento y luego por fecha
panel_departamento_ <- panel_departamento_[ order( panel_departamento_$DEPARTAMENTO , panel_departamento_$FECHA ) , ]  

#No se perdieron 
sum(panel_departamento_$CASOS) #1637744
sum(panel_departamento$CASOS)

sum(panel_departamento_$FALLECIDOS) #54285
sum(panel_departamento$FALLECIDOS)

sum(panel_departamento_$FALLECIDOS_SINADEF) #287728
sum(panel_departamento$FALLECIDOS_SINADEF)

#eliminaos la columna MES
panel_departamento_ <- panel_departamento_[,-2]

#Filas con NA
panel_departamento_[rowSums(is.na(panel_departamento_)) > 0,] 

#creamos columna de exceso de muertes
panel_departamento_$EXCESO_MUERTES <- panel_departamento_$FALLECIDOS_SINADEF - panel_departamento_$FALLECIDOS_2019

#################
# Guardamos base
#################

write.csv(panel_departamento_,'_departamento_panel_.csv')


###############################################################################
#A NIVEL PROVINCIAL
###############################################################################

#muertes mensuales en el 2019 por provincia
fallecidos_prov_sinadef <- data.frame(table(sinadef$ubigeo_inei2, sinadef$FECHA_MUERTE))

nrow(fallecidos_prov_sinadef)
#2352 (196 provincias x 12 meses)

colnames(fallecidos_prov_sinadef) = c("ubigeo_inei", "MES", "FALLECIDOS_2019")
fallecidos_prov_sinadef$MES <- substr(fallecidos_prov_sinadef$MES,5,6)

#arreglos en la base panel
panel_provincia$MES <- substr(panel_provincia$FECHA,5,6)

#cruzamos bases
panel_provincia_ <-merge(x = panel_provincia, y = fallecidos_prov_sinadef, by = c("ubigeo_inei", "MES"), all.x = TRUE)

head(panel_provincia_)

#Ordamos los datos primero por departamento y luego por fecha
panel_provincia_ <- panel_provincia_[ order( panel_provincia_$ubigeo_inei , panel_provincia_$FECHA ) , ]  

#No se perdieron 
sum(panel_provincia_$CASOS) #1552427
sum(panel_provincia$CASOS)

sum(panel_provincia_$FALLECIDOS) #54042
sum(panel_provincia$FALLECIDOS)

sum(panel_provincia_$FALLECIDOS_SINADEF) #287728
sum(panel_provincia$FALLECIDOS_SINADEF)

#eliminaos la columna MES
panel_provincia_ <- panel_provincia_[,-2]

#los NA en FALLECIDOS_SINADEF y FALLECIDOS_2019 cambiarlos por 0  :OJO CON ESTO
panel_provincia_$FALLECIDOS_SINADEF[is.na(panel_provincia_$FALLECIDOS_SINADEF)] = 0
panel_provincia_$FALLECIDOS_2019[is.na(panel_provincia_$FALLECIDOS_2019)] = 0

#creamos columna de exceso de muertes
panel_provincia_$EXCESO_MUERTES <- panel_provincia_$FALLECIDOS_SINADEF - panel_provincia_$FALLECIDOS_2019

#################
# Guardamos base
#################

write.csv(panel_provincia_,'_provincia_panel_.csv')


###############################################################################
#A NIVEL DISTRITAL
###############################################################################

#muertes mensuales en el 2019 por distrito
fallecidos_dist_sinadef <- data.frame(table(sinadef$ubigeo_inei, sinadef$FECHA_MUERTE))

nrow(fallecidos_dist_sinadef)
#22056 (1838 distritos x 12 meses)  - hay 1874 

colnames(fallecidos_dist_sinadef) = c("ubigeo_inei", "MES", "FALLECIDOS_2019")
fallecidos_dist_sinadef$MES <- substr(fallecidos_dist_sinadef$MES,5,6)

#arreglos en la base panel
panel_distrito$MES <- substr(panel_distrito$FECHA,5,6)

#cruzamos bases
panel_distrito_ <-merge(x = panel_distrito, y = fallecidos_dist_sinadef, by = c("ubigeo_inei", "MES"), all.x = TRUE)

head(panel_distrito_)

#Ordamos los datos primero por departamento y luego por fecha
panel_distrito_ <- panel_distrito_[ order( panel_distrito_$ubigeo_inei , panel_distrito_$FECHA ) , ]  

#No se perdieron 
sum(panel_distrito_$CASOS) #1552374
sum(panel_distrito$CASOS)

sum(panel_distrito_$FALLECIDOS) #53689
sum(panel_distrito$FALLECIDOS)

sum(panel_distrito_$FALLECIDOS_SINADEF) #287728
sum(panel_distrito$FALLECIDOS_SINADEF)

#eliminaos la columna MES
panel_distrito_ <- panel_distrito_[,-2]

#los NA en FALLECIDOS_SINADEF y FALLECIDOS_2019 cambiarlos por 0  :OJO CON ESTO
panel_distrito_$FALLECIDOS_SINADEF[is.na(panel_distrito_$FALLECIDOS_SINADEF)] = 0
panel_distrito_$FALLECIDOS_2019[is.na(panel_distrito_$FALLECIDOS_2019)] = 0

#creamos columna de exceso de muertes
panel_distrito_$EXCESO_MUERTES <- panel_distrito_$FALLECIDOS_SINADEF - panel_distrito_$FALLECIDOS_2019

#################
# Guardamos base
#################

write.csv(panel_distrito_,'_distrito_panel_.csv')