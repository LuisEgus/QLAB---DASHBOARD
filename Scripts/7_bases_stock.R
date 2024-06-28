library(tidyverse)
library(dplyr)
library(stringr) #str_pad

setwd("C:/Users/Admin/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")

################################################################################
#IMPORTAMOS DATOS
###############################################################################

#distrito
df1<- read.table("positivos_.csv", header= TRUE, sep = ",", encoding="UTF-8")
df2<- read.table("fallecidos_.csv", header= TRUE, sep = ",", encoding="UTF-8")

#departamento
df3<- read.table("positivos_dep_.csv", header= TRUE, sep = ",", encoding="UTF-8")
df4<- read.table("fallecidos_dep_.csv", header= TRUE, sep = ",", encoding="UTF-8")

#poblacion
df5<-read.table("poblacion_distrito_final.csv", header= TRUE, sep= ",", encoding="LATIN1")
df6<-read.table("poblacion_departamento_final.csv", header= TRUE, sep= ",", encoding="UTF-8")
df10<-read.table("poblacion_provincia_final.csv", header= TRUE, sep= ",", encoding="LATIN1")

#sinadef
df7<-read.table("fallecidos_sinadef.csv", header= TRUE, sep= ",", encoding="UTF-8")

#provincia
df8<- read.table("positivos_prov_.csv", header= TRUE, sep = ",", encoding="UTF-8")
df9<- read.table("fallecidos_prov_.csv", header= TRUE, sep = ",", encoding="UTF-8")



df1 <- df1[,-1] #Delete first column
df2 <- df2[,-1]
df3 <- df3[,-1]
df4 <- df4[,-1] #Delete first column
df5 <- df5[,-1]
df6 <- df6[,-1]
df7 <- df7[,-1]
df8 <- df8[,-1]
df9 <- df9[,-1]
df10 <- df10[,-1]


pos_dist      <- df1 
fall_dist     <- df2
pos_dep       <- df3
fall_dep      <- df4
pos_prov      <- df8
fall_prov     <- df9
pob_dist      <- df5
pob_dep       <- df6
pob_prov      <- df10
fall_sinadef  <- df7

#rm("df1", "df2", "df3", "df4", "df5", "df6", "df7", "df8", "df9", "df10")

#Nos quedamos con las muertes que hayan sido registradas desde el 2020 de marzo hacia adelante
fall_sinadef <- subset(fall_sinadef, fall_sinadef$FECHA_MUERTE >= 202003)

###############################################################################
# A NIVEL DISTRITAL
###############################################################################

#################
#POSITIVOS
################

#CASOS_DISTRITOS: cantidad de casos por distritos

casos_distritos <- data.frame(table(pos_dist$UBIGEO))
nrow(casos_distritos) #1824 distritos con casos positivos

colnames(casos_distritos) = c("ubigeo_inei", "CASOS_DISTRITO")
head(casos_distritos)   

#Eliminamos los NA (OJO CON ESTO- SI ES QUE NO HAY "NA" ENTONCES NO DEBERIA EL CODIGO)
casos_distritos <- casos_distritos[- grep("NA", casos_distritos$ubigeo_inei),]

sum(casos_distritos$CASOS_DISTRITO)

#DISTRITOS_CONTAGIOS: % de contagios por distrito 

pob_dist$ubigeo_inei <- as.numeric(pob_dist$ubigeo_inei)
pob_dist$ubigeo_inei <- sprintf("%06d", pob_dist$ubigeo_inei)  #ASEGURARSE QUE LA BASE DE CASOS_DISTRITOS TENGA EL UBIGEO CON 6 DÍGITOS

distritos_contagios <- merge(x = pob_dist, y = casos_distritos , by = "ubigeo_inei", all.x = TRUE)
head(distritos_contagios)
nrow(distritos_contagios) #1874 distritos

#los NA en CASOS POR DISTRITO cambiarlos por 0  :OJO CON ESTO
distritos_contagios$CASOS_DISTRITO[is.na(distritos_contagios$CASOS_DISTRITO)] = 0

summary(distritos_contagios$CASOS_DISTRITO)

# % de contagios
distritos_contagios$PORCENTAJE_DISTRITO <- round( (distritos_contagios$CASOS_DISTRITO/distritos_contagios$POBLACION)*100, digits = 2)

sum(distritos_contagios$CASOS_DISTRITO)  #1314170 . Se perdieron 1314211 - 1314170  = 41
#1552374. Se perdieron  1552427 - 1552374  = 53
#################
#FALLECIDOS
################

#FALLECIDOS_DISTRITOS: la cantidad de fallecidos por distritos

fallecidos_distritos <- data.frame(table(fall_dist$UBIGEO))
nrow(fallecidos_distritos) #1550 distritos

colnames(fallecidos_distritos) = c("ubigeo_inei", "FALLECIDOS_DISTRITO")

#Eliminamos los NA
fallecidos_distritos <- fallecidos_distritos[- grep("NA", fallecidos_distritos$ubigeo_inei),]

sum(fallecidos_distritos$FALLECIDOS_DISTRITO)  #48323/ 54285 (no se perdieron - ver script "Positivos_fallecidos_distritos")

#CONTAGIOS_FALLECIDOS_DISTRITOS (cont_fall_dist): casos, % de contagios, fallecidos y mortalidad por distrito

cont_fall_dist <- merge(x = distritos_contagios, y = fallecidos_distritos , by = "ubigeo_inei", all.x = TRUE)
head(cont_fall_dist)
nrow(cont_fall_dist) #1874 distritos
sum(cont_fall_dist$CASOS_DISTRITO)  #1314170 / 1552374

#los NA en FALLECIDOS POR DISTRITO cambiarlos por 0  :OJO CON ESTO
cont_fall_dist$FALLECIDOS_DISTRITO[is.na(cont_fall_dist$FALLECIDOS_DISTRITO)] = 0
sum(cont_fall_dist$FALLECIDOS_DISTRITO)  #47067. Se perdieron 48323 - 47067 = 1256
#53689. Se perdieron 54285 - 53689 = 596 

summary(cont_fall_dist$FALLECIDOS_DISTRITO)

# % de fallecidos del total de contagios por distritos (mortalidad por distrito) |||  OJO CON ESTO (la cifra puede ser engañosa ver el caso de ubigeo inei = 10102)
cont_fall_dist$MORTALIDAD_DISTRITO <- round( (cont_fall_dist$FALLECIDOS_DISTRITO/cont_fall_dist$CASOS_DISTRITO)*100, digits = 2)

head(cont_fall_dist)


###################
#FALLECIDOS SINADEF
##################

#FALL_DIST_SINADEF: la cantidad de fallecidos por distritos SINADEF

fall_dist_sinadef <- data.frame(table(fall_sinadef$ubigeo_inei))
nrow(fall_dist_sinadef) #1856 / 1858 distritos con muertes SINADEF (no se perdieron ver script "Sinadef")

colnames(fall_dist_sinadef) = c("ubigeo_inei", "FALLECIDOS_SINADEF")

sum(fall_dist_sinadef$FALLECIDOS_SINADEF)  #299309 / 307749 (no se perdieron - ver script Sinadef")      


#################
#BASE FINAL
################

fall_dist_sinadef$ubigeo_inei <- as.numeric(as.character(fall_dist_sinadef$ubigeo_inei))
fall_dist_sinadef$ubigeo_inei <- sprintf("%06d", fall_dist_sinadef$ubigeo_inei)


final_distrito <- merge(x = cont_fall_dist, y = fall_dist_sinadef , by = "ubigeo_inei", all.x = TRUE)
head(final_distrito)
nrow(final_distrito) #1874 distritos

summary(final_distrito$FALLECIDOS_SINADEF) #18 NA tiene sentido (1874-1856)/ 16 NA
sum(final_distrito$CASOS_DISTRITO) #1314170 / 1552374
sum(final_distrito$FALLECIDOS_DISTRITO) #47067 / 53689


#final_distrito$FALLECIDOS_SINADEF[is.na(final_distrito$FALLECIDOS_SINADEF)] = 0
#sum(final_distrito$FALLECIDOS_SINADEF) #299309 / 307749 no se perdieron

final_distrito <- final_distrito[,c("ubigeo_inei", "POBLACION", "DEPARTAMENTO", "PROVINCIA", "DISTRITO", "LLAVE", "CASOS_DISTRITO", 
                                    "PORCENTAJE_DISTRITO", "FALLECIDOS_DISTRITO", "MORTALIDAD_DISTRITO", "FALLECIDOS_SINADEF")]

# % de fallecidos por covid (FALLECIDOS POR DISTRITO/ FALLECIDOS_SINADEF)
final_distrito$PORC_MUERTES_COVID <- round( (final_distrito$FALLECIDOS_DISTRITO/final_distrito$FALLECIDOS_SINADEF)*100, digits = 2)

colnames(final_distrito) <- c("CODIGO", "POBLACION", "DEPARTAMENTO", "PROVINCIA", "DISTRITO", "LLAVE", "CASOS POR DISTRITO", 
                              "PORCENTAJE_DISTRITO", "FALLECIDOS POR DISTRITO", "MORTALIDAD_DISTRITO", "FALLECIDOS_SINADEF", "PORC_MUERTES_COVID")


#################
# Guardamos base
#################

write.csv(final_distrito,'covid_distrito_final.csv')


###############################################################################
# A NIVEL PROVINCIAL
###############################################################################

#################
#POSITIVOS
################

#CASOS_PROVINCIAS: cantidad de casos por provincias

casos_provincias <- data.frame(table(pos_prov$UBIGEO))
nrow(casos_provincias) #196 provincias con casos positivos

colnames(casos_provincias) = c("ubigeo_inei", "CASOS_PROVINCIA")
head(casos_provincias)   

sum(casos_provincias$CASOS_PROVINCIA)

#PROVINCIAS_CONTAGIOS: % de contagios por provincia

provincias_contagios <- merge(x = pob_prov, y = casos_provincias , by = "ubigeo_inei", all.x = TRUE)
head(provincias_contagios)
nrow(provincias_contagios) #196 provincias

#los NA en CASOS POR PROVINCIA cambiarlos por 0  :OJO CON ESTO
provincias_contagios$CASOS_PROVINCIA[is.na(provincias_contagios$CASOS_PROVINCIA)] = 0

summary(provincias_contagios$CASOS_PROVINCIA)

# % de contagios
provincias_contagios$PORCENTAJE_PROVINCIA <- round( (provincias_contagios$CASOS_PROVINCIA/provincias_contagios$POBLACION)*100, digits = 2)

sum(provincias_contagios$CASOS_PROVINCIA)  
#1552427, no se perdieron

#################
#FALLECIDOS               
################


#FALLECIDOS_PROVINCIAS: la cantidad de fallecidos por provincias

fallecidos_provincias <- data.frame(table(fall_prov$UBIGEO))
nrow(fallecidos_provincias) #196

colnames(fallecidos_provincias) = c("ubigeo_inei", "FALLECIDOS_PROVINCIA")

sum(fallecidos_provincias$FALLECIDOS_PROVINCIA)  #54285 (no se perdieron - ver script "Positivos_fallecidos_provincias")

#CONTAGIOS_fallecidos_provincias (cont_fall_prov: casos, % de contagios, fallecidos y mortalidad por provincia

cont_fall_prov <- merge(x = provincias_contagios, y = fallecidos_provincias , by = "ubigeo_inei", all.x = TRUE)
head(cont_fall_prov)
nrow(cont_fall_prov) #196 provincias
sum(cont_fall_prov$CASOS_PROVINCIA)  #1552427
sum(cont_fall_prov$FALLECIDOS_PROVINCIA)  
# 54042. Se perdieron 54285 -  54042 = 243 

#los NA en FALLECIDOS POR PROVINCIA cambiarlos por 0  :OJO CON ESTO
cont_fall_prov$FALLECIDOS_PROVINCIA[is.na(cont_fall_prov$FALLECIDOS_PROVINCIA)] = 0

summary(cont_fall_prov$FALLECIDOS_PROVINCIA)

# % de fallecidos del total de contagios por provincias (mortalidad por provincia) |||  OJO CON ESTO 
cont_fall_prov$MORTALIDAD_PROVINCIA <- round( (cont_fall_prov$FALLECIDOS_PROVINCIA/cont_fall_prov$CASOS_PROVINCIA)*100, digits = 2)

head(cont_fall_prov)        

###################
#FALLECIDOS SINADEF  
##################

#FALL_DIST_SINADEF: la cantidad de fallecidos por provincias SINADEF

fall_prov_sinadef <- data.frame(table(fall_sinadef$ubigeo_inei2))
nrow(fall_prov_sinadef) #196 provincias

colnames(fall_prov_sinadef) = c("ubigeo_inei", "FALLECIDOS_SINADEF")

sum(fall_prov_sinadef$FALLECIDOS_SINADEF)  #421762 (no se perdieron - ver script Sinadef")      

#################
#BASE FINAL
################

final_provincia <- merge(x = cont_fall_prov, y = fall_prov_sinadef , by = "ubigeo_inei", all.x = TRUE)
head(final_provincia)
nrow(final_provincia) #196 provincias

summary(final_provincia$FALLECIDOS_SINADEF) #Sin NA
sum(final_provincia$CASOS_PROVINCIA) #1552427
sum(final_provincia$FALLECIDOS_PROVINCIA) #54042
sum(final_provincia$FALLECIDOS_SINADEF) #421762 no se perdieron

final_provincia <- final_provincia[,c("ubigeo_inei", "POBLACION", "DEPARTAMENTO", "PROVINCIA", "LLAVE", "CASOS_PROVINCIA", 
                                      "PORCENTAJE_PROVINCIA", "FALLECIDOS_PROVINCIA", "MORTALIDAD_PROVINCIA", "FALLECIDOS_SINADEF")]

# % de fallecidos por covid (FALLECIDOS POR PROVINCIA/ FALLECIDOS_SINADEF)
final_provincia$PORC_MUERTES_COVID <- round( (final_provincia$FALLECIDOS_PROVINCIA/final_provincia$FALLECIDOS_SINADEF)*100, digits = 2)

colnames(final_provincia) <- c("CODIGO", "POBLACION", "DEPARTAMENTO", "PROVINCIA", "LLAVE", "CASOS POR PROVINCIA", 
                               "PORCENTAJE_PROVINCIA", "FALLECIDOS POR PROVINCIA", "MORTALIDAD_PROVINCIA", "FALLECIDOS_SINADEF", "PORC_MUERTES_COVID")        


#################
# Guardamos base
#################

write.csv(final_provincia,'covid_provincia_final.csv')


###############################################################################
# A NIVEL DEPARTAMENTAL
###############################################################################


#################
#POSITIVOS
################

#CASOS_DEPARTAMENTOS: la cantidad de casos por departamento
casos_departamentos <- data.frame(table(pos_dep$DEPARTAMENTO))
nrow(casos_departamentos) #25

colnames(casos_departamentos) = c("DEPARTAMENTO", "CASOS_DEPARTAMENTO")

sum(casos_departamentos$CASOS_DEPARTAMENTO) #1385434 / 1637744  (no se perdieron ver script "Positivos_fallecidos_departamentos")


#DEPARTAMENTOS_CONTAGIOS: % de contagios por departamento
departamentos_contagios <- merge(x = pob_dep, y = casos_departamentos , by = "DEPARTAMENTO", all.x = TRUE)
head(departamentos_contagios)
nrow(departamentos_contagios) #25

sum(departamentos_contagios$CASOS_DEPARTAMENTO) #1385434 / 1637744

# % de contagios
departamentos_contagios$PORCENTAJE_DEPARTAMENTO <- round( (departamentos_contagios$CASOS_DEPARTAMENTO/departamentos_contagios$POBLACION)*100, digits = 2)


#################
#FALLECIDOS
################

#FALLECIDOS_DEPARTAMENTOS: la cantidad de fallecidos por departamentos
fallecidos_departamentos <- data.frame(table(fall_dep$DEPARTAMENTO))
nrow(fallecidos_departamentos) #25

colnames(fallecidos_departamentos) = c("DEPARTAMENTO", "FALLECIDOS_DEPARTAMENTO")

sum(fallecidos_departamentos$FALLECIDOS_DEPARTAMENTO) #48323 / 54285  (no se perdieron ver script "Positivos_fallecidos_departamentos")


###################
#FALLECIDOS SINADEF
##################

#FALLECIDOS_DEP_SINADEF: la cantidad de fallecidos por departamentos (Sinadef)
fallecidos_dep_sinadef <- data.frame(table(fall_sinadef$DEPARTAMENTO))
nrow(fallecidos_dep_sinadef) #25

colnames(fallecidos_dep_sinadef) = c("DEPARTAMENTO", "FALLECIDOS_SINADEF")

sum(fallecidos_dep_sinadef$FALLECIDOS_SINADEF) #299309 / 307749 (no se perdieron ver script "Sinadef")


###################
#BASE FINAL
##################

#CONTAGIOS_FALLECIDOS_DEPARTAMENTOS (cont_fall_dep): casos, % de contagios, fallecidos y mortalidad por departamento

cont_fall_dep <- merge(x = departamentos_contagios, y = fallecidos_departamentos , by = "DEPARTAMENTO", all.x = TRUE)

sum(cont_fall_dep$CASOS_DEPARTAMENTO) #1385434 / 1637744
sum(cont_fall_dep$FALLECIDOS_DEPARTAMENTO) #48323 / 54285

# % de fallecidos del total de contagios por departamento (mortalidad por departamento) |||  OJO CON ESTO 
cont_fall_dep$MORTALIDAD_DEPARTAMENTO <- round( (cont_fall_dep$FALLECIDOS_DEPARTAMENTO/cont_fall_dep$CASOS_DEPARTAMENTO)*100, digits = 2)

head(cont_fall_dep)

#LE AGREGAMOS LA BASE FALLECIDOS_DEP_SINADEF

cont_fall_dep_ <- merge(x = cont_fall_dep, y = fallecidos_dep_sinadef , by = "DEPARTAMENTO", all.x = TRUE)

sum(cont_fall_dep_$CASOS_DEPARTAMENTO) #1385434 /  1637744
sum(cont_fall_dep_$FALLECIDOS_DEPARTAMENTO) #48323 / 54285 
sum(cont_fall_dep_$FALLECIDOS_SINADEF) #299309  / 307749

# % de fallecidos por covid (FALLECIDOS POR DISTRITO/ FALLECIDOS_SINADEF)
cont_fall_dep_$PORC_MUERTES_COVID <- round( (cont_fall_dep_$FALLECIDOS_DEPARTAMENTO/cont_fall_dep_$FALLECIDOS_SINADEF)*100, digits = 2)

#################
# Guardamos base
#################

write.csv(cont_fall_dep_,'covid_departamento_final.csv')

