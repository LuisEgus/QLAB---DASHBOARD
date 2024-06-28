library(tidyverse)
library(dplyr)

setwd("C:/Users/Admin/Desktop/QLAB/B - Dashboards/QLAB - COVID/Data")

df1<- read.table("positivos_covid.csv", header= TRUE, sep = ";", encoding="UTF-8")

df2<- read.table("fallecidos_covid.csv", header= TRUE, sep = ";", encoding="UTF-8")

df3<- read.delim2("fallecidos_sinadef.csv", encoding="UTF-8", sep="|")  #OJO CON ESTO (LA FORMA DE IMPORTAR PUEDE CAMBIAR DEPENDIENDO DEL FORMATO DE LOS DATOS)


###############################################################################

#LIMPIEZA PREVIA A LA BASE DE FALLECIDOS_SINADEF

##############################################################################

#Nos quedamos con las columnas PAIS.DOMICILIO, AÑO, MES 
fallecidos <- df3[,c("PAIS.DOMICILIO", "AÑO", "MES")]  

#Nos quedamos con los fallecidos del 2020 para adelante
fallecidos <- subset(fallecidos, fallecidos$AÑO >= 2020)

#Nos quedamos solo con aquellos que residen en el PerC:
fallecidos <- subset(fallecidos, fallecidos$PAIS.DOMICILIO == "PERU")

#Arreglamos el MES
fallecidos$MES <- sprintf("%02d", as.numeric(fallecidos$MES ))

#Creamos una variable que tenga AÑO y MES
fallecidos$FECHA_MUERTE <- paste0( fallecidos$AÑO, fallecidos$MES )

nrow(fallecidos)
#308357


###############################################################################

#BASE PANEL PARA POSITIVOS (CANTIDAD DE POSITIVOS MENSUALMENTE)

##############################################################################

positivos_panel <- df1
nrow(positivos_panel)
#1639767
head(positivos_panel)

sum(is.na(positivos_panel$FECHA_RESULTADO))
#2023 sin FECHA_RESULTADO

#Solo nos quedamos con el aC1o y mes (omitimos el dia)
positivos_panel$FECHA_RESULTADO <- substr(positivos_panel$FECHA_RESULTADO,1,6)

#Agrupamos los datos por fecha de resultado
positivos_panel <- data.frame(table( positivos_panel$FECHA_RESULTADO))

colnames(positivos_panel) = c("FECHA", "CASOS")

nrow(positivos_panel)
#14 meses 

sum(positivos_panel$CASOS)
#1639767 - 2023= 1637744


###############################################################################

#BASE PANEL PARA FALLECIDOS (CANTIDAD DE FALLECIDOS POR DISTRITO MENSUALMENTE)

##############################################################################

fallecidos_panel <- df2
head(fallecidos_panel)
nrow(fallecidos_panel)
#54285

sum(is.na(fallecidos_panel$FECHA_FALLECIMIENTO))
#0 sin FECHA_FALLECIMIENTO

#Solo nos quedamos con el aC1o y mes (omitimos el dia)
fallecidos_panel$FECHA_FALLECIMIENTO <- substr(fallecidos_panel$FECHA_FALLECIMIENTO,1,6)

#Agrupamos los datos por fecha de fallecimiento
fallecidos_panel <- data.frame(table( fallecidos_panel$FECHA_FALLECIMIENTO))

head(fallecidos_panel)

colnames(fallecidos_panel) = c("FECHA", "FALLECIDOS")

nrow(fallecidos_panel)
#14 meses

sum(fallecidos_panel$FALLECIDOS)
#54285


###############################################################################

#BASE PANEL PARA FALLECIDOS SINADEF (CANTIDAD DE FALLECIDOS MENSUALMENTE)

##############################################################################

sinadef_panel <- fallecidos
head(sinadef_panel)


#Nos quedamos con las muertes que hayan sido registradas desde el 2020 de marzo hacia adelante
sinadef_panel <- subset(sinadef_panel, sinadef_panel$FECHA_MUERTE >= 202003)
nrow(sinadef_panel)
#288266

#Agrupamos los datos por fecha de muerte
sinadef_panel <- data.frame(table( sinadef_panel$FECHA_MUERTE))

head(sinadef_panel)

colnames(sinadef_panel) = c("FECHA", "FALLECIDOS_SINADEF")

nrow(sinadef_panel)
#14 meses

sum(sinadef_panel$FALLECIDOS_SINADEF)
#288266 no se perdieron casos

###############################################################################

#CRUZAMOS LAS BASES 

###############################################################################

##positivos panel y fallecidos panel
nacional_panel <-merge(x = positivos_panel, y = fallecidos_panel, by = c("FECHA"))

nrow(nacional_panel)
#14 meses

##departamento_panel y sinadef_panel
nacional_panel1 <-merge(x = nacional_panel, y = sinadef_panel, by = c("FECHA"))

nrow(nacional_panel1)
#14

sum(nacional_panel1$CASOS)
#1637744
sum(nacional_panel1$FALLECIDOS)
#54285
sum(nacional_panel1$FALLECIDOS_SINADEF)
#288266

##################################################
# Guardamos base
################################################## 

setwd("C:/Users/Admin/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")

write.csv(nacional_panel1,'nacional_panel_.csv')
