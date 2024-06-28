library(tidyverse)
library(dplyr)

setwd("C:/Users/Admin/Desktop/QLAB/B - Dashboards/QLAB - COVID/Data")

df1 <- read.csv("positivos_covid.csv", header= TRUE, sep = ";", encoding="UTF-8")

df2 <- read.csv("fallecidos_covid.csv", header= TRUE, sep = ";", encoding="UTF-8")

#df3 <- read.csv("fallecidos_sinadef.csv", sep = ";", skip=2,  encoding="UTF-8")
df3 <- read.csv("fallecidos_sinadef.csv", encoding="UTF-8", sep="|")

###############################################################################

#LIMPIEZA PREVIA A LA BASE DE FALLECIDOS_SINADEF

##############################################################################

#Nos quedamos con las columnas PAIS.DOMICILIO, AÑO, MES 
fallecidos <- df3[,c("PAIS.DOMICILIO","AÑO", "MES")]
colnames(fallecidos)<-c("PAIS.DOMICILIO","AÑO", "MES")

#Nos quedamos con los fallecidos del 2020 para adelante
fallecidos <- subset(fallecidos, fallecidos$AÑO >= 2020)

#Nos quedamos solo con aquellos que residen en el PerC:
fallecidos <- subset(fallecidos, fallecidos$PAIS.DOMICILIO == "PERU")

#Arreglamos el MES
fallecidos$MES <- sprintf("%02d", as.numeric(fallecidos$MES ))

#Creamos una variable que tenga AÑO y MES
fallecidos$FECHA_MUERTE <- paste0( fallecidos$AÑO, fallecidos$MES )

#Nos quedamos con los fallecidos del marzo de 2020 para adelante
fallecidos <- subset(fallecidos, fallecidos$FECHA_MUERTE >= 202003)

nrow(fallecidos)
#288266

###############################################################################

#BASE STOCK NACIONAL

##############################################################################

#poblaciC3n peruana  / https://datacommons.org/place/country/PER?utm_medium=explore&mprop=count&popt=Person&hl=es

pob = 32971846

#variables para base stock
POBLACION <- pob

CASOS <- nrow(df1)

PORCENTAJE <- round( (CASOS/POBLACION)*100, digits = 2 )

FALLECIDOS <- nrow(df2) 

MORTALIDAD <- round( (FALLECIDOS/CASOS)*100, digits = 2 )

FALLECIDOS_SINADEF <- nrow(fallecidos)  

PORC_MUERTES_COVID <- round( (FALLECIDOS/FALLECIDOS_SINADEF)*100, digits = 2 )

#base stock nacional
stock_nacional <- data.frame(POBLACION, CASOS, PORCENTAJE, FALLECIDOS, MORTALIDAD, FALLECIDOS_SINADEF, PORC_MUERTES_COVID)

##################################################
# Guardamos base
################################################## 

setwd("C:/Users/Admin/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")

write.csv(stock_nacional,'covid_nacional_final.csv')