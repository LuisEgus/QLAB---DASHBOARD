#*******************************************************************************
# MANEJO DE BASES DE POSITIVOS Y FALLECIDOS ORIGINALES PARA LA CREACI?N DE UNA BASE PANEL A NIVEL DISTRITAL
#*******************************************************************************

library(tidyverse)
library(dplyr)
library(stringr) #str_pad

setwd("C:/Users/egus2/Desktop/QLAB/B - Dashboards/QLAB - COVID/Data")

###############################################################################
#IMPORTAMOS DATOS
###############################################################################

df1<- read.table("positivos_covid.csv", header= TRUE, sep = ";", encoding="UTF-8")

df2<-read.table("fallecidos_covid.csv", header= TRUE, sep = ";", encoding="UTF-8")


df_positivos <- df1 
df_fallecidos <- df2


#Revisando las variables de cada base
ls(df_positivos)
ls(df_fallecidos)


#Verificando duplicados (si es que aparece TRUE, no hay duplicados)
length(unique(df_positivos$id_persona)) == nrow(df_positivos)

length(unique(df_fallecidos$id_persona)) == nrow(df_fallecidos)


#tama?o de cada base
nrow(df_positivos) #1387457   /1639767

nrow(df_fallecidos) #48323    /54285


#Nos quedamos solo con las columnas de inter?s

#df_positivos (columnas: DEPARTAMENTO PROVINCIA FECHA_RESULTADO)
df_positivos <- df_positivos[ ,c("UBIGEO","FECHA_RESULTADO")]

#df_fallecidos (columnas: DEPARTAMENTO PROVINCIA FECHA_FALLECIMIENTO)
df_fallecidos <- df_fallecidos[ ,c("UBIGEO","FECHA_FALLECIMIENTO")]

#Arreglamos el formato del UBIGEO para que se mantenga con 6 dígitos (DEPATAMENTO + PROVINCIA + DISTRITO)

df_positivos$UBIGEO <- as.numeric(df_positivos$UBIGEO)
df_fallecidos$UBIGEO <- as.numeric(df_fallecidos$UBIGEO)


df_positivos$UBIGEO <- sprintf("%06d", df_positivos$UBIGEO)

df_fallecidos$UBIGEO <- sprintf("%06d", df_fallecidos$UBIGEO)


###############################################################################       
#BASE POSITIVOS      
###############################################################################

head(df_positivos)
nrow(df_positivos)
#1387457/ 1639767

##############
#NA
#############

#Filas con NA
df_positivos[rowSums(is.na(df_positivos)) > 0,] 

sum(is.na(df_positivos$FECHA_RESULTADO))
#2023 sin FECHA_RESULTADO (No me sirven para base panel, pero sí para base stock)

sum(is.na(df_positivos$DEPARTAMENTO))
sum(is.na(df_positivos$PROVINCIA))
sum(is.na(df_positivos$DISTRITO))
# Las columnas de DEPARTAMENTO, PROVINCIA Y DISTRITO no tienen NA


#Eliminanos todas las filas que tengan NA en alguna columna, ya que no nos sirve para la base panel
df_positivos <-df_positivos[complete.cases(df_positivos), ]  #eliminos todas las filas que tengan algun NA

#eliminamos las filas que tengan algun espacio en blanco
df_positivos <- df_positivos[!apply(df_positivos == "", 1, all),]

nrow(df_positivos)
#13854354 / 1637744


###############################################################################       
#BASE FALLECIDOS 
###############################################################################

head(df_fallecidos)
nrow(df_fallecidos)
#48323 / 54285

##############
#NA
#############

#Filas con NA
df_fallecidos[rowSums(is.na(df_fallecidos)) > 0,]
nrow(df_fallecidos[rowSums(is.na(df_fallecidos)) > 0,])
#0

sum(is.na(df_fallecidos$FECHA_FALLECIMIENTO))
#0 con NA
sum(is.na(df_fallecidos$DEPARTAMENTO))
#0 con NA
sum(is.na(df_fallecidos$PROVINCIA))
#0 con NA
sum(is.na(df_fallecidos$DISTRITO))
#0 con NA

#Eliminanos todas las filas que tengan NA en alguna columna, ya que no nos sirve para la base panel
df_fallecidos <-df_fallecidos[complete.cases(df_fallecidos), ]  #eliminos todas las filas que tengan algun NA

#eliminamos las filas que tengan algun espacio en blanco
df_fallecidos <- df_fallecidos[!apply(df_fallecidos == "", 1, all),] #No me funcion?


nrow(df_fallecidos)
#48323 / 54285



########################       
#Guardamos bases     
#######################

setwd("C:/Users/egus2/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")

write.csv(df_positivos,'positivos_.csv')
write.csv(df_fallecidos,'fallecidos_.csv')