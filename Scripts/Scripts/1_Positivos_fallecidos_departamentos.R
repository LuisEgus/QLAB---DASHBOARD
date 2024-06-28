
#*******************************************************************************
# MANEJO DE BASES DE POSITIVOS Y FALLECIDOS ORIGINALES PARA LA CREACI?N DE UNA BASE PANEL A NIVEL DEPARTAMENTAL
#*******************************************************************************

library(tidyverse)
library(dplyr)

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

#df_positivos (columnas: DEPARTAMENTO FECHA_RESULTADO)
df_positivos <- df_positivos[ ,c("DEPARTAMENTO","FECHA_RESULTADO")]

#df_fallecidos (columnas: DEPARTAMENTO FECHA_FALLECIMIENTO)
df_fallecidos <- df_fallecidos[ ,c("DEPARTAMENTO","FECHA_FALLECIMIENTO")]

#Quitamos tildes y caracteres extra?os en las columnas DEPARTAMENTO(1)          

df_positivos[,c("DEPARTAMENTO")]<- iconv(df_positivos[,c("DEPARTAMENTO")], from="UTF-8", to="ASCII//TRANSLIT")

df_fallecidos[,c("DEPARTAMENTO")]<- iconv(df_fallecidos[,c("DEPARTAMENTO")], from="UTF-8", to="ASCII//TRANSLIT")

###############################################################################       
#BASE POSITIVOS      
###############################################################################

head(df_positivos)
nrow(df_positivos)
#1387457 / 1639767

##############
#NA
#############

#Filas con NA
#df_positivos[rowSums(is.na(df_positivos)) > 0,] 

sum(is.na(df_positivos$FECHA_RESULTADO))
#2023 sin FECHA_RESULTADO (No me sirven para base panel, pero s? para base stock)

sum(is.na(df_positivos$DEPARTAMENTO))
# Las columnas de DEPARTAMENTO no tienen NA

#Eliminanos todas las filas que tengan NA en alguna columna, ya que no nos sirve para la base panel
df_positivos <-df_positivos[complete.cases(df_positivos), ]  #eliminos todas las filas que tengan algun NA

#eliminamos las filas que tengan alg?n espacio en blanco
df_positivos <- df_positivos[!apply(df_positivos == "", 1, all),]

nrow(df_positivos)
#13854354 / 1637744


##################
#EN INVESTIGACION
##################

#Para base a nivel departamental, no nos sirve los que tengan "EN INVESTIGACION" en las columnas de DEPARTAMENTO

df_positivos <- filter(df_positivos, df_positivos$DEPARTAMENTO != "EN INVESTIGACION" ) 

nrow(df_positivos)
# 1385434 /1637744


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
#df_fallecidos[rowSums(is.na(df_fallecidos)) > 0,]
nrow(df_fallecidos[rowSums(is.na(df_fallecidos)) > 0,])
#0

sum(is.na(df_fallecidos$FECHA_FALLECIMIENTO))
#0 con NA
sum(is.na(df_fallecidos$DEPARTAMENTO))
#0 con NA

#Eliminanos todas las filas que tengan NA en alguna columna, ya que no nos sirve para la base panel
df_fallecidos <-df_fallecidos[complete.cases(df_fallecidos), ]  #eliminos todas las filas que tengan alg?n NA

#eliminamos las filas que tengan alg?n espacio en blanco
df_fallecidos <- df_fallecidos[!apply(df_fallecidos == "", 1, all),] 


nrow(df_fallecidos)
#48323 / 54285


##################
#EN INVESTIGACION
##################

#Para base a nivel distrital, no nos sirve los que tengan "EN INVESTIGACION" en cualquiera de las columnas de DEPARTAMENTO

df_fallecidos <- filter(df_fallecidos, df_fallecidos$DEPARTAMENTO != "EN INVESTIGACION" )  

nrow(df_fallecidos)
# 48323 / 54285


###############################################################################
#CORREGIMOS NOMBRES DE DEPARTAMENTOS
###############################################################################

########################       
#BASE POSITIVOS        
#######################      

#Corregimos el departamento de LIMA REGION por LIMA
df_positivos$DEPARTAMENTO[df_positivos$DEPARTAMENTO == "LIMA REGION"] <- "LIMA" 
df_positivos$DEPARTAMENTO[df_positivos$DEPARTAMENTO == "LIMA METROPOLITANA"] <- "LIMA" 

unique(df_positivos$DEPARTAMENTO)


########################       
#BASE FALLECIDOS       
#######################

#Corregimos el departamento de LIMA REGION por LIMA
df_fallecidos$DEPARTAMENTO[df_fallecidos$DEPARTAMENTO == "LIMA REGION"] <- "LIMA" 
df_fallecidos$DEPARTAMENTO[df_fallecidos$DEPARTAMENTO == "LIMA METROPOLITANA"] <- "LIMA" 

unique(df_fallecidos$DEPARTAMENTO)


########################       
#Guardamos bases     
#######################

setwd("C:/Users/egus2/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")

write.csv(df_positivos,'positivos_dep_.csv')
write.csv(df_fallecidos,'fallecidos_dep_.csv')