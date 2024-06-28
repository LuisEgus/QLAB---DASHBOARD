library(tidyverse)
library(dplyr)
library(zoo)


setwd("C:/Users/Admin/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")


#IMPORTAMOS DATOS
df1<- read.table("positivos_dep_.csv", header= TRUE, sep = ",", encoding="UTF-8")

df2<-read.table("fallecidos_dep_.csv", header= TRUE, sep= ",", encoding="UTF-8")

df3<- read.table("fallecidos_sinadef.csv", header= TRUE, sep= ",", encoding="UTF-8")

df1 <- df1[,-1] #Delete first column
df2 <- df2[,-1]
df3 <- df3[,-1]

#Solo nos interesa las columnas DEPARTAMENTO y FECHA_MUERTE
df3 <- df3[,c("DEPARTAMENTO","FECHA_MUERTE")]     

lapply(df3,class)

###############################################################################

#BASE PANEL PARA POSITIVOS (CANTIDAD DE POSITIVOS POR DISTRITO MENSUALMENTE)

##############################################################################

positivos_panel <- df1
nrow(positivos_panel)
#1385434 / 1637744
head(positivos_panel)


length(unique(positivos_panel$DEPARTAMENTO)) #25 departamentos (24 + CALLAO)


#Solo nos quedamos con el año y mes (omitimos el dia)
positivos_panel$FECHA_RESULTADO <- substr(positivos_panel$FECHA_RESULTADO,1,6)


#Agrupamos los datos por departamento y fecha de resultado
positivos_panel <- data.frame(table( positivos_panel$DEPARTAMENTO, positivos_panel$FECHA_RESULTADO))


#Ordamos los datos primero por departamento y luego por fecha
positivos_panel <- positivos_panel[ order( positivos_panel$Var1 , positivos_panel$Var2 ) , ]  

length(unique(positivos_panel$Var1)) #25 departamentos (24 + CALLAO)

colnames(positivos_panel) = c("DEPARTAMENTO", "FECHA", "CASOS")

nrow(positivos_panel)
#325( 25 distritos x 13 meses) / 350 (25x14)

sum(positivos_panel$CASOS)
#1385434 / 1637744 no se perdieron casos

###############################################################################

#BASE PANEL PARA FALLECIDOS (CANTIDAD DE FALLECIDOS POR DISTRITO MENSUALMENTE)

##############################################################################

fallecidos_panel <- df2
head(fallecidos_panel)
nrow(fallecidos_panel)
#48323 / 54285


length(unique(fallecidos_panel$DEPARTAMENTO)) #25 departamentos (24 + CALLAO)


#Solo nos quedamos con el año y mes (omitimos el dia)
fallecidos_panel$FECHA_FALLECIMIENTO <- substr(fallecidos_panel$FECHA_FALLECIMIENTO,1,6)


#Agrupamos los datos por departamento y fecha de fallecimiento
fallecidos_panel <- data.frame(table( fallecidos_panel$DEPARTAMENTO, fallecidos_panel$FECHA_FALLECIMIENTO))

head(fallecidos_panel)

#Ordamos los datos primero por departamento y luego por fecha
fallecidos_panel <- fallecidos_panel[ order( fallecidos_panel$Var1 , fallecidos_panel$Var2 ) , ]  

length(unique(fallecidos_panel$Var1)) #25


colnames(fallecidos_panel) = c("DEPARTAMENTO", "FECHA", "FALLECIDOS")

nrow(fallecidos_panel)
#325( 25 distritos x 13 meses) / 350(25x14)

sum(fallecidos_panel$FALLECIDOS)
#48323 / 54285 no se perdieron casos


###############################################################################

#BASE PANEL PARA FALLECIDOS SINADEF (CANTIDAD DE FALLECIDOS POR DISTRITO MENSUALMENTE)  

##############################################################################

sinadef_panel <- df3
head(sinadef_panel)


#Nos quedamos con las muertes que hayan sido registradas desde el 2020 de marzo hacia adelante
sinadef_panel <- subset(sinadef_panel, sinadef_panel$FECHA_MUERTE >= 202003)
nrow(sinadef_panel)
#276103 / 287728

length(unique(sinadef_panel$DEPARTAMENTO)) #25 departamentos (24 + CALLAO)


#Agrupamos los datos por departamento y fecha de muerte
sinadef_panel <- data.frame(table( sinadef_panel$DEPARTAMENTO, sinadef_panel$FECHA_MUERTE))

head(sinadef_panel)

#Ordamos los datos primero por departamento y luego por fecha
sinadef_panel <- sinadef_panel[ order(sinadef_panel$Var1 , sinadef_panel$Var2 ) , ]  

length(unique(sinadef_panel$Var1)) #25


colnames(sinadef_panel) = c("DEPARTAMENTO", "FECHA", "FALLECIDOS_SINADEF")

nrow(sinadef_panel)
#325( 25 distritos x 13 meses) / 350

sum(sinadef_panel$FALLECIDOS)
#276103/ 287728 no se perdieron casos


###############################################################################

#CRUZAMOS LAS BASES 

###############################################################################

##positivos panel y fallecidos panel
departamento_panel <-merge(x = positivos_panel, y = fallecidos_panel, by = c("DEPARTAMENTO","FECHA"))


nrow(departamento_panel)
#325( 25 distritos x 13 meses) / 350

nrow(departamento_panel[rowSums(is.na(departamento_panel)) > 0,])
# 0 filas con NA

sum(is.na(departamento_panel$CASOS))
#0 con NA en CASOS 
sum(is.na(departamento_panel$FALLECIDOS))
#0 con NA en FALLECIDOS


##departamento_panel y sinadef_panel
departamento_panel1 <-merge(x = departamento_panel, y = sinadef_panel, by = c("DEPARTAMENTO","FECHA"))

nrow(departamento_panel1)
#475( 25 distritos x 19 meses)

nrow(departamento_panel1[rowSums(is.na(departamento_panel1)) > 0,])
# 0 filas con NA

sum(is.na(departamento_panel1$CASOS))
#0 con NA en CASOS 
sum(is.na(departamento_panel1$FALLECIDOS))
#0 con NA en FALLECIDOS
sum(is.na(departamento_panel1$FALLECIDOS_SINADEF))
#0 con NA en FALLECIDOS_SINADEF


##################################################
# Guardamos base
################################################## 

setwd("C:/Users/Admin/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")

write.csv(departamento_panel1,'departamento_panel_.csv')