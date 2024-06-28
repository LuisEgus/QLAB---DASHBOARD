library(tidyverse)
library(dplyr)
library(zoo)


setwd("C:/Users/egus2/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")


#IMPORTAMOS DATOS
df1 <- read.table("positivos_.csv", header= TRUE, sep = ",", encoding="UTF-8")

df2 <- read.table("fallecidos_.csv", header= TRUE, sep= ",", encoding="UTF-8")

df3 <- read.table("poblacion_distrito_final.csv", header= TRUE, sep= ",", encoding="LATIN1")

df5 <- read.table("fallecidos_sinadef.csv", header= TRUE, sep= ",", encoding="UTF-8")

df1 <- df1[,-1] #Delete first column
df2 <- df2[,-1]
df3 <- df3[,-1]
df5 <- df5[,-1]

df5 <- df5[,c("ubigeo_inei","FECHA_MUERTE")]    

df4 <- df3

###############################################################################

#BASE PANEL PARA POSITIVOS (CANTIDAD DE POSITIVOS POR DISTRITO MENSUALMENTE)

##############################################################################

positivos_panel_ <- df1
nrow(positivos_panel_)
#1314211 / 1552427
head(positivos_panel_)
colnames(positivos_panel_) <- c("ubigeo_inei", "FECHA_RESULTADO")

sum(is.na(positivos_panel_$ubigeo_inei))  # 0 NA en ubigeo_inei
length(unique(positivos_panel_$ubigeo_inei)) #1874 distritos

#Solo nos quedamos con el año y mes (omitimos el d?a)
positivos_panel_$FECHA_RESULTADO <- substr(positivos_panel_$FECHA_RESULTADO,1,6)


#Agrupamos los datos por distrito y fecha de resultado
positivos_panel_ <- data.frame(table( positivos_panel_$ubigeo_inei, positivos_panel_$FECHA_RESULTADO))

head(positivos_panel_,30)

#Ordamos los datos primero por distrito y luego por fecha
positivos_panel_ <- positivos_panel_[ order( positivos_panel_$Var1 , positivos_panel_$Var2 ) , ]  

length(unique(positivos_panel_$Var1)) #1874 distritos

colnames(positivos_panel_) = c("ubigeo_inei", "FECHA", "CASOS")

nrow(positivos_panel_)
#24362 ( 1874 distritos x 13 meses) / 26236 ( 1874 distritos x 14 meses)

sum(positivos_panel_$CASOS)
#1314170  / se perdieron 1314211 - 1314170 = 41 casos 
#1552374  / se perdieron 1552427- 1552374  = 53 casos


###############################################################################

#BASE PANEL PARA FALLECIDOS (CANTIDAD DE FALLECIDOS POR DISTRITO MENSUALMENTE)

##############################################################################

fallecidos_panel_ <- df2
head(fallecidos_panel_)
nrow(fallecidos_panel_)
#48323 / 54285
colnames(fallecidos_panel_) <- c("ubigeo_inei", "FECHA_FALLECIMIENTO")

sum(is.na(fallecidos_panel_$ubigeo_inei))  # 0 NA en codigo
length(unique(fallecidos_panel_$ubigeo_inei)) #1874 distritos


#Solo nos quedamos con el año y mes (omitimos el dia)
fallecidos_panel_$FECHA_FALLECIMIENTO <- substr(fallecidos_panel_$FECHA_FALLECIMIENTO,1,6)


#Agrupamos los datos por distrito y fecha de fallecimiento
fallecidos_panel_ <- data.frame(table( fallecidos_panel_$ubigeo_inei, fallecidos_panel_$FECHA_FALLECIMIENTO))

head(fallecidos_panel_,13)

#Ordamos los datos primero por distrito y luego por fecha
fallecidos_panel_ <- fallecidos_panel_[ order( fallecidos_panel_$Var1 , fallecidos_panel_$Var2 ) , ]  

length(unique(fallecidos_panel_$Var1)) #1874 distritos

colnames(fallecidos_panel_) = c("ubigeo_inei", "FECHA", "FALLECIDOS")

nrow(fallecidos_panel_)
#24362  ( 1874 distritos x 13 meses) / 26236

sum(fallecidos_panel_$FALLECIDOS)
#47067 / se perdieron 48323 - 47067 = 1256 fallecidos
#53689/ se perdieron 54285 - 53689  = 596 fallecidos


###############################################################################

#BASE PANEL PARA FALLECIDOS SINAFEF (CANTIDAD DE FALLECIDOS POR DISTRITO MENSUALMENTE)

##############################################################################

sinadef_panel <- df5
head(sinadef_panel)

#Nos quedamos con las muertes que se haya registrado desde marzo del 2020 
sinadef_panel <- subset(sinadef_panel, sinadef_panel$FECHA_MUERTE >= 202003)

nrow(sinadef_panel) #275582/ 287728

sum(is.na(sinadef_panel$ubigeo_inei))  # 0 NA en codigo

length(unique(sinadef_panel$ubigeo_inei))  #1853 / 1855 distritos. No llegan a los 1874 porque la RENIEC tiene menos distritos registrados en su ubigeo
#Recordar que el registro del SINADEF está en base del ubigeo de la RENIEC

#Agrupamos los datos por distrito y fecha de fallecimiento
sinadef_panel <- data.frame(table( sinadef_panel$ubigeo_inei, sinadef_panel$FECHA_MUERTE))

head(sinadef_panel,13)

#Ordamos los datos primero por distrito y luego por fecha
sinadef_panel <- sinadef_panel[ order( sinadef_panel$Var1 , sinadef_panel$Var2 ) , ]  

length(unique(sinadef_panel$Var1)) #1853 / 1855 distritos

colnames(sinadef_panel) = c("ubigeo_inei", "FECHA", "FALLECIDOS_SINADEF")

nrow(sinadef_panel)
#24089  ( 1853  distritos x 13 meses) / 25970 (1855 distritos x 14 meses)

sum(sinadef_panel$FALLECIDOS_SINADEF)
#275582 / 287728


###############################################################################

#CRUZAMOS LAS BASES 

###############################################################################

#positivos panel y fallecidos panel

distrito_panel <-merge(x = positivos_panel_, y = fallecidos_panel_, by = c("ubigeo_inei","FECHA"))


nrow(distrito_panel)
#24362  ( 1874 distritos x 13 meses) / 26236

nrow(distrito_panel[rowSums(is.na(distrito_panel)) > 0,])
# 0 filas con NA

sum(is.na(distrito_panel$CASOS))
#0 con NA en CASOS 
sum(is.na(distrito_panel$FALLECIDOS))
#0 con NA en FALLECIDOS

length(unique(distrito_panel$ubigeo_inei))
#1874 distritos
sum(distrito_panel$CASOS) #1314170 / 1552374
sum(distrito_panel$FALLECIDOS) #47067 / 53689


#distrito_panel y sinadef_panel

#arreglamos el ubigeo para que tenga 6 dígitos
sinadef_panel$ubigeo_inei <- stringr::str_pad(sinadef_panel$ubigeo_inei, 6, side = "left", pad = 0)

distrito_panel1 <-merge(x = distrito_panel, y = sinadef_panel, by = c("ubigeo_inei","FECHA"), all.x = TRUE)

nrow(distrito_panel1)
#24362  ( 1874 distritos x 13 meses) / 26236

nrow(distrito_panel1[rowSums(is.na(distrito_panel1)) > 0,])
# 273 filas con NA / 266

sum(is.na(distrito_panel1$CASOS))
#0 con NA en CASOS 
sum(is.na(distrito_panel1$FALLECIDOS))
#0 con NA en FALLECIDOS
sum(is.na(distrito_panel1$FALLECIDOS_SINADEF))
# 273 filas con NA / 266

length(unique(distrito_panel1$ubigeo_inei))
#1874 distritos
sum(distrito_panel1$CASOS) #1314170 / 1552374
sum(distrito_panel1$FALLECIDOS) #47067 / 53689
sum(distrito_panel1$FALLECIDOS_SINADEF) #275582 /287728 (hice el calculo en Excel)

#Eliminamos las columnas con NA
distrito_panel1 <- distrito_panel1[complete.cases(distrito_panel1), ]

##################################################
# Le pegamos los nombres del DEPARTAMENTO, PROVINCIA Y DISTRITO
################################################## 

df4 <- df4[ ,c("ubigeo_inei", "DEPARTAMENTO", "PROVINCIA", "DISTRITO")]

#arreglamos el ubigeo para que tenga 6 dígitos
df4$ubigeo_inei <- stringr::str_pad(df4$ubigeo_inei, 6, side = "left", pad = 0)        

distrito_panel_ <-merge(x = distrito_panel1 , y = df4 , by = c("ubigeo_inei"))

head(distrito_panel_)
nrow(distrito_panel_)
#24362  ( 1874 distritos x 13 meses) / 26236

distrito_panel_ <- distrito_panel_[ , c("ubigeo_inei", "DEPARTAMENTO", "PROVINCIA", "DISTRITO", "FECHA", "CASOS", "FALLECIDOS", "FALLECIDOS_SINADEF")]


##################################################
# Guardamos base
################################################## 

setwd("C:/Users/egus2/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")

write.csv(distrito_panel_,'distrito_panel_.csv')

