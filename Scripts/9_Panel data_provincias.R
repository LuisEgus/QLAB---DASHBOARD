library(tidyverse)
library(dplyr)
library(zoo)

setwd("C:/Users/Admin/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")


#IMPORTAMOS DATOS
df1 <- read.table("positivos_prov_.csv", header= TRUE, sep = ",", encoding="UTF-8")

df2 <- read.table("fallecidos_prov_.csv", header= TRUE, sep= ",", encoding="UTF-8")

df3 <- read.table("poblacion_provincia_final.csv", header= TRUE, sep= ",", encoding="LATIN1")

df5 <- read.table("fallecidos_sinadef.csv", header= TRUE, sep= ",", encoding="UTF-8")

df1 <- df1[,-1] #Delete first column
df2 <- df2[,-1]
df3 <- df3[,-1]
df5 <- df5[,-1]

df5 <- df5[,c("ubigeo_inei2","FECHA_MUERTE")]    
colnames(df5)<-c("ubigeo_inei","FECHA_MUERTE")

df4 <- df3

###############################################################################

#BASE PANEL PARA POSITIVOS (CANTIDAD DE POSITIVOS POR PROVINCIA MENSUALMENTE)

##############################################################################

positivos_panel_ <- df1
nrow(positivos_panel_)
#1552427
head(positivos_panel_)
colnames(positivos_panel_) <- c("ubigeo_inei", "FECHA_RESULTADO")


sum(is.na(positivos_panel_$ubigeo_inei))  # 0 NA en ubigeo_inei
length(unique(positivos_panel_$ubigeo_inei)) #196 provincias


#Solo nos quedamos con el año y mes (omitimos el dia)
positivos_panel_$FECHA_RESULTADO <- substr(positivos_panel_$FECHA_RESULTADO,1,6)


#Agrupamos los datos por provincia y fecha de resultado
positivos_panel_ <- data.frame(table( positivos_panel_$ubigeo_inei, positivos_panel_$FECHA_RESULTADO))

head(positivos_panel_,30)

#Ordamos los datos primero por distrito y luego por fecha
positivos_panel_ <- positivos_panel_[ order( positivos_panel_$Var1 , positivos_panel_$Var2 ) , ]  

length(unique(positivos_panel_$Var1)) #196 provincias

nrow(positivos_panel_)
#2744 ( 196 x 14 meses)

colnames(positivos_panel_) = c("ubigeo_inei", "FECHA", "CASOS")

sum(positivos_panel_$CASOS)
#1552427

###############################################################################

#BASE PANEL PARA FALLECIDOS (CANTIDAD DE FALLECIDOS POR PROVINCIA MENSUALMENTE)

##############################################################################

fallecidos_panel_ <- df2
head(fallecidos_panel_)
nrow(fallecidos_panel_)
#54285
colnames(fallecidos_panel_) <- c("ubigeo_inei", "FECHA_FALLECIMIENTO")


sum(is.na(fallecidos_panel_$ubigeo_inei))  # 0 NA en codigo
length(unique(fallecidos_panel_$ubigeo_inei)) #196 provincias


#Solo nos quedamos con el año y mes (omitimos el dia)
fallecidos_panel_$FECHA_FALLECIMIENTO <- substr(fallecidos_panel_$FECHA_FALLECIMIENTO,1,6)


#Agrupamos los datos por provincia y fecha de fallecimiento
fallecidos_panel_ <- data.frame(table( fallecidos_panel_$ubigeo_inei, fallecidos_panel_$FECHA_FALLECIMIENTO))

head(fallecidos_panel_,13)

#Ordamos los datos primero por distrito y luego por fecha
fallecidos_panel_ <- fallecidos_panel_[ order( fallecidos_panel_$Var1 , fallecidos_panel_$Var2 ) , ]  

length(unique(fallecidos_panel_$Var1)) #196

colnames(fallecidos_panel_) = c("ubigeo_inei", "FECHA", "FALLECIDOS")

nrow(fallecidos_panel_)
#2744 ( 196 x 14 meses)

sum(fallecidos_panel_$FALLECIDOS)
#54042


###############################################################################

#BASE PANEL PARA FALLECIDOS SINAFEF (CANTIDAD DE FALLECIDOS POR PROVINCIA MENSUALMENTE)

##############################################################################

sinadef_panel <- df5
head(sinadef_panel)

#Nos quedamos con las muertes que se haya registrado desde marzo del 2020 
sinadef_panel <- subset(sinadef_panel, sinadef_panel$FECHA_MUERTE >= 202003)

nrow(sinadef_panel) #287728

sum(is.na(sinadef_panel$ubigeo_inei))  # 0 NA en codigo

length(unique(sinadef_panel$ubigeo_inei))  #196

#Agrupamos los datos por provincia y fecha de fallecimiento
sinadef_panel <- data.frame(table( sinadef_panel$ubigeo_inei, sinadef_panel$FECHA_MUERTE))

head(sinadef_panel,13)

#Ordamos los datos primero por provincia y luego por fecha
sinadef_panel <- sinadef_panel[ order( sinadef_panel$Var1 , sinadef_panel$Var2 ) , ]  

length(unique(sinadef_panel$Var1)) #196 provincias

colnames(sinadef_panel) = c("ubigeo_inei", "FECHA", "FALLECIDOS_SINADEF")

nrow(sinadef_panel)
#2744 ( 196 x 14 meses)

sum(sinadef_panel$FALLECIDOS_SINADEF)
#287728


###############################################################################

#CRUZAMOS LAS BASES 

###############################################################################

#positivos panel y fallecidos panel

provincia_panel <-merge(x = positivos_panel_, y = fallecidos_panel_, by = c("ubigeo_inei","FECHA"))


nrow(provincia_panel)
#3724 ( 196 x 19 meses)

nrow(provincia_panel[rowSums(is.na(provincia_panel)) > 0,])
# 0 filas con NA

sum(is.na(provincia_panel$CASOS))
#0 con NA en CASOS 
sum(is.na(provincia_panel$FALLECIDOS))
#0 con NA en FALLECIDOS

length(unique(provincia_panel$ubigeo_inei))
#196 provincias
sum(provincia_panel$CASOS) #1552427
sum(provincia_panel$FALLECIDOS) #54042


#provincia_panel y sinadef_panel

provincia_panel1 <-merge(x = provincia_panel, y = sinadef_panel, by = c("ubigeo_inei","FECHA"), all.x = TRUE)

nrow(provincia_panel1)
#3724 ( 196 x 19 meses)

nrow(provincia_panel1[rowSums(is.na(provincia_panel1)) > 0,])
# 0 filas con NA

sum(is.na(provincia_panel1$CASOS))
#0 con NA en CASOS 
sum(is.na(provincia_panel1$FALLECIDOS))
#0 con NA en FALLECIDOS
sum(is.na(provincia_panel1$FALLECIDOS_SINADEF))
#0 filas con NA 

length(unique(provincia_panel1$ubigeo_inei))
#196 provincias
sum(provincia_panel1$CASOS) #1552427
sum(provincia_panel1$FALLECIDOS) #54042
sum(provincia_panel1$FALLECIDOS_SINADEF) #287728 


##################################################
# Le pegamos los nombres del DEPARTAMENTO Y PROVINCIA
################################################## 

df4 <- df4[ ,c("ubigeo_inei","DEPARTAMENTO","PROVINCIA")]

provincia_panel_ <-merge(x = provincia_panel1 , y = df4 , by = c("ubigeo_inei"))

head(provincia_panel_)
nrow(provincia_panel_)
#2744 ( 196 x 14 meses)

provincia_panel_ <- provincia_panel_[ , c("ubigeo_inei", "DEPARTAMENTO", "PROVINCIA", "FECHA", "CASOS", "FALLECIDOS", "FALLECIDOS_SINADEF")]


##################################################
# Guardamos base
################################################## 

setwd("C:/Users/Admin/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")

write.csv(provincia_panel_,'provincia_panel_.csv')