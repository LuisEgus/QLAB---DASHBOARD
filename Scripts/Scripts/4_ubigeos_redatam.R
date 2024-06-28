library(tidyverse)
library(dplyr)
library(readxl)
library(splitstackshape)
library(stringi)

setwd("C:/Users/egus2/Desktop/QLAB/B - Dashboards/QLAB - COVID/Data")

###############################################################################
#IMPORTAMOS DATOS
###############################################################################

df1<- read.csv("ubigeos1.csv", header= TRUE, encoding="UTF-8")
df2<- read_excel("poblacion_distrito.xlsx")
df3<- read_excel("poblacion_departamento.xlsx")
df4<- read_excel("poblacion_provincia.xlsx")

ubigeos <- df1
ubigeos2 <- df1
pob_distrito <- df2
pob_departamento <- df3
pob_provincia <- df4


###############################################################################
#TRABAJAMOS BASES
###############################################################################

#Trabajamos con la base UBIGEOS

#PARA DISTRITOS
nrow(ubigeos) #1874

colnames(ubigeos) <- c("dep_inei", "prov_inei", "dist_inei", "ubigeo_inei", "dep_reniec", "prov_reniec", "dist_reniec", "ubigeo_reniec")

#Cambios para Callao
ubigeos[690:696,]

ubigeos$dep_reniec <- gsub('PROV. CONST. DEL CALLAO', 'CALLAO', ubigeos$dep_reniec)

#generamos una LLAVE que estará conformada por el DEPARTAMENTO, PROVINCIA Y DISTRITO del registro del RENIEC
ubigeos$LLAVE_reniec <- paste0(ubigeos$dep_reniec, ubigeos$prov_reniec, ubigeos$dist_reniec)

#Quitamos tildes y caracteres extraños de la LLAVE_reniec 
ubigeos$LLAVE_reniec <- iconv(ubigeos$LLAVE_reniec, from="UTF-8", to="ASCII//TRANSLIT")

#Quitamos espacios en la llave
ubigeos$LLAVE_reniec <- gsub(" ", "", ubigeos$LLAVE_reniec) 

#PARA PROVINCIAS
nrow(ubigeos2) #1874

#Nos quedamos con las columnas de interés
ubigeos2 <- ubigeos2[,c(1:2,4:6,8)] 
colnames(ubigeos2) <- c("dep_inei", "prov_inei", "ubigeo_inei", "dep_reniec", "prov_reniec", "ubigeo_reniec")

#ubigeo a nivel provincial
ubigeos2$ubigeo_inei <- stri_sub(as.character(ubigeos2$ubigeo_inei), -6,-3)
ubigeos2$ubigeo_reniec <- stri_sub(as.character(ubigeos2$ubigeo_reniec), -6,-3)

#Cambios para Callao
ubigeos2[690:696,]

ubigeos2$dep_reniec <- gsub('PROV. CONST. DEL CALLAO', 'CALLAO', ubigeos2$dep_reniec)

#generamos una LLAVE que estará conformada por el DEPARTAMENTO Y PROVINCIA
ubigeos2$LLAVE_reniec <- paste0(ubigeos2$dep_reniec, ubigeos2$prov_reniec)

#Quitamos tildes y caracteres extraños de la LLAVE_reniec 
ubigeos2$LLAVE_reniec <- iconv(ubigeos2$LLAVE_reniec, from="UTF-8", to="ASCII//TRANSLIT")

#Quitamos espacios en la llave
ubigeos2$LLAVE_reniec <- gsub(" ", "", ubigeos2$LLAVE_reniec) 


#Trabajamos con la base POB_DISTRITO

pob_distrito <- pob_distrito[,-1] #Delete first column

pob_distrito <- na.omit(pob_distrito) #omit all rows that contain NA values

pob_distrito <- pob_distrito[,c(1,3)]

colnames(pob_distrito) = c("ubigeo_inei", "POBLACION")

pob_distrito <- pob_distrito[-1,] #Delete first row

#Trabajamos con la base POB_PROVINCIA

pob_provincia <- pob_provincia[,-1] #Delete first column

pob_provincia <- na.omit(pob_provincia) #omit all rows that contain NA values

pob_provincia <- pob_provincia[,c(1,3)]

colnames(pob_provincia) = c("ubigeo_inei", "POBLACION")

pob_provincia <- pob_provincia[-1,] #Delete first row

#Trabajamos con la base POB_DEPARTAMENTO

pob_departamento <- pob_departamento[,-1] #Delete first column

pob_departamento <- na.omit(pob_departamento) #omit all rows that contain NA values

pob_departamento <- pob_departamento[,c(2,3)]

colnames(pob_departamento) = c("DEPARTAMENTO", "POBLACION")

pob_departamento <- pob_departamento[-1,] #Delete first row  

#Quitamos tildes y caracteres extraños,  y hacemos que la columna DEPARTAMENTO esta en mayuscula
pob_departamento$DEPARTAMENTO <- iconv(pob_departamento$DEPARTAMENTO, from="UTF-8", to="ASCII//TRANSLIT")
pob_departamento$DEPARTAMENTO <- toupper(pob_departamento$DEPARTAMENTO)

#Cambios para Callao
pob_departamento$DEPARTAMENTO <- gsub('PROVINCIA CONSTITUCIONAL DEL CALLAO', 'DEPARTAMENTO: CALLAO', pob_departamento$DEPARTAMENTO)

#En la columna DEPARTAMENTO, tenemos que eliminar el "DEPARTAMENTO: "
pob_departamento$DEPARTAMENTO <- gsub('DEPARTAMENTO: ', '', pob_departamento$DEPARTAMENTO)



###############################################################################
# A NIVEL DISTRITAL
###############################################################################

#Unimos las dos bases : UBIGEOS y POB_DISTRITO

pob_distrito_ubigeo <-merge(x = pob_distrito, y = ubigeos, by = c("ubigeo_inei"))
nrow(pob_distrito_ubigeo) #1874

pob_distrito_ubigeo <- pob_distrito_ubigeo[,c(1,9,3:5,2,10)] 

colnames(pob_distrito_ubigeo) <- c("ubigeo_inei", "ubigeo_reniec", "DEPARTAMENTO", "PROVINCIA", "DISTRITO", "POBLACION", "LLAVE")


########################       
#Guardamos bases     
#######################

setwd("C:/Users/egus2/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")
write.csv(pob_distrito_ubigeo,'poblacion_distrito_final.csv')


###############################################################################
# A NIVEL PROVINCIAL
###############################################################################

#Unimos las dos bases : UBIGEOS2 y POB_PROVINCIA

pob_provincia_ubigeo <-merge(x = pob_provincia, y = ubigeos2, by = c("ubigeo_inei"))

#Eliminamos las provincias duplicadas
pob_provincia_ubigeo <- subset(pob_provincia_ubigeo, !duplicated(subset(pob_provincia_ubigeo, select=c(ubigeo_inei))))
nrow(pob_provincia_ubigeo) 
length(unique(pob_provincia_ubigeo$ubigeo_inei)) #196 provincias

pob_provincia_ubigeo <- pob_provincia_ubigeo[,c(1,7,3:4,2,8)] 

colnames(pob_provincia_ubigeo) <- c("ubigeo_inei", "ubigeo_reniec", "DEPARTAMENTO", "PROVINCIA", "POBLACION", "LLAVE")


########################       
#Guardamos bases     
#######################

setwd("C:/Users/egus2/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")
write.csv(pob_provincia_ubigeo,'poblacion_provincia_final.csv')

###############################################################################
# A NIVEL DEPARTAMENTAL
###############################################################################

########################       
#Guardamos bases     
#######################

setwd("C:/Users/egus2/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")
write.csv(pob_departamento,'poblacion_departamento_final.csv')
