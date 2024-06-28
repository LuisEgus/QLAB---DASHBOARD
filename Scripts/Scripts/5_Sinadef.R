library(tidyverse)
library(dplyr)
library(stringi)

setwd("C:/Users/egus2/Desktop/QLAB/B - Dashboards/QLAB - COVID/Data")

###############################################################################
#IMPORTAMOS DATOS
###############################################################################

df1 <- read.csv("fallecidos_sinadef.csv", encoding="UTF-8", sep="|")
#df1 <- read.delim2("fallecidos_sinadef.csv", encoding="UTF-8", sep="|")

df2 <- read.csv("ubigeos1.csv", header= TRUE, encoding="UTF-8")

###############################################################################
#TRABAJAMOS BASES
###############################################################################

#Trabajamos con la base UBIGEOS
ubigeos <- df2

nrow(ubigeos) #1874

colnames(ubigeos) <- c("dep_inei", "prov_inei", "dist_inei", "ubigeo_inei", "dep_reniec", "prov_reniec", "dist_reniec", "ubigeo_reniec")

#Cambios para Callao
ubigeos[690:696,]

ubigeos$dep_reniec <- gsub('PROV. CONST. DEL CALLAO', 'CALLAO', ubigeos$dep_reniec)

#PARA NIVEL DISTRITAL
#generamos una LLAVE que estarC! conformada por el DEPARTAMENTO, PROVINCIA Y DISTRITO del registro del RENIEC
ubigeos$LLAVE_reniec <- paste0(ubigeos$dep_reniec, ubigeos$prov_reniec, ubigeos$dist_reniec)

#Quitamos tildes y caracteres extraC1os de la LLAVE_reniec 
ubigeos$LLAVE_reniec <- iconv(ubigeos$LLAVE_reniec, from="UTF-8", to="ASCII//TRANSLIT")

#Quitamos espacios en la llave
ubigeos$LLAVE_reniec <- gsub(" ", "", ubigeos$LLAVE_reniec) 

#PARA NIVEL PROVINCIAL
ubigeos$ubigeo_inei2 <- stri_sub(as.character(ubigeos$ubigeo_inei), -6,-3)
ubigeos$ubigeo_reniec2 <- stri_sub(as.character(ubigeos$ubigeo_reniec), -6,-3)

#generamos una LLAVE que estarC! conformada por el DEPARTAMENTO Y PROVINCIA
ubigeos$LLAVE_reniec2 <- paste0(ubigeos$dep_reniec, ubigeos$prov_reniec)

#Quitamos tildes y caracteres extraC1os de la LLAVE_reniec 
ubigeos$LLAVE_reniec2 <- iconv(ubigeos$LLAVE_reniec2, from="UTF-8", to="ASCII//TRANSLIT")

#Quitamos espacios en la llave
ubigeos$LLAVE_reniec2 <- gsub(" ", "", ubigeos$LLAVE_reniec2) 


ubigeos_ <- ubigeos[,c(4,8,9,10:12)]  #ubigeo_inei, ubigeo_reniec, LLAVE_reniec, ubigeo_inei2, ubigeo_reniec2, LLAVE_reniec2


#Trabajamos con la base fallecidos      

#Nos quedamos con las columnas COD..UBIGEO.DOMICILIO, DEPARTAMENTO, PROVINCIA, DISTRITO, AÑO, MES 
fallecidos <- df1[,c("COD..UBIGEO.DOMICILIO", "DEPARTAMENTO.DOMICILIO", "PROVINCIA.DOMICILIO","DISTRITO.DOMICILIO", "AÑO", "MES")]  
colnames(fallecidos)<-c("COD..UBIGEO.DOMICILIO", "DEPARTAMENTO.DOMICILIO", "PROVINCIA.DOMICILIO","DISTRITO.DOMICILIO", "AÑO", "MES")

#Nos quedamos con los fallecidos del 2019 para adelante
fallecidos <- subset(fallecidos, fallecidos$AÑO >= 2019)

#Retiramos de la base a los que fallecieron en el extranjero y a los sin registro
fallecidos <- subset(fallecidos, fallecidos$DEPARTAMENTO.DOMICILIO != "EXTRANJERO" & fallecidos$DEPARTAMENTO.DOMICILIO != "SIN REGISTRO")

#Nos quedamos solo con los codigos del departamento, provincia y distrito
fallecidos$COD..UBIGEO.DOMICILIO <- gsub("-", "", fallecidos$COD..UBIGEO.DOMICILIO) 
fallecidos$COD..UBIGEO.DOMICILIO <- substr(fallecidos$COD..UBIGEO.DOMICILIO,5,10)

#Arreglamos el MES
fallecidos$MES <- sprintf("%02d", as.numeric(fallecidos$MES ))

#Creamos una variable que tenga AÑO y MES
fallecidos$FECHA_MUERTE <- paste0( fallecidos$AÑO, fallecidos$MES )

#PARA NIVEL DISTRITAL
#Creamos una LLAVE
fallecidos$LLAVE <- paste0(fallecidos$DEPARTAMENTO.DOMICILIO, fallecidos$PROVINCIA.DOMICILIO, fallecidos$DISTRITO.DOMICILIO)

#Quitamos tildes y caracteres extraC1os de la LLAVE_reniec 
fallecidos$LLAVE <- iconv(fallecidos$LLAVE, from="UTF-8", to="ASCII//TRANSLIT")

#quitamos espacios
fallecidos$LLAVE <- gsub(" ", "", fallecidos$LLAVE) 

#PARA NIVEL PROVINCIAL
#Creamos una LLAVE
fallecidos$LLAVE2 <- paste0(fallecidos$DEPARTAMENTO.DOMICILIO, fallecidos$PROVINCIA.DOMICILIO)

#Quitamos tildes y caracteres extraC1os de la LLAVE_reniec 
fallecidos$LLAVE2 <- iconv(fallecidos$LLAVE2, from="UTF-8", to="ASCII//TRANSLIT")

#quitamos espacios
fallecidos$LLAVE2 <- gsub(" ", "", fallecidos$LLAVE2) 


fallecidos <- fallecidos[,c(1,2,7,8:9)]  
colnames(fallecidos) <- c("ubigeo_reniec", "DEPARTAMENTO", "FECHA_MUERTE", "LLAVE_reniec", "LLAVE_reniec2")

nrow(fallecidos) #422805


#A la base le agregamos una columna con el ubigeo_inei

fallecidos_sinadef <-merge(x = fallecidos, y = ubigeos_, by = c("LLAVE_reniec", "LLAVE_reniec2"))

nrow(fallecidos_sinadef)
#421762
#Se perdieron 422805 - 421762 = 1043 registros

length(unique(fallecidos_sinadef$ubigeo_inei)) #1865 distritos . No llegan a los 1874 porque la RENIEC tiene menos distritos registrados en su ubigeo



fallecidos_sinadef <-  fallecidos_sinadef[,c(6,4,5,1,8,2)] #ubigeo_inei, DEPARTAMENTO, FECHA_MUERTE, LLAVE_reniec, ubigeo_inei2, LLAVE_reniec2

########################       
#Guardamos bases     
#######################

setwd("C:/Users/egus2/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")

write.csv(fallecidos_sinadef,"fallecidos_sinadef.csv")

