library(rgdal)
library(broom)
library(rgeos)
library(maptools)
library(ggplot2)
library(viridis)
library(scales)

###############################################################################
#A NIVEL DEPARTAMENTAL
###############################################################################

#Ubicaci?n de los archivos de cartograf?a
dirmapas <- "C:/Users/Admin/Desktop/QLAB/B - Dashboards/QLAB - COVID/Cloropleth Map/DEPARTAMENTO"
setwd(dirmapas)

# El mapa de polígonos en blanco y negro
departamentos<-readOGR(dsn="DEPARTAMENTO_27_04_2015.shp", layer="DEPARTAMENTO_27_04_2015")

summary(departamentos)

departamentos$NOMBDEP

head(departamentos@data)

#plot(departamentos)

#Debemos transformar los datos para poder hacer un gráfico con ggplot
#departamentos_fortified <- tidy(departamentos, region =  "NOMBDEP")
#unique(departamentos_fortified$id)

# Transformar el objeto SpatialPolygonsDataFrame en un objeto fortificado (tidy)
departamentos_fortified <- ggplot2::fortify(departamentos, region =  "NOMBDEP")
# Obtener los valores únicos de id
unique(departamentos_fortified$id)


# importamos los datos que queremos insertar en el mapa
setwd("C:/Users/Admin/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")

dep_stock<- read.csv("covid_departamento_final.csv", header= TRUE, encoding="UTF-8")

#Nos quedamos solo con las columnas id Y CASOS_DEPARTAMENTO
dep_stock$id <- dep_stock$DEPARTAMENTO
#dep_stock <- dep_stock[,c(-1:-4,-6:-10)]

#Hacemos un left join
departamentos_fortified_ <-merge(x = departamentos_fortified, y = dep_stock, by = c("id"), all.x = TRUE)



#Hacemos el gráfico
dep_plot <- ggplot() +
  geom_polygon(data = departamentos_fortified_, aes(fill = CASOS_DEPARTAMENTO/1000, x = long, y = lat, group = group), colour= "gray", size=0.05, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(option = 'D', trans = "log2",  breaks = trans_breaks("log2", function(x) 2^x), name="Casos\n(en miles)") +
  labs(
    title = "Casos positivos de COVID-19 acumulados a nivel departamental",
    subtitle = "desde inicios de pandemia hasta agosto del 2023",
    caption = "Fuente: Elaboración propia basada en MINSA"
  )  +
  theme(  
    text = element_text(color = "#22211d"),
    
    plot.title = element_text(size= 14, hjust=0.01, color = "black", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=10, color = "black", margin = margin(b = 0.3, r=-99, unit = "cm") ),
  ) +
  coord_map()

#dep_plot

ggsave(filename = "map_dep.png", dep_plot, height = 7 , width = 7)

#pie de página mencionando cuál es el valor de lima

###############################################################################
#A NIVEL PROVINCIAL
###############################################################################
dirmapas2 <- "C:/Users/Admin/Desktop/QLAB/B - Dashboards/QLAB - COVID/Cloropleth Map/PROVINCIA" 
setwd(dirmapas2)
provincias<-readOGR(dsn="INEI_LIMITE_PROVINCIAL_196_GEOGPSPERU_JUANSUYO_931381206.shp")
head(provincias@data)

summary(provincias)
length(unique(provincias$IDPROV)) #196

#plot(provincias)

#Debemos transformar los datos para poder hacer un gráfico con ggplot
#provincias_fortified <- tidy(provincias, region =  "IDPROV")
#length(unique(provincias_fortified$id)) #196

provincias_fortified <- ggplot2::fortify(provincias, region =  "IDPROV")
# Obtener los valores únicos de id
unique(provincias_fortified$id)

# importamos los datos que queremos insertar en el mapa
setwd("C:/Users/Admin/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")

prov_stock<- read.csv("covid_provincia_final.csv")

prov_stock$id <- sprintf("%04d", as.numeric(prov_stock$CODIGO))

#Nos quedamos solo con las columnas id Y CASOS.POR.PROVINCIA 
prov_stock <- prov_stock[,c(-1:-6,-8:-12)]

#Hacemos un left join
provincias_fortified_ <-merge(x = provincias_fortified, y = prov_stock, by = c("id"), all.x = TRUE)

#Hacemos el gráfico
prov_plot <- ggplot() +
  geom_polygon(data = provincias_fortified_, aes(fill = CASOS.POR.PROVINCIA/100, x = long, y = lat, group = group), colour = "gray", size=0.05, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(option = 'D', trans = "log2",  breaks = trans_breaks("log2", function(x) 2^x), name="Casos\n(en cientos)") +
  labs(
    title = "Casos positivos de COVID-19 acumulados a nivel provincial",
    subtitle = "desde inicios de pandemia hasta agosto del 2023",
    caption = "Fuente: Elaboración propia basada en MINSA"
  )  +
  theme(  
    text = element_text(color = "#22211d"),
    
    plot.title = element_text(size= 14, hjust=0.01, color = "black", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=10, color = "black", margin = margin(b = 0.3, r=-99, unit = "cm") ),
  ) +
  coord_map()

#prov_plot

ggsave(filename = "map_prov.png", prov_plot, height = 7 , width = 7)

###############################################################################
#A NIVEL DISTRITAL
###############################################################################

dirmapas3 <- "C:/Users/Admin/Desktop/QLAB/B - Dashboards/QLAB - COVID/Cloropleth Map/DISTRITO" 
setwd(dirmapas3)
distritos<-readOGR(dsn="LIMITE_DISTRITAL_2020_INEI_geogpsperu_juansuyo_931381206.shp")
head(distritos@data)

length(unique(distritos$CODIGO)) #1874

#plot(distritos)

#Debemos transformar los datos para poder hacer un gráfico con ggplot
#distritos_fortified <- tidy(distritos, region =  "CODIGO")
#length(unique(distritos_fortified$id)) #1874

distritos_fortified <- ggplot2::fortify(distritos, region =  "CODIGO")
# Obtener los valores únicos de id
unique(distritos_fortified$id)

# importamos los datos que queremos insertar en el mapa
setwd("C:/Users/Admin/Desktop/QLAB/B - Dashboards/QLAB - COVID/Outputs")

dist_stock<- read.csv("covid_distrito_final.csv", header= TRUE, encoding="UTF-8")

dist_stock$id <- sprintf("%06d", as.numeric(dist_stock$CODIGO))

#Nos quedamos solo con las columnas id Y CASOS.POR.DISTRITO 
dist_stock <- dist_stock[,c(-1:-7,-9:-13)]

#Hacemos un left join
distritos_fortified_ <-merge(x = distritos_fortified, y = dist_stock, by = c("id"), all.x = TRUE)

#Hacemos el gráfico
dist_plot <- ggplot() +
  geom_polygon(data = distritos_fortified_, aes(fill = CASOS.POR.DISTRITO, x = long, y = lat, group = group), size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(option = 'D', trans = "log2",  breaks = trans_breaks("log2", function(x) 2^x), name="Casos") +
  labs(
    title = "Casos positivos de COVID-19 acumulados a nivel distrital",
    subtitle = "desde inicios de pandemia hasta agosto del 2023",
    caption = "Fuente: Elaboración propia basada en MINSA"
  )  +
  theme(  
    text = element_text(color = "#22211d"),
    
    plot.title = element_text(size= 14, hjust=0.01, color = "black", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=10, color = "black", margin = margin(b = 0.3, r=-99, unit = "cm") ),
  ) +
  coord_map()

#dist_plot

ggsave(filename = "map_dist.png", dist_plot, height = 7 , width = 7)

