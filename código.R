# Librerías utilizadas

library(tidyverse) # Paquete para manipulación y consulta.
library(lubridate)
library(highcharter)
library(readxl) #Paquete para lectura de datos.
library(ggplot2)
library(htmlwidgets)
library(leaflet)
library(rgdal)
library(rjson)

library(treemap)
library(extrafont)
source("functions.R", encoding = 'UTF-8')


tipovar <- c("text", "text", "date" ,"date"
) # Especificar tipo de variables del Dataset


#colores de las series

col <-   c( "#8cc63f", # verde
            "#f15a24", # naranja
            "#0071bc", # azul vivo
            "#6d6666", # gris
            "#fbb03b", # amarillo
            "#93278f", # morado
            "#29abe2", # azul claro
            "#c1272d", # rojo
            "#8b7355", # cafe
            "#855b5b", # vinotinto
            "#ed1e79") # rosado

#Lectura de bases de datos

cartas <- read_excel("cartas.xlsx", 
                        sheet = 1,  col_types = tipovar)



cartas$YEAR <- as.character(year(cartas$Fecha)) #Año
cartas$MONTH <- as.character(month(cartas$Fecha)) #meses


for (i in 1:9){
  cartas$MONTH[cartas$MONTH==i]= paste0("0",i)
}

conteo <- function(varc){
  cartas %>% group_by_(.dots = list("YEAR",varc)) %>% 
    summarise(Total = n()) %>% rename_(.dots=list("Clase"=varc)) %>% 
    mutate_(Variable = "varc") %>% select(Variable, YEAR, Clase, Total)
}

conteo2 <- function(varc){
  cartas %>% group_by_(.dots = list("MONTH",varc)) %>% 
    summarise(Total = n()) %>% rename_(.dots=list("Clase"=varc)) %>% 
    mutate_(Variable = "varc") %>% select(Variable, MONTH, Clase, Total)
}

meses <- conteo2("YEAR")
meses <- na.omit(meses[meses$Clase != "20173" & meses$Clase != "2010",,])

serie_global <- series3(
  datos = meses,
  categoria = "YEAR",
  colores = col,
  titulo = "Evolución por meses del número de cartas en cada año",
  eje = "Número de cartas"
); serie_global

saveWidget(serie_global, file = file.path(getwd(), "gráficos", "serie_mensual.html")  ,  selfcontained = F , libdir = "libraryjs")

Total <- cartas %>% group_by(YEAR) %>%  summarise(Total = n()) %>% ungroup() %>% 
  mutate(Variable="TOTAL", YEAR=YEAR, Clase = "Total", Total=Total) %>% 
  select(Variable, YEAR, Clase, Total)

Total <- na.omit(Total[Total$YEAR != "20173" & Total$YEAR != "2010",,])


serie_global <- series2(
  datos = Total,
  categoria = "TOTAL",
  colores = col,
  titulo =  "Evolución histórica anual",
  eje = "Número de cartas"
); serie_global

saveWidget(serie_global, file = file.path(getwd(), "gráficos", paste0("serie_anual.html"))  ,  selfcontained = F , libdir = "libraryjs")

paises <- conteo("País")

#paises <- paises[order(paises$Total, decreasing = T),]
paises <- paises[paises$YEAR != "20173",]
paises <- paises[paises$Clase  == "Colombia" | paises$Clase  == "Estados Unidos" | paises$Clase  == "Francia" | paises$Clase  == "Alemania" | paises$Clase  == "España" & paises$Clase  == "México" | paises$Clase  == "Argentina" | paises$Clase  == "Brasil" | paises$Clase  == "Chile" | paises$Clase  == "Venezuela"  | paises$Clase  == "Perú" | paises$Clase  == "Italia",]
paises <- na.omit(paises)
serie_global <- series2(
  datos = paises,
  categoria = "País",
  colores = col,
  titulo = "Evolución por meses del número de cartas por país de orígen",
  eje = "Número de cartas"
); serie_global

saveWidget(serie_global, file = file.path(getwd(), "gráficos", "serie_paises.html")  ,  selfcontained = F , libdir = "libraryjs")

Conteo3 <- function(varc){
  cartas %>% group_by_(.dots = list("YEAR",varc)) %>% 
    summarise(Total = n()) %>% rename_(.dots=list("Clase"=varc)) %>% 
    mutate_(Variable = "varc") %>% select(Variable, YEAR, Clase, Total)
}



paises <- Conteo3("tramite")
paises <- na.omit(paises[paises$YEAR != "20173",])

serie_global <- series2(
  datos = paises,
  categoria = "tramite",
  colores = col,
  titulo = "Evolución por meses del número de cartas según tipo de trámite",
  eje = "Número de cartas"
); serie_global

saveWidget(serie_global, file = file.path(getwd(), "gráficos", "serie_tramites.html")  ,  selfcontained = F , libdir = "libraryjs")

##### Mapa

divipola.R <- read.table("DIVIPOLA_20160930.csv", sep=";", header=T)


#### Lectura de datos

# Base de datos con solo cabeceras municipales 

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


centroids <- read_xlsx("centroids.xlsx") 

# Lista con los nombres ISO en español de los diferentes países.

spanish <- read_xlsx("centroids.xlsx", sheet = "Spanish")

spanish <- na.omit(spanish)


#### Manipulación

# Cuenta el número de docentes por país

cant_cartas <- cartas %>% group_by(País) %>% summarise(Total=n())
cant_cartas <- na.omit(cant_cartas)
cant_cartasPIP <- cartas[cartas$tramite== "PIP",] %>% group_by(País) %>% summarise(Total=n())
cant_cartasPIP <- na.omit(cant_cartasPIP)

cant_cartasAPC <- cartas[cartas$tramite== "APC",] %>% group_by(País) %>% summarise(Total=n())
cant_cartasAPC <- na.omit(cant_cartasAPC)
cant_cartasPIP5 <- cartas[cartas$tramite== "PIP5",] %>% group_by(País) %>% summarise(Total=n())
cant_cartasPIP5 <- na.omit(cant_cartasPIP5)

cant_cartasTP12 <- cartas[cartas$tramite== "TP12",] %>% group_by(País) %>% summarise(Total=n())
cant_cartasTP12 <- na.omit(cant_cartasTP12)

cant_cartasTP7 <- cartas[cartas$tramite== "TP7",] %>% group_by(País) %>% summarise(Total=n())
cant_cartasTP7 <- na.omit(cant_cartasTP7)

cant_cartasAUIP <- cartas[cartas$tramite== "PIP6",] %>% group_by(País) %>% summarise(Total=n())
cant_cartasAUIP <- na.omit(cant_cartasAUIP)

cant_cartasTP4 <- cartas[cartas$tramite== "TP4",] %>% group_by(País) %>% summarise(Total=n())
cant_cartasTP4 <- na.omit(cant_cartasTP4)

cant_cartasVISA <- cartas[cartas$tramite== "VISA",] %>% group_by(País) %>% summarise(Total=n())
cant_cartasVISA <- na.omit(cant_cartasVISA)

cant_cartasPIP7 <- cartas[cartas$tramite== "PIP7",] %>% group_by(País) %>% summarise(Total=n())
cant_cartasPIP7 <- na.omit(cant_cartasPIP7)

cant_cartasTP5 <- cartas[cartas$tramite== "TP5",] %>% group_by(País) %>% summarise(Total=n())
cant_cartasTP5 <- na.omit(cant_cartasTP5)

cant_cartasPIP2 <- cartas[cartas$tramite== "PIP2",] %>% group_by(País) %>% summarise(Total=n())
cant_cartasPIP2 <- na.omit(cant_cartasPIP2)

cant_cartasTP1 <- cartas[cartas$tramite== "TP1",] %>% group_by(País) %>% summarise(Total=n())
cant_cartasTP1 <- na.omit(cant_cartasTP1)

cant_cartasTP6 <- cartas[cartas$tramite== "TP6",] %>% group_by(País) %>% summarise(Total=n())
cant_cartasTP6 <- na.omit(cant_cartasTP6)


countries <- rgdal::readOGR("countriesgeo.json")

head(countries@data) # Vista previa del JSON

codigos <- matrix(0, nrow = 180, ncol = 18)

for(i in 1:180){
  codigos[i,1] = as.character(countries@data$id[i])
}


for(i in spanish$ISO){
  codigos[codigos[,1] == i, 2] = spanish$Nombre[spanish$ISO== i]
}

for(i in na.omit(cant_cartas$País)){
  codigos[codigos[,2] == i, 3] = cant_cartas$Total[cant_cartas$País == i]
}

for(i in codigos[-c(40,148, 69, 91, 140, 143, 176),1]){
  codigos[codigos[,1] == i, 4] = centroids$Longitude[centroids$CODS_PAISU == i]
}

for(i in codigos[-c(40,148, 69, 91, 140, 143, 176),1]){
  codigos[codigos[,1] == i, 5] = centroids$Latitude[centroids$CODS_PAISU == i]
}

for(i in na.omit(cant_cartasPIP$País)){
  codigos[codigos[,2] == i, 6] = cant_cartasPIP$Total[cant_cartasPIP$País == i]
}

for(i in na.omit(cant_cartasAPC$País)){
  codigos[codigos[,2] == i, 7] = cant_cartasAPC$Total[cant_cartasAPC$País == i]
}

for(i in na.omit(cant_cartasAUIP$País)){
  codigos[codigos[,2] == i, 8] = cant_cartasAUIP$Total[cant_cartasAUIP$País == i]
}

for(i in na.omit(cant_cartasPIP2$País)){
  codigos[codigos[,2] == i, 9] = cant_cartasPIP2$Total[cant_cartasPIP2$País == i]
}

for(i in na.omit(cant_cartasPIP5$País)){
  codigos[codigos[,2] == i, 10] = cant_cartasPIP5$Total[cant_cartasPIP5$País == i]
}

for(i in na.omit(cant_cartasPIP7$País)){
  codigos[codigos[,2] == i, 11] = cant_cartasPIP7$Total[cant_cartasPIP7$País == i]
}

for(i in na.omit(cant_cartasTP1$País)){
  codigos[codigos[,2] == i, 12] = cant_cartasTP1$Total[cant_cartasTP1$País == i]
}

for(i in na.omit(cant_cartasTP12$País)){
  codigos[codigos[,2] == i, 13] = cant_cartasTP12$Total[cant_cartasTP12$País == i]
}

for(i in na.omit(cant_cartasTP4$País)){
  codigos[codigos[,2] == i, 14] = cant_cartasTP4$Total[cant_cartasTP4$País == i]
}

for(i in na.omit(cant_cartasTP5$País)){
  codigos[codigos[,2] == i, 15] = cant_cartasTP5$Total[cant_cartasTP5$País == i]
}

for(i in na.omit(cant_cartasTP6$País)){
  codigos[codigos[,2] == i, 16] = cant_cartasTP6$Total[cant_cartasTP6$País == i]
}

for(i in na.omit(cant_cartasTP7$País)){
  codigos[codigos[,2] == i, 17] = cant_cartasTP7$Total[cant_cartasTP7$País == i]
}

for(i in na.omit(cant_cartasVISA$País)){
  codigos[codigos[,2] == i, 18] = cant_cartasVISA$Total[cant_cartasVISA$País == i]
}


countries@data$CANT_CARTAS <- as.numeric(codigos[,3])
countries@data$LONGITUD <- as.numeric(codigos[,4])
countries@data$LATITUD <- as.numeric(codigos[,5])
countries@data$NOMBRE <- codigos[,2]
countries@data$CANT_CARTASPIP <- as.numeric(codigos[,6])
countries@data$CANT_CARTASAPC <- as.numeric(codigos[,7])
countries@data$CANT_CARTASAUIP <- as.numeric(codigos[,8])
countries@data$CANT_CARTASPIP2 <- as.numeric(codigos[,9])
countries@data$CANT_CARTASPIP5 <- as.numeric(codigos[,10])
countries@data$CANT_CARTASPIP7 <- as.numeric(codigos[,11])
countries@data$CANT_CARTASTP1 <- as.numeric(codigos[,12])
countries@data$CANT_CARTASTP12 <- as.numeric(codigos[,13])
countries@data$CANT_CARTASTP4 <- as.numeric(codigos[,14])
countries@data$CANT_CARTASTP5 <- as.numeric(codigos[,15])
countries@data$CANT_CARTASTP6 <- as.numeric(codigos[,16])
countries@data$CANT_CARTASTP7 <- as.numeric(codigos[,17])
countries@data$CANT_CARTASVISA <- as.numeric(codigos[,18])




View(countries@data)

### Cambio para Francia de centroide afectado por isla en el Meditarraneo

countries@data[57,]$LONGITUD <- 2.413913
countries@data[57,]$LATITUD <- 46.766583

#### Base de mapa

#ESRI es un proveedor de bases de mapas con licencia pública

esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11, 2,5)]
esri
names.esri <- c("SimpleMap","StreetMap", "SateliteMap")
## Sedes de la Universidad

Sede <- c("Medellín", "Bogotá", "Manizales", "Tumaco", "Palmira", "Arauca", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")


# Aparece el nombre de la sede

label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Etiquetas con información para cada país

labels_countries <- sprintf(
  "<strong> %s </strong> <br/> %g  cartas" , 
  countries@data$NOMBRE,  countries@data$CANT_CARTAS
) %>% lapply(htmltools::HTML)

# Paleta de colores

colores <- c('#ffffcc','#c2e699','#78c679','#31a354','#006837')
binpal <- colorBin("Blues", bins=c(0,1, 11, 51, 101, Inf), palette = colores)

# código de mapa con leaflet

countriesmap <- leaflet(countries)

for (k in c(1 , 2 , 3)) {
  countriesmap <- countriesmap %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 2))
}

countriesmap <- countriesmap %>%
  addLayersControl(baseGroups = names.esri , 
                   options = layersControlOptions(collapsed = T)) %>%
  setView(lat = 0,  lng = 0,  zoom = 2)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_CARTAS) , 
              label = labels_countries ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addLegend("bottomright" ,  values = ~CANT_CARTAS, colors = colores ,  
            title = "Cartas",  opacity = 1,  bins=c(0,1, 11, 51, 101, Inf), labels = c("0","1 - 10 ", "11 - 50","51 - 100","Más de 100"))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(0 , 0) ,  2); }")))%>%
  addLabelOnlyMarkers(lat = ~LATITUD,  lng = ~LONGITUD , label =  ~paste0(countries$NOMBRE) ,   labelOptions = labelOptions(noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "10px") )%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))


countriesmap

saveWidget(countriesmap , file = file.path(getwd() ,  "gráficos" ,  "mapa_por_cartas.html")  ,  selfcontained = F , libdir = "libraryjs")

# Etiquetas con información para cada país

labels_countries <- sprintf(
  "<strong> %s </strong> <br/> %s %g  cartas <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g  " , 
  countries@data$NOMBRE, "Total: ", countries@data$CANT_CARTAS, 
  "PIP: ", countries@data$CANT_CARTASPIP,
  "PIP2: ", countries@data$CANT_CARTASPIP2,
  "PIP5: ", countries@data$CANT_CARTASPIP5,
  "PIP7: ", countries@data$CANT_CARTASPIP7,
  "APC: ", countries@data$CANT_CARTASAPC,
  "AUIP: ", countries@data$CANT_CARTASAUIP,
  "TP1: ", countries@data$CANT_CARTASTP1,
  "TP4: ", countries@data$CANT_CARTASTP4,
  "TP5: ", countries@data$CANT_CARTASTP5,
  "TP6: ", countries@data$CANT_CARTASTP6,
  "TP7: ", countries@data$CANT_CARTASTP7,
  "TP12: ", countries@data$CANT_CARTASTP12,
  "VISA: ", countries@data$CANT_CARTASVISA
) %>% lapply(htmltools::HTML)

# Paleta de colores

colores <- c('#ffffcc','#c2e699','#78c679','#31a354','#006837')
binpal <- colorBin("Blues", bins=c(0,1, 11, 51, 101, Inf), palette = colores)

# código de mapa con leaflet

countriesmap <- leaflet(countries)

for (k in c(1 , 2 , 3)) {
  countriesmap <- countriesmap %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 2))
}

countriesmap <- countriesmap %>%
  addLayersControl(baseGroups = names.esri , 
                   options = layersControlOptions(collapsed = T)) %>%
  setView(lat = 0,  lng = 0,  zoom = 2)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_CARTAS) , 
              label = labels_countries ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addLegend("bottomright" ,  values = ~CANT_CARTAS, colors = colores ,  
            title = "Cartas",  opacity = 1,  bins=c(0,1, 11, 51, 101, Inf), labels = c("0","1 - 10 ", "11 - 50","51 - 100","Más de 100"))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(0 , 0) ,  2); }")))%>%
  addLabelOnlyMarkers(lat = ~LATITUD,  lng = ~LONGITUD , label =  ~paste0(countries$NOMBRE) ,   labelOptions = labelOptions(noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "10px") )%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))


countriesmap

saveWidget(countriesmap , file = file.path(getwd() ,  "gráficos" ,  "mapa_por_cartas_tramite.html")  ,  selfcontained = F , libdir = "libraryjs")


###Año 2017

cant_cartas <- cartas %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartas <- na.omit(cant_cartas[cant_cartas$YEAR== "2017",])
cant_cartasPIP <- cartas[cartas$tramite== "PIP",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasPIP <- na.omit(cant_cartasPIP[cant_cartasPIP$YEAR== "2017",])

cant_cartasAPC <- cartas[cartas$tramite== "APC",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasAPC <- na.omit(cant_cartasAPC[cant_cartasAPC$YEAR== "2017",])
cant_cartasPIP5 <- cartas[cartas$tramite== "PIP5",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasPIP5 <- na.omit(cant_cartasPIP5[cant_cartasPIP5$YEAR== "2017",])

cant_cartasTP12 <- cartas[cartas$tramite== "TP12",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP12 <- na.omit(cant_cartasTP12[cant_cartasTP12$YEAR== "2017",])

cant_cartasTP7 <- cartas[cartas$tramite== "TP7",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP7 <- na.omit(cant_cartasTP7[cant_cartasTP7$YEAR== "2017",])

cant_cartasAUIP <- cartas[cartas$tramite== "PIP6",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasAUIP <- na.omit(cant_cartasAUIP[cant_cartasAUIP$YEAR== "2017",])

cant_cartasTP4 <- cartas[cartas$tramite== "TP4",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP4 <- na.omit(cant_cartasTP4[cant_cartasTP4$YEAR== "2017",])

cant_cartasVISA <- cartas[cartas$tramite== "VISA",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasVISA <- na.omit(cant_cartasVISA[cant_cartasVISA$YEAR== "2017",])

cant_cartasPIP7 <- cartas[cartas$tramite== "PIP7",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasPIP7 <- na.omit(cant_cartasPIP7[cant_cartasPIP7$YEAR== "2017",])

cant_cartasTP5 <- cartas[cartas$tramite== "TP5",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP5 <- na.omit(cant_cartasTP5[cant_cartasTP5$YEAR== "2017",])

cant_cartasPIP2 <- cartas[cartas$tramite== "PIP2",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasPIP2 <- na.omit(cant_cartasPIP2[cant_cartasPIP2$YEAR== "2017",])

cant_cartasTP1 <- cartas[cartas$tramite== "TP1",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP1 <- na.omit(cant_cartasTP1[cant_cartasTP1$YEAR== "2017",])

cant_cartasTP6 <- cartas[cartas$tramite== "TP6",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP6 <- na.omit(cant_cartasTP6[cant_cartasTP6$YEAR== "2017",])


countries <- rgdal::readOGR("countriesgeo.json")

head(countries@data) # Vista previa del JSON

codigos <- matrix(0, nrow = 180, ncol = 18)

for(i in 1:180){
  codigos[i,1] = as.character(countries@data$id[i])
}


for(i in spanish$ISO){
  codigos[codigos[,1] == i, 2] = spanish$Nombre[spanish$ISO== i]
}

for(i in na.omit(cant_cartas$País)){
  codigos[codigos[,2] == i, 3] = cant_cartas$Total[cant_cartas$País == i]
}

for(i in codigos[-c(40,148, 69, 91, 140, 143, 176),1]){
  codigos[codigos[,1] == i, 4] = centroids$Longitude[centroids$CODS_PAISU == i]
}

for(i in codigos[-c(40,148, 69, 91, 140, 143, 176),1]){
  codigos[codigos[,1] == i, 5] = centroids$Latitude[centroids$CODS_PAISU == i]
}

for(i in na.omit(cant_cartasPIP$País)){
  codigos[codigos[,2] == i, 6] = cant_cartasPIP$Total[cant_cartasPIP$País == i]
}

for(i in na.omit(cant_cartasAPC$País)){
  codigos[codigos[,2] == i, 7] = cant_cartasAPC$Total[cant_cartasAPC$País == i]
}

for(i in na.omit(cant_cartasAUIP$País)){
  codigos[codigos[,2] == i, 8] = cant_cartasAUIP$Total[cant_cartasAUIP$País == i]
}

for(i in na.omit(cant_cartasPIP2$País)){
  codigos[codigos[,2] == i, 9] = cant_cartasPIP2$Total[cant_cartasPIP2$País == i]
}

for(i in na.omit(cant_cartasPIP5$País)){
  codigos[codigos[,2] == i, 10] = cant_cartasPIP5$Total[cant_cartasPIP5$País == i]
}

for(i in na.omit(cant_cartasPIP7$País)){
  codigos[codigos[,2] == i, 11] = cant_cartasPIP7$Total[cant_cartasPIP7$País == i]
}

for(i in na.omit(cant_cartasTP1$País)){
  codigos[codigos[,2] == i, 12] = cant_cartasTP1$Total[cant_cartasTP1$País == i]
}

for(i in na.omit(cant_cartasTP12$País)){
  codigos[codigos[,2] == i, 13] = cant_cartasTP12$Total[cant_cartasTP12$País == i]
}

for(i in na.omit(cant_cartasTP4$País)){
  codigos[codigos[,2] == i, 14] = cant_cartasTP4$Total[cant_cartasTP4$País == i]
}

for(i in na.omit(cant_cartasTP5$País)){
  codigos[codigos[,2] == i, 15] = cant_cartasTP5$Total[cant_cartasTP5$País == i]
}

for(i in na.omit(cant_cartasTP6$País)){
  codigos[codigos[,2] == i, 16] = cant_cartasTP6$Total[cant_cartasTP6$País == i]
}

for(i in na.omit(cant_cartasTP7$País)){
  codigos[codigos[,2] == i, 17] = cant_cartasTP7$Total[cant_cartasTP7$País == i]
}

for(i in na.omit(cant_cartasVISA$País)){
  codigos[codigos[,2] == i, 18] = cant_cartasVISA$Total[cant_cartasVISA$País == i]
}


countries@data$CANT_CARTAS <- as.numeric(codigos[,3])
countries@data$LONGITUD <- as.numeric(codigos[,4])
countries@data$LATITUD <- as.numeric(codigos[,5])
countries@data$NOMBRE <- codigos[,2]
countries@data$CANT_CARTASPIP <- as.numeric(codigos[,6])
countries@data$CANT_CARTASAPC <- as.numeric(codigos[,7])
countries@data$CANT_CARTASAUIP <- as.numeric(codigos[,8])
countries@data$CANT_CARTASPIP2 <- as.numeric(codigos[,9])
countries@data$CANT_CARTASPIP5 <- as.numeric(codigos[,10])
countries@data$CANT_CARTASPIP7 <- as.numeric(codigos[,11])
countries@data$CANT_CARTASTP1 <- as.numeric(codigos[,12])
countries@data$CANT_CARTASTP12 <- as.numeric(codigos[,13])
countries@data$CANT_CARTASTP4 <- as.numeric(codigos[,14])
countries@data$CANT_CARTASTP5 <- as.numeric(codigos[,15])
countries@data$CANT_CARTASTP6 <- as.numeric(codigos[,16])
countries@data$CANT_CARTASTP7 <- as.numeric(codigos[,17])
countries@data$CANT_CARTASVISA <- as.numeric(codigos[,18])





### Cambio para Francia de centroide afectado por isla en el Meditarraneo

countries@data[57,]$LONGITUD <- 2.413913
countries@data[57,]$LATITUD <- 46.766583

#### Base de mapa

#ESRI es un proveedor de bases de mapas con licencia pública

esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11, 2,5)]
esri
names.esri <- c("SimpleMap","StreetMap", "SateliteMap")
## Sedes de la Universidad

Sede <- c("Medellín", "Bogotá", "Manizales", "Tumaco", "Palmira", "Arauca", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")


# Aparece el nombre de la sede

label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)



labels_countries <- sprintf(
  "<strong> %s </strong> <br/> %s %g  cartas <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g  " , 
  countries@data$NOMBRE, "Total: ", countries@data$CANT_CARTAS, 
  "PIP: ", countries@data$CANT_CARTASPIP,
  "PIP2: ", countries@data$CANT_CARTASPIP2,
  "PIP5: ", countries@data$CANT_CARTASPIP5,
  "PIP7: ", countries@data$CANT_CARTASPIP7,
  "APC: ", countries@data$CANT_CARTASAPC,
  "AUIP: ", countries@data$CANT_CARTASAUIP,
  "TP1: ", countries@data$CANT_CARTASTP1,
  "TP4: ", countries@data$CANT_CARTASTP4,
  "TP5: ", countries@data$CANT_CARTASTP5,
  "TP6: ", countries@data$CANT_CARTASTP6,
  "TP7: ", countries@data$CANT_CARTASTP7,
  "TP12: ", countries@data$CANT_CARTASTP12,
  "VISA: ", countries@data$CANT_CARTASVISA
) %>% lapply(htmltools::HTML)

# Paleta de colores

colores <- c('#f2f0f7','#cbc9e2','#9e9ac8','#756bb1','#54278f')
binpal <- colorBin("Blues", bins=c(0,1, 11, 51, 101, Inf), palette = colores)

# código de mapa con leaflet

countriesmap <- leaflet(countries)

for (k in c(1 , 2 , 3)) {
  countriesmap <- countriesmap %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 2))
}

countriesmap <- countriesmap %>%
  addLayersControl(baseGroups = names.esri , 
                   options = layersControlOptions(collapsed = T)) %>%
  setView(lat = 0,  lng = 0,  zoom = 2)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_CARTAS) , 
              label = labels_countries ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addLegend("bottomright" ,  values = ~CANT_CARTAS, colors = colores ,  
            title = "Cartas",  opacity = 1,  bins=c(0,1, 11, 51, 101, Inf), labels = c("0","1 - 10 ", "11 - 50","51 - 100","Más de 100"))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(0 , 0) ,  2); }")))%>%
  addLabelOnlyMarkers(lat = ~LATITUD,  lng = ~LONGITUD , label =  ~paste0(countries$NOMBRE) ,   labelOptions = labelOptions(noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "10px") )%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))


countriesmap

saveWidget(countriesmap , file = file.path(getwd() ,  "gráficos" ,  "mapa_por_cartas_tramite2017.html")  ,  selfcontained = F , libdir = "libraryjs")


###Año 2018

cant_cartas <- cartas %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartas <- na.omit(cant_cartas[cant_cartas$YEAR== "2018",])
cant_cartasPIP <- cartas[cartas$tramite== "PIP",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasPIP <- na.omit(cant_cartasPIP[cant_cartasPIP$YEAR== "2018",])

cant_cartasAPC <- cartas[cartas$tramite== "APC",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasAPC <- na.omit(cant_cartasAPC[cant_cartasAPC$YEAR== "2018",])
cant_cartasPIP5 <- cartas[cartas$tramite== "PIP5",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasPIP5 <- na.omit(cant_cartasPIP5[cant_cartasPIP5$YEAR== "2018",])

cant_cartasTP12 <- cartas[cartas$tramite== "TP12",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP12 <- na.omit(cant_cartasTP12[cant_cartasTP12$YEAR== "2018",])

cant_cartasTP7 <- cartas[cartas$tramite== "TP7",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP7 <- na.omit(cant_cartasTP7[cant_cartasTP7$YEAR== "2018",])

cant_cartasAUIP <- cartas[cartas$tramite== "PIP6",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasAUIP <- na.omit(cant_cartasAUIP[cant_cartasAUIP$YEAR== "2018",])

cant_cartasTP4 <- cartas[cartas$tramite== "TP4",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP4 <- na.omit(cant_cartasTP4[cant_cartasTP4$YEAR== "2018",])

cant_cartasVISA <- cartas[cartas$tramite== "VISA",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasVISA <- na.omit(cant_cartasVISA[cant_cartasVISA$YEAR== "2018",])

cant_cartasPIP7 <- cartas[cartas$tramite== "PIP7",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasPIP7 <- na.omit(cant_cartasPIP7[cant_cartasPIP7$YEAR== "2018",])

cant_cartasTP5 <- cartas[cartas$tramite== "TP5",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP5 <- na.omit(cant_cartasTP5[cant_cartasTP5$YEAR== "2018",])

cant_cartasPIP2 <- cartas[cartas$tramite== "PIP2",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasPIP2 <- na.omit(cant_cartasPIP2[cant_cartasPIP2$YEAR== "2018",])

cant_cartasTP1 <- cartas[cartas$tramite== "TP1",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP1 <- na.omit(cant_cartasTP1[cant_cartasTP1$YEAR== "2018",])

cant_cartasTP6 <- cartas[cartas$tramite== "TP6",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP6 <- na.omit(cant_cartasTP6[cant_cartasTP6$YEAR== "2018",])


countries <- rgdal::readOGR("countriesgeo.json")

head(countries@data) # Vista previa del JSON

codigos <- matrix(0, nrow = 180, ncol = 18)

for(i in 1:180){
  codigos[i,1] = as.character(countries@data$id[i])
}


for(i in spanish$ISO){
  codigos[codigos[,1] == i, 2] = spanish$Nombre[spanish$ISO== i]
}

for(i in na.omit(cant_cartas$País)){
  codigos[codigos[,2] == i, 3] = cant_cartas$Total[cant_cartas$País == i]
}

for(i in codigos[-c(40,148, 69, 91, 140, 143, 176),1]){
  codigos[codigos[,1] == i, 4] = centroids$Longitude[centroids$CODS_PAISU == i]
}

for(i in codigos[-c(40,148, 69, 91, 140, 143, 176),1]){
  codigos[codigos[,1] == i, 5] = centroids$Latitude[centroids$CODS_PAISU == i]
}

for(i in na.omit(cant_cartasPIP$País)){
  codigos[codigos[,2] == i, 6] = cant_cartasPIP$Total[cant_cartasPIP$País == i]
}

for(i in na.omit(cant_cartasAPC$País)){
  codigos[codigos[,2] == i, 7] = cant_cartasAPC$Total[cant_cartasAPC$País == i]
}

for(i in na.omit(cant_cartasAUIP$País)){
  codigos[codigos[,2] == i, 8] = cant_cartasAUIP$Total[cant_cartasAUIP$País == i]
}

for(i in na.omit(cant_cartasPIP2$País)){
  codigos[codigos[,2] == i, 9] = cant_cartasPIP2$Total[cant_cartasPIP2$País == i]
}

for(i in na.omit(cant_cartasPIP5$País)){
  codigos[codigos[,2] == i, 10] = cant_cartasPIP5$Total[cant_cartasPIP5$País == i]
}

for(i in na.omit(cant_cartasPIP7$País)){
  codigos[codigos[,2] == i, 11] = cant_cartasPIP7$Total[cant_cartasPIP7$País == i]
}

for(i in na.omit(cant_cartasTP1$País)){
  codigos[codigos[,2] == i, 12] = cant_cartasTP1$Total[cant_cartasTP1$País == i]
}

for(i in na.omit(cant_cartasTP12$País)){
  codigos[codigos[,2] == i, 13] = cant_cartasTP12$Total[cant_cartasTP12$País == i]
}

for(i in na.omit(cant_cartasTP4$País)){
  codigos[codigos[,2] == i, 14] = cant_cartasTP4$Total[cant_cartasTP4$País == i]
}

for(i in na.omit(cant_cartasTP5$País)){
  codigos[codigos[,2] == i, 15] = cant_cartasTP5$Total[cant_cartasTP5$País == i]
}

for(i in na.omit(cant_cartasTP6$País)){
  codigos[codigos[,2] == i, 16] = cant_cartasTP6$Total[cant_cartasTP6$País == i]
}

for(i in na.omit(cant_cartasTP7$País)){
  codigos[codigos[,2] == i, 17] = cant_cartasTP7$Total[cant_cartasTP7$País == i]
}

for(i in na.omit(cant_cartasVISA$País)){
  codigos[codigos[,2] == i, 18] = cant_cartasVISA$Total[cant_cartasVISA$País == i]
}


countries@data$CANT_CARTAS <- as.numeric(codigos[,3])
countries@data$LONGITUD <- as.numeric(codigos[,4])
countries@data$LATITUD <- as.numeric(codigos[,5])
countries@data$NOMBRE <- codigos[,2]
countries@data$CANT_CARTASPIP <- as.numeric(codigos[,6])
countries@data$CANT_CARTASAPC <- as.numeric(codigos[,7])
countries@data$CANT_CARTASAUIP <- as.numeric(codigos[,8])
countries@data$CANT_CARTASPIP2 <- as.numeric(codigos[,9])
countries@data$CANT_CARTASPIP5 <- as.numeric(codigos[,10])
countries@data$CANT_CARTASPIP7 <- as.numeric(codigos[,11])
countries@data$CANT_CARTASTP1 <- as.numeric(codigos[,12])
countries@data$CANT_CARTASTP12 <- as.numeric(codigos[,13])
countries@data$CANT_CARTASTP4 <- as.numeric(codigos[,14])
countries@data$CANT_CARTASTP5 <- as.numeric(codigos[,15])
countries@data$CANT_CARTASTP6 <- as.numeric(codigos[,16])
countries@data$CANT_CARTASTP7 <- as.numeric(codigos[,17])
countries@data$CANT_CARTASVISA <- as.numeric(codigos[,18])





### Cambio para Francia de centroide afectado por isla en el Meditarraneo

countries@data[57,]$LONGITUD <- 2.413913
countries@data[57,]$LATITUD <- 46.766583

#### Base de mapa

#ESRI es un proveedor de bases de mapas con licencia pública

esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11, 2,5)]
esri
names.esri <- c("SimpleMap","StreetMap", "SateliteMap")
## Sedes de la Universidad

Sede <- c("Medellín", "Bogotá", "Manizales", "Tumaco", "Palmira", "Arauca", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")


# Aparece el nombre de la sede

label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)



labels_countries <- sprintf(
  "<strong> %s </strong> <br/> %s %g  cartas <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g  " , 
  countries@data$NOMBRE, "Total: ", countries@data$CANT_CARTAS, 
  "PIP: ", countries@data$CANT_CARTASPIP,
  "PIP2: ", countries@data$CANT_CARTASPIP2,
  "PIP5: ", countries@data$CANT_CARTASPIP5,
  "PIP7: ", countries@data$CANT_CARTASPIP7,
  "APC: ", countries@data$CANT_CARTASAPC,
  "AUIP: ", countries@data$CANT_CARTASAUIP,
  "TP1: ", countries@data$CANT_CARTASTP1,
  "TP4: ", countries@data$CANT_CARTASTP4,
  "TP5: ", countries@data$CANT_CARTASTP5,
  "TP6: ", countries@data$CANT_CARTASTP6,
  "TP7: ", countries@data$CANT_CARTASTP7,
  "TP12: ", countries@data$CANT_CARTASTP12,
  "VISA: ", countries@data$CANT_CARTASVISA
) %>% lapply(htmltools::HTML)

# Paleta de colores

colores <- c('#ffffd4','#fed98e','#fe9929','#d95f0e','#993404')
binpal <- colorBin("Blues", bins=c(0,1, 11, 51, 101, Inf), palette = colores)

# código de mapa con leaflet

countriesmap <- leaflet(countries)

for (k in c(1 , 2 , 3)) {
  countriesmap <- countriesmap %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 2))
}

countriesmap <- countriesmap %>%
  addLayersControl(baseGroups = names.esri , 
                   options = layersControlOptions(collapsed = T)) %>%
  setView(lat = 0,  lng = 0,  zoom = 2)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_CARTAS) , 
              label = labels_countries ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addLegend("bottomright" ,  values = ~CANT_CARTAS, colors = colores ,  
            title = "Cartas",  opacity = 1,  bins=c(0,1, 11, 51, 101, Inf), labels = c("0","1 - 10 ", "11 - 50","51 - 100","Más de 100"))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(0 , 0) ,  2); }")))%>%
  addLabelOnlyMarkers(lat = ~LATITUD,  lng = ~LONGITUD , label =  ~paste0(countries$NOMBRE) ,   labelOptions = labelOptions(noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "10px") )%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))


countriesmap

saveWidget(countriesmap , file = file.path(getwd() ,  "gráficos" ,  "mapa_por_cartas_tramite2018.html")  ,  selfcontained = F , libdir = "libraryjs")


###Año 2019

cant_cartas <- cartas %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartas <- na.omit(cant_cartas[cant_cartas$YEAR== "2019",])
cant_cartasPIP <- cartas[cartas$tramite== "PIP",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasPIP <- na.omit(cant_cartasPIP[cant_cartasPIP$YEAR== "2019",])

cant_cartasAPC <- cartas[cartas$tramite== "APC",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasAPC <- na.omit(cant_cartasAPC[cant_cartasAPC$YEAR== "2019",])
cant_cartasPIP5 <- cartas[cartas$tramite== "PIP5",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasPIP5 <- na.omit(cant_cartasPIP5[cant_cartasPIP5$YEAR== "2019",])

cant_cartasTP12 <- cartas[cartas$tramite== "TP12",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP12 <- na.omit(cant_cartasTP12[cant_cartasTP12$YEAR== "2019",])

cant_cartasTP7 <- cartas[cartas$tramite== "TP7",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP7 <- na.omit(cant_cartasTP7[cant_cartasTP7$YEAR== "2019",])

cant_cartasAUIP <- cartas[cartas$tramite== "PIP6",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasAUIP <- na.omit(cant_cartasAUIP[cant_cartasAUIP$YEAR== "2019",])

cant_cartasTP4 <- cartas[cartas$tramite== "TP4",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP4 <- na.omit(cant_cartasTP4[cant_cartasTP4$YEAR== "2019",])

cant_cartasVISA <- cartas[cartas$tramite== "VISA",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasVISA <- na.omit(cant_cartasVISA[cant_cartasVISA$YEAR== "2019",])

cant_cartasPIP7 <- cartas[cartas$tramite== "PIP7",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasPIP7 <- na.omit(cant_cartasPIP7[cant_cartasPIP7$YEAR== "2019",])

cant_cartasTP5 <- cartas[cartas$tramite== "TP5",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP5 <- na.omit(cant_cartasTP5[cant_cartasTP5$YEAR== "2019",])

cant_cartasPIP2 <- cartas[cartas$tramite== "PIP2",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasPIP2 <- na.omit(cant_cartasPIP2[cant_cartasPIP2$YEAR== "2019",])

cant_cartasTP1 <- cartas[cartas$tramite== "TP1",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP1 <- na.omit(cant_cartasTP1[cant_cartasTP1$YEAR== "2019",])

cant_cartasTP6 <- cartas[cartas$tramite== "TP6",] %>% group_by(País, YEAR) %>% summarise(Total=n())
cant_cartasTP6 <- na.omit(cant_cartasTP6[cant_cartasTP6$YEAR== "2019",])


countries <- rgdal::readOGR("countriesgeo.json")

head(countries@data) # Vista previa del JSON

codigos <- matrix(0, nrow = 180, ncol = 18)

for(i in 1:180){
  codigos[i,1] = as.character(countries@data$id[i])
}


for(i in spanish$ISO){
  codigos[codigos[,1] == i, 2] = spanish$Nombre[spanish$ISO== i]
}

for(i in na.omit(cant_cartas$País)){
  codigos[codigos[,2] == i, 3] = cant_cartas$Total[cant_cartas$País == i]
}

for(i in codigos[-c(40,148, 69, 91, 140, 143, 176),1]){
  codigos[codigos[,1] == i, 4] = centroids$Longitude[centroids$CODS_PAISU == i]
}

for(i in codigos[-c(40,148, 69, 91, 140, 143, 176),1]){
  codigos[codigos[,1] == i, 5] = centroids$Latitude[centroids$CODS_PAISU == i]
}

for(i in na.omit(cant_cartasPIP$País)){
  codigos[codigos[,2] == i, 6] = cant_cartasPIP$Total[cant_cartasPIP$País == i]
}

for(i in na.omit(cant_cartasAPC$País)){
  codigos[codigos[,2] == i, 7] = cant_cartasAPC$Total[cant_cartasAPC$País == i]
}

for(i in na.omit(cant_cartasAUIP$País)){
  codigos[codigos[,2] == i, 8] = cant_cartasAUIP$Total[cant_cartasAUIP$País == i]
}

for(i in na.omit(cant_cartasPIP2$País)){
  codigos[codigos[,2] == i, 9] = cant_cartasPIP2$Total[cant_cartasPIP2$País == i]
}

for(i in na.omit(cant_cartasPIP5$País)){
  codigos[codigos[,2] == i, 10] = cant_cartasPIP5$Total[cant_cartasPIP5$País == i]
}

for(i in na.omit(cant_cartasPIP7$País)){
  codigos[codigos[,2] == i, 11] = cant_cartasPIP7$Total[cant_cartasPIP7$País == i]
}

for(i in na.omit(cant_cartasTP1$País)){
  codigos[codigos[,2] == i, 12] = cant_cartasTP1$Total[cant_cartasTP1$País == i]
}

for(i in na.omit(cant_cartasTP12$País)){
  codigos[codigos[,2] == i, 13] = cant_cartasTP12$Total[cant_cartasTP12$País == i]
}

for(i in na.omit(cant_cartasTP4$País)){
  codigos[codigos[,2] == i, 14] = cant_cartasTP4$Total[cant_cartasTP4$País == i]
}

for(i in na.omit(cant_cartasTP5$País)){
  codigos[codigos[,2] == i, 15] = cant_cartasTP5$Total[cant_cartasTP5$País == i]
}

for(i in na.omit(cant_cartasTP6$País)){
  codigos[codigos[,2] == i, 16] = cant_cartasTP6$Total[cant_cartasTP6$País == i]
}

for(i in na.omit(cant_cartasTP7$País)){
  codigos[codigos[,2] == i, 17] = cant_cartasTP7$Total[cant_cartasTP7$País == i]
}

for(i in na.omit(cant_cartasVISA$País)){
  codigos[codigos[,2] == i, 18] = cant_cartasVISA$Total[cant_cartasVISA$País == i]
}


countries@data$CANT_CARTAS <- as.numeric(codigos[,3])
countries@data$LONGITUD <- as.numeric(codigos[,4])
countries@data$LATITUD <- as.numeric(codigos[,5])
countries@data$NOMBRE <- codigos[,2]
countries@data$CANT_CARTASPIP <- as.numeric(codigos[,6])
countries@data$CANT_CARTASAPC <- as.numeric(codigos[,7])
countries@data$CANT_CARTASAUIP <- as.numeric(codigos[,8])
countries@data$CANT_CARTASPIP2 <- as.numeric(codigos[,9])
countries@data$CANT_CARTASPIP5 <- as.numeric(codigos[,10])
countries@data$CANT_CARTASPIP7 <- as.numeric(codigos[,11])
countries@data$CANT_CARTASTP1 <- as.numeric(codigos[,12])
countries@data$CANT_CARTASTP12 <- as.numeric(codigos[,13])
countries@data$CANT_CARTASTP4 <- as.numeric(codigos[,14])
countries@data$CANT_CARTASTP5 <- as.numeric(codigos[,15])
countries@data$CANT_CARTASTP6 <- as.numeric(codigos[,16])
countries@data$CANT_CARTASTP7 <- as.numeric(codigos[,17])
countries@data$CANT_CARTASVISA <- as.numeric(codigos[,18])





### Cambio para Francia de centroide afectado por isla en el Meditarraneo

countries@data[57,]$LONGITUD <- 2.413913
countries@data[57,]$LATITUD <- 46.766583

#### Base de mapa

#ESRI es un proveedor de bases de mapas con licencia pública

esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11, 2,5)]
esri
names.esri <- c("SimpleMap","StreetMap", "SateliteMap")
## Sedes de la Universidad

Sede <- c("Medellín", "Bogotá", "Manizales", "Tumaco", "Palmira", "Arauca", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")


# Aparece el nombre de la sede

label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)



labels_countries <- sprintf(
  "<strong> %s </strong> <br/> %s %g  cartas <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g    &nbsp %s %g   <br/> %s %g  " , 
  countries@data$NOMBRE, "Total: ", countries@data$CANT_CARTAS, 
  "PIP: ", countries@data$CANT_CARTASPIP,
  "PIP2: ", countries@data$CANT_CARTASPIP2,
  "PIP5: ", countries@data$CANT_CARTASPIP5,
  "PIP7: ", countries@data$CANT_CARTASPIP7,
  "APC: ", countries@data$CANT_CARTASAPC,
  "AUIP: ", countries@data$CANT_CARTASAUIP,
  "TP1: ", countries@data$CANT_CARTASTP1,
  "TP4: ", countries@data$CANT_CARTASTP4,
  "TP5: ", countries@data$CANT_CARTASTP5,
  "TP6: ", countries@data$CANT_CARTASTP6,
  "TP7: ", countries@data$CANT_CARTASTP7,
  "TP12: ", countries@data$CANT_CARTASTP12,
  "VISA: ", countries@data$CANT_CARTASVISA
) %>% lapply(htmltools::HTML)

# Paleta de colores

colores <- c('#feebe2','#fbb4b9','#f768a1','#c51b8a','#7a0177')
binpal <- colorBin("Blues", bins=c(0,1, 11, 51, 101, Inf), palette = colores)

# código de mapa con leaflet

countriesmap <- leaflet(countries)

for (k in c(1 , 2 , 3)) {
  countriesmap <- countriesmap %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 2))
}

countriesmap <- countriesmap %>%
  addLayersControl(baseGroups = names.esri , 
                   options = layersControlOptions(collapsed = T)) %>%
  setView(lat = 0,  lng = 0,  zoom = 2)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_CARTAS) , 
              label = labels_countries ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addLegend("bottomright" ,  values = ~CANT_CARTAS, colors = colores ,  
            title = "Cartas",  opacity = 1,  bins=c(0,1, 11, 51, 101, Inf), labels = c("0","1 - 10 ", "11 - 50","51 - 100","Más de 100"))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(0 , 0) ,  2); }")))%>%
  addLabelOnlyMarkers(lat = ~LATITUD,  lng = ~LONGITUD , label =  ~paste0(countries$NOMBRE) ,   labelOptions = labelOptions(noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "10px") )%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))


countriesmap

saveWidget(countriesmap , file = file.path(getwd() ,  "gráficos" ,  "mapa_por_cartas_tramite2019.html")  ,  selfcontained = F , libdir = "libraryjs")
