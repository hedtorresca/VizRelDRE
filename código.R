# Librerías utilizadas

library(tidyverse) # Paquete para manipulación y consulta.
library(lubridate)
library(highcharter)
library(readxl) #Paquete para lectura de datos.
library(ggplot2)
library(htmlwidgets)
library(xlsx)
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

paises <- conteo2("País")

paises <- na.omit(paises[paises$YEAR != "20173" & paises$YEAR != "2010" & paises$YEAR != "2016" & paises$YEAR != "2017",])

serie_global <- series2(
  datos = paises,
  categoria = "País",
  colores = col,
  titulo = "Evolución por meses del número de cartas en cada país de orígen",
  eje = "Número de cartas"
); serie_global

saveWidget(serie_global, file = file.path(getwd(), "gráficos", "serie_paises.html")  ,  selfcontained = F , libdir = "libraryjs")

Conteo3 <- function(varc){
  cartas %>% group_by_(.dots = list("YEAR",varc)) %>% 
    summarise(Total = n()) %>% rename_(.dots=list("Clase"=varc)) %>% 
    mutate_(Variable = "varc") %>% select(Variable, YEAR, Clase, Total)
}


paises <- Conteo3("País")
paises <- na.omit(paises[paises$YEAR != "20173" & paises$YEAR != "2010" & paises$YEAR != "2016" & paises$YEAR != "2017",])

serie_global <- series2(
  datos = paises,
  categoria = "País",
  colores = col,
  titulo = "Evolución por meses del número de cartas en cada país de orígen",
  eje = "Número de cartas"
); serie_global

saveWidget(serie_global, file = file.path(getwd(), "gráficos", "serie_paises2.html")  ,  selfcontained = F , libdir = "libraryjs")

paises <- Conteo3("tramite")
paises <- na.omit(paises[paises$YEAR != "20173" & paises$YEAR != "2010",])

serie_global <- series2(
  datos = paises,
  categoria = "tramite",
  colores = col,
  titulo = "Evolución por meses del número de cartas según tipo de trámite",
  eje = "Número de cartas"
); serie_global

saveWidget(serie_global, file = file.path(getwd(), "gráficos", "serie_tramites.html")  ,  selfcontained = F , libdir = "libraryjs")
