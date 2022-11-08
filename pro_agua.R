library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)
library(leaflet)
library(GGally)
library(reshape2)
library(plotly)
library(tidyverse)
library(gridExtra)


#ingresar el path donde estan todos los archivos
#el path de la carpeta que se llama proagua-data
path = "C:/Users/mateo/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/Vis/proagua-data/"


load(paste(path, "mxmunicipio.map.RData", sep=""))
aux = read.csv(paste(path, "secc_i_tr_m5_2018.csv", sep=""))

#como en una son char y en la otra numeric, hay un cero colado a la izquierda
mxmunicipio.map$region = as.numeric(as.character(mxmunicipio.map$region))

#cambia el nombre de la columna para hacer el join
names(aux)[names(aux)=="folio"] <- "region"

agua_municipio = merge(x=mxmunicipio.map, 
                       y=aux,by="region",all="TRUE")

#municipios
ggplot(agua_municipio) + 
  geom_polygon(data = mxmunicipio.map, aes(long, lat, group = group),
               fill = 'grey30', colour = 'grey30', show.legend = FALSE,) + 
  geom_polygon(data = agua_municipio, aes(long, lat, group = group, fill = pobl_prc)) +
  coord_quickmap() +
  ggtitle("Acceso a agua de la red p�blica") +
  theme(plot.title = element_text(hjust = 0.5))


# #mean(agua_municipio$pobl_prc[agua_municipio$pobl_prc >= 0], na.rm = TRUE)
# porcentajemx = as.numeric(agua_municipio$pobl_prc)
# #obtenemos el promedio de agua potable seg?n datos del INEGI
# promediomx = mean(porcentajemx,  na.rm = TRUE)
# promediomx


#importamos datos del banco mundial
apm = read.csv(paste(path, "apm.csv", sep=""))
#names(apm)[names(apm)=="?..Year"] <- "Year"
names(apm)[1] <- "Year"
apm$Population <- gsub("\\%","",apm$Population)
apm$Population <- as.numeric(as.character(apm$Population))
apm$Annual_Change <- gsub("\\%","",apm$Annual_Change)
apm$Annual_Change <- as.numeric(as.character(apm$Annual_Change))
apm$Year <- as.numeric(as.character(apm$Year))

apm
glimpse(apm)

ggplot(apm, aes(x=Year, y=Population, group = 1)) + 
  geom_point() +
  geom_line() +
  ggtitle("Porcentaje de poblacion con agua potable Mexico") + 
  theme(plot.title = element_text(hjust = 0.5)) 

bar_apm <- ggplot(apm, aes(x=factor(Year), y=Annual_Change)) + 
  ggtitle("Cambio porcentual anual",) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(stat="identity") 

line_apm <- ggplot(apm, aes(x=Year, y=Population, group = 1)) + 
  geom_point() +
  geom_line() +
  ggtitle("Porcentaje de poblacion con agua potable Mexico") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,100))


appaises = read.csv(paste(path, "appaises.csv", sep=""))
#names(appaises)[names(appaises)=="ï..Country.Name"] <- "Country"
names(appaises)[1] <- "Country"
names(appaises)[names(appaises)=="X..of.Population"] <- "Population"
appaises$Population <- gsub("\\%","",appaises$Population)
appaises$Population <- as.numeric(as.character(appaises$Population))

glimpse(appaises)

appaises1 = rbind(appaises %>% slice(1:5),appaises %>% slice(15:19) , appaises %>% slice(25:27) )
bar_paises <- ggplot(appaises1, aes(x=fct_reorder(Country, Population, .desc=TRUE), y=Population,
                      fill=factor(ifelse(Country=="Mexico","Highlighted","Normal")))) +
  geom_bar(stat="identity", show.legend = FALSE)  + 
  ggtitle("Pobalacion con agua potable en varios paises",) +
  theme(plot.title = element_text(hjust = 0.5))
appaises

ggplot(appaises1, aes(x=fct_reorder(Country, Population, .desc=TRUE), y=Population,
                      fill=factor(ifelse(Country=="Mexico","Highlighted","Normal")))) +
  geom_bar(stat="identity", show.legend = FALSE)  + scale_fill_grey() +
  ggtitle("Pobalacion con agua potable en varios paises",) +
  theme(plot.title = element_text(hjust = 0.5))

appaises1

grid.arrange(bar_paises, line_apm, bar_apm, ncol = 2, 
             layout_matrix = cbind(c(1,1), c(2,3)))

fuentes = read.csv(paste(path, "fuentes_agua.csv", sep=""))
glimpse(fuentes)

fuentes_sem_v = filter(fuentes, SEMAFORO == 'Verde')
fuentes_sem_a = filter(fuentes, SEMAFORO == 'Amarillo')
fuentes_sem_r = filter(fuentes, SEMAFORO == 'Rojo')

nrow(fuentes_sem_v)
nrow(fuentes_sem_a)
nrow(fuentes_sem_r)

leaflet() %>% 
  addTiles() %>% 
  addMarkers(data = fuentes_sem_v, lng = ~LONGITUD, lat = ~LATITUD, group = "Semaforo Verde",
             popup = ~paste(MUNICIPIO, CUENCA, CUERPO.DE.AGUA, TIPO, SEMAFORO, sep = '<br/>'),
             icon = list (
               iconUrl = paste(path, "water-drop-blue.png", sep=""),
               iconSize = c(30,30)
             )) %>%
  addMarkers(data = fuentes_sem_a, lng = ~LONGITUD, lat = ~LATITUD, group = "Semaforo Amarillo",
             popup = ~paste(MUNICIPIO, CUENCA, CUERPO.DE.AGUA, TIPO, SEMAFORO, sep = '<br/>'),
             icon = list (
               iconUrl = paste(path, "water-drop-yellow.png", sep=""),
               iconSize = c(30,30)
             )) %>%
  addMarkers(data = fuentes_sem_r, lng = ~LONGITUD, lat = ~LATITUD, group = "Semaforo Rojo",
             popup = ~paste(MUNICIPIO, CUENCA, CUERPO.DE.AGUA, TIPO, SEMAFORO, sep = '<br/>'),
             icon = list (
               iconUrl = paste(path, "water-drop-red.png", sep=""),
               iconSize = c(30,30)
             )) %>% 
  addLayersControl(
    overlayGroups = c("Semaforo Verde", "Semaforo Amarillo", "Semaforo Rojo")
  )

fcap_aux = read.csv(paste(path, "fuentes-captacion-2018.csv", sep=""))
fcap = fcap_aux[c('fnt_tcap','fnt_ppro', 'fnt_m3', 'capa_ext', 
                  'hors_ope','dias_ope', 'fnt_kwh', 'fnt_peso')]

fcap$fnt_tcap = factor(fcap$fnt_tcap)

#Nombres entendibles
names(fcap)[1] <- "Tipo"
names(fcap)[2] <- "Profundidad"
names(fcap)[3] <- "Vol_extraido"
names(fcap)[4] <- "Capacidad_extraccion"
names(fcap)[5] <- "Prom_horas_ope"
names(fcap)[6] <- "Dias_ope"
names(fcap)[7] <- "Consumo_elect"
names(fcap)[8] <- "Consumo_elect_p"

#Tipo de fuente
tipos_de_fuente = c("no-data", "Pozo" ,"Rio", "Presa","Galeria_filtrante","Manantial","Canal/dren","Cenote","Otra")

ggparcoord(fcap, columns = 2:8, scale="uniminmax",
           alphaLines = 0.4, groupColumn = "Tipo")  


ggparcoord(filter(fcap, Tipo != '1'), columns = 2:8, scale="uniminmax",
           alphaLines = 0.4, groupColumn = "Tipo")  

#########
# 
# fnt_cap_id	fn_cap_des
# 0	No aplica/Sin respuesta
# 1	Pozo
# 2	Río
# 3	Presa
# 4	Galería filtrante
# 5	Manantial
# 6	Canal o dren
# 7	Cenote
# 8	Otra

#Heat map
fcapheat <- fcap[2:8]
str(fcapheat)
correM <- cor(fcapheat)
correM
correM2 <- melt(correM)
correM2
corrheat <- ggplot(correM2, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()

ggplotly(corrheat)


# gantt 
gantt_aux <- data.frame(actividad=c("Nuevos_datos", "Analisis",
                                    "Escribir-Articulo", "publicar" ), 
                        inicio=c("2022-06-11", "2022-06-14",
                                 "2022-06-21","2022-06-30"),
                        fin=c("2022-06-14", "2022-06-23",
                              "2022-06-29", "2022-07-01"),
                        lugar_de_trabajo=c("depa", "depa", "casa", "casa"))

gantt_aux

df_gantt <- gantt_aux %>% 
  mutate(inicio = as.Date(inicio), fin = as.Date(fin)) %>%
  pivot_longer(c(-actividad,-lugar_de_trabajo), names_to = "InicioFin",
               values_to = "Fechas")

df_gantt
gantt <- ggplot(df_gantt, aes(x=actividad, y=Fechas)) +
  geom_line(aes(x=fct_rev(fct_inorder(actividad)),
                y = Fechas, color=lugar_de_trabajo),
              size = 8) + coord_flip() + labs(title="Adios al verano :c",
                                              x = "Tareas", y = "Fechas 2022")







