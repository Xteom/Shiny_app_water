library(shiny)
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
library(shinythemes)

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


apm = read.csv(paste(path, "apm.csv", sep=""))
#names(apm)[names(apm)=="?..Year"] <- "Year"
names(apm)[1] <- "Year"
apm$Population <- gsub("\\%","",apm$Population)
apm$Population <- as.numeric(as.character(apm$Population))
apm$Annual_Change <- gsub("\\%","",apm$Annual_Change)
apm$Annual_Change <- as.numeric(as.character(apm$Annual_Change))
apm$Year <- as.numeric(as.character(apm$Year))


appaises = read.csv(paste(path, "appaises.csv", sep=""))
#names(appaises)[names(appaises)=="ï..Country.Name"] <- "Country"
names(appaises)[1] <- "Country"
names(appaises)[names(appaises)=="X..of.Population"] <- "Population"
appaises$Population <- gsub("\\%","",appaises$Population)
appaises$Population <- as.numeric(as.character(appaises$Population))
appaises1 = rbind(appaises %>% slice(1:5),appaises %>% slice(15:19) , appaises %>% slice(25:27) )


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

fuentes = read.csv(paste(path, "fuentes_agua.csv", sep=""))

fuentes_sem_v = filter(fuentes, SEMAFORO == 'Verde')
fuentes_sem_a = filter(fuentes, SEMAFORO == 'Amarillo')
fuentes_sem_r = filter(fuentes, SEMAFORO == 'Rojo')



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



#Heat map
fcapheat <- fcap[2:8]
str(fcapheat)
correM <- cor(fcapheat)
correM2 <- melt(correM)
corrheat <- ggplot(correM2, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()


#Gantt
gantt_aux <- data.frame(actividad=c("Nuevos_datos", "Analisis",
                                    "Escribir-Articulo", "publicar" ), 
                        inicio=c("2022-06-11", "2022-06-14",
                                 "2022-06-21","2022-06-30"),
                        fin=c("2022-06-14", "2022-06-23",
                              "2022-06-29", "2022-07-01"),
                        lugar_de_trabajo=c("depa", "depa", "casa", "casa"))


df_gantt <- gantt_aux %>% 
  mutate(inicio = as.Date(inicio), fin = as.Date(fin)) %>%
  pivot_longer(c(-actividad,-lugar_de_trabajo), names_to = "InicioFin",
               values_to = "Fechas")



### Datos prueba
datasets <- c("agua_municipio", "apm", "appaises", "fuentes", "fcap")


### Extra
set.seed(3)

ui <- fluidPage(theme = shinytheme("cerulean"),
  navbarPage("Agua Potable en México", id="menu",
             tabPanel("Municipios con Agua en Mexico", 
                      mainPanel(
                        p("Gracias al CENSO NACIONAL DE GOBIERNOS MUNICIPALES Y DEMARCACIONES TERRITORIALES DE LA CIUDAD DE MÉXICO 2019, censo
                        realizado a los gobernadores de cada municipio, se puede ver que, al menos según sus dirigentes, gran parte del país cuenta
                        con acceso a los servicios de agua. Mientras más claro es el azul, mayor porcentaje de poblacion tiene acceso al agua en 
                        ese municipio. 
                        Las regiones en gris no tienen datos disponibles"),
                        sliderInput("A", "Alto", min = 200, max = 800, value = 400),
                        sliderInput("B", "Ancho", min = 200, max = 1200, value = 600),  
                        plotOutput("mapa_muni"),
                        
                      )
             ),
             
             tabPanel("Agua Potable en Mexico", 
                      mainPanel(
                        p("Con los datos obtenidos en del banco mundial se puede ver que en realidad, a pesar de que gran parte de la población
                          tiene acceso a agua, solo el 42.87% tienen acceso a agua potable. Del lado izquierdo se puede ver la comparación con 
                          otros paises (México se encuentra resaltado en rojo), del lado derecho se muestra el incremento que ha habido en el
                          alcance de agua potable en el país a lo largo de los años. 
                          Se puede ver claramente que no solo es poca la población con servicios de agua potable, sino, el incremento a lo 
                          largo de los años ha sido poco significatico y con cada año sido más pequeño."),
                        sliderInput("D", "Ajusta hasta que sean visibles las leyendas", min = 400, max = 1800, value = 600), 
                        plotOutput("grid_ap"),
                        selectInput("col", "Cambiar de color la gráfica", choices = c("default", "viridis", "grey"))
                      )
             ),
             
             tabPanel("Fuentes de Agua en México", 
                      mainPanel(
                        p("El mapa que se muestra a continuacion muestra la fuentes de agua que se encuentran en la república, el color de
                          la gota de agua indica el semáforo que le fue asignado a la fuente según la potabilidad del agua que se obtiene. 
                          Los contaminante que se pueden encontrar en el agua pueden ir ser desde desechos fecales hasta químicos tóxicos.
                          En la esquina superior derecha del mapa es posible seleccionar las fuentes de agua que se desean mostrar."),
                        leafletOutput("fuentes_conagua"),
                        p("Para ahorrarle la molestia de contarlas al lector, de las 3493 fuentes de agua en la republica solo hay 1267
                          con semáforo verde, con semáforo amarillo hay 1135 y con semáforo rojo 1091. Esto quiere decir que solo el 36.27% de
                          las fuentes de agua en el país se podrían cosiderar completamente potables.")
                      )
             ),
             
             tabPanel("Correlación en las Fuentes", 
                      mainPanel(
                        p("A continuacion se muestra la correlación entre algunas de las características de las fuentes de agua"),
                        p("indice de tipos:"),
                        p("0	No aplica/Sin respuesta"),
                        p("1	Pozo"),
                        p("2	Río"),
                        p("3	Presa"),
                        p("4	Galería filtrante"),
                        p("5	Manantial"),
                        p("6	Canal o dren"),
                        p("7	Cenote"),
                        p("8	Otra"),
                        p(""),
                        p("En coordenadas paralelas:"),
                        plotOutput("par_coord"),
                        p(""),
                        p("En un heatmap:"),
                        plotlyOutput("heat")
                      )),
             tabPanel("Planes a Futuro",
                      mainPanel(
                        p("Durante el verano se planea continuar con este proyecto. A continacion un diagrama de gantt con la
                          planeacion en tiempo y en lugar"),
                        plotOutput("gantt")
                      )),
             tabPanel("Read Me",includeMarkdown("readme.md")))
)

server <- function(input, output, session) {
  
  ### Elementos reactivos
  color_select <- reactive({
    input$col
  })
  
  ### Outputs

  output$plot <- renderPlot({
    plot(dataset())
  })
  
  output$result <- renderText({
    which(datasets %in% input$dataset)
  })
  
  output$mapa_muni <- renderPlot(
    height = function() input$A,
    width = function() input$B,
    {
      ggplot(agua_municipio) + 
        geom_polygon(data = mxmunicipio.map, aes(long, lat, group = group),
                     fill = 'grey30', colour = 'grey30', show.legend = FALSE,) + 
        geom_polygon(data = agua_municipio, aes(long, lat, group = group, fill = pobl_prc)) +
        coord_quickmap() +
        ggtitle("Acceso a agua de la red pública") +
        theme(plot.title = element_text(hjust = 0.5))
    })
  
  output$grid_ap <- renderPlot(
    height = 400,
    width = function() input$D,
    {
      if(color_select() == "viridis"){
        bar_paises <- ggplot(appaises1, aes(x=fct_reorder(Country, Population, .desc=TRUE), y=Population,
                                            fill=factor(ifelse(Country=="Mexico","Highlighted","Normal")))) +
          geom_bar(stat="identity", show.legend = FALSE)  + scale_fill_viridis_d() +
          ggtitle("Pobalacion con agua potable en varios paises",) +
          theme(plot.title = element_text(hjust = 0.5))
      } else if(color_select() == "grey"){
        bar_paises <- ggplot(appaises1, aes(x=fct_reorder(Country, Population, .desc=TRUE), y=Population,
                                            fill=factor(ifelse(Country=="Mexico","Highlighted","Normal")))) +
          geom_bar(stat="identity", show.legend = FALSE)  + scale_fill_grey() +
          ggtitle("Pobalacion con agua potable en varios paises",) +
          theme(plot.title = element_text(hjust = 0.5))
        
      }else {
        bar_paises <- ggplot(appaises1, aes(x=fct_reorder(Country, Population, .desc=TRUE), y=Population,
                                            fill=factor(ifelse(Country=="Mexico","Highlighted","Normal")))) +
          geom_bar(stat="identity", show.legend = FALSE)  + 
          ggtitle("Pobalacion con agua potable en varios paises",) +
          theme(plot.title = element_text(hjust = 0.5))
      }
      
      grid.arrange(bar_paises, line_apm, bar_apm, ncol = 2, 
                  layout_matrix = cbind(c(1,1), c(2,3)))
    }
  )
   
  output$fuentes_conagua <- renderLeaflet({
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
  })
  
  output$par_coord <- renderPlot(
    width = 1000,
    {
      ggparcoord(fcap, columns = 2:8, scale="uniminmax",
               alphaLines = 0.4, groupColumn = "Tipo")  
    }
  )
  
  output$heat <- renderPlotly(
    {
      ggplotly(corrheat) %>% layout(height = 500, width = 900)
    }
  )
  
  output$gantt <- renderPlot({
    ggplot(df_gantt, aes(x=actividad, y=Fechas)) +
      geom_line(aes(x=fct_rev(fct_inorder(actividad)),
                    y = Fechas, color=lugar_de_trabajo),
                size = 8) + coord_flip() + labs(title="Adios al verano :c",
                                                x = "Tareas", y = "Fechas 2022")
  })
    
  
}

shinyApp(ui, server)

