#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(leaflet)
library(htmltools)
library(shiny)

# Carga de los datos
# Fuente datos brutos: https://www.meetup.com/es-ES/topics/r-project-for-statistical-computing/all/
# Las coordenadas se han obtenido con la API Geocode de Gooolge Cloud
# (en archivo aparte, codigo para consultar con R "Geocode Google API.R")
# Importante el encoding, la instancia de Shiny.rstudio trabaja en UTF-8
# Hay que leer con el encoding original y luego convertir a UTF-8
Meetups.R.World <- read.csv("Meetups R World.csv", sep=";", stringsAsFactors=FALSE, encoding = 'ISO8859-1')

# Cambio a numeric, aunque ya lo eran. Es recomedable para que no de problemas DataTable en Shiny
Meetups.R.World[,c('id', 'lon', 'lat', 'Miembros')] <- Meetups.R.World %>% select(id, lon, lat, Miembros) %>% mutate_all(as.numeric)

# Conversion del Unicode a UTF-8
Meetups.R.World$Pais <- iconv(Meetups.R.World$Pais, from = 'ISO8859-1', to = 'UTF-8')
Meetups.R.World$Ciudad <- iconv(Meetups.R.World$Ciudad, from = 'ISO8859-1', to = 'UTF-8')

# Funcion para crear varias lineas en el pop-up de Leaflet
labs <- lapply(seq(nrow(Meetups.R.World)), function(i) {
  paste0( '<p>', Meetups.R.World[i, "Grupo"], '<p></p>', 
          Meetups.R.World[i, "Ciudad"], ', ', 
          Meetups.R.World[i, "Pais"],'</p><p>', 
          Meetups.R.World[i, "Miembros"], '</p>' )
})

# Opciones generales de DT (DataTable)
# Lenguaje: Spanish. Realza caracteres buscados. Muestra 10 registros por pagina 
options(DT.options = list(
  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
  searchHighlight = TRUE,
  pageLength = 10,
  columnDefs = list(list(width = 40, targets = list(0, 2, 5)))
))


# Define UI
ui <- fluidPage(
  h3("R Meetups in the World"),
  leafletOutput("mymap", width = '100%', height = 600),
  DT::dataTableOutput('Meetups.R.World')
)

# Define server
server <- function(input, output) {
  
  output$mymap <- renderLeaflet({
    leaflet(data = Meetups.R.World) %>% 
      addProviderTiles('Esri.WorldStreetMap') %>%
      # Mas opciones de ProviderTiles: https://leaflet-extras.github.io/leaflet-providers/preview/
      setView(lng = -5.9844589, lat = 37.3890924, zoom = 3) %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addMarkers(~lon, ~lat, popup = ~as.character(id),
                 label = lapply(labs, HTML),
                 clusterOptions = markerClusterOptions())
  })
  
  output$Meetups.R.World = DT::renderDataTable({
    Meetups.R.World
  }, server = FALSE, filter = 'top', rownames = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

