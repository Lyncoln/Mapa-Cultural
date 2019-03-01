
# carregar a malha do rio
library(rgdal)
RiodeJaneiro<-readOGR(dsn="C:/Users/Steven/Documents/GitHub/Mapa-Cultural/Malha_shp",layer="33MUE250GC_SIR")
# Teste da malha do rio
plot(RiodeJaneiro)

# visualizacao da malha com o leaflet 
library(leaflet)
leaflet(RiodeJaneiro) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = 'red',
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addProviderTiles("Esri.WorldImagery")


leaflet(RiodeJaneiro) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.2,
              fillColor = 'blue',
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addProviderTiles("Esri.WorldImagery")
