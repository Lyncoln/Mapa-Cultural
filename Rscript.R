library(tidyverse)
library(leaflet)
library(ggmap)
library(readxl)

register_google(key = "------------------------------") 

casas = read_csv2("F://GitHub//Mapa-Cultural//Casas.txt"
                 ,col_names = F,locale = locale(encoding = 'ISO-8859-1'))

casas = casas %>% 
  rename(lugar = X1, endereco = X2)

casas = casas %>% 
  mutate(endereco = paste(endereco," Rio de Janeiro"),
         latitude = "999",
         longitude = "999")

qtd = dim(casas)[1]

for ( i in 1:qtd){
  aux = geocode(as.character(casas$endereco[i])) 
  longitudeAux = aux$lon
  latitudeAux = aux$lat
  casas$longitude[i] = longitudeAux
  casas$latitude[i] = latitudeAux
  
}

write_rds(casas,"F://GitHub//Mapa-Cultural//BD.rds")
teste = geocode("Brazil")
teste$lon
