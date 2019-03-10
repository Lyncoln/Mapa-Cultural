library(tidyverse)
library(leaflet)
library(ggmap)
library(readxl)
library(leaflet.extras)
library(rgdal)

#Tratei manualmente alguns endereços na base de dados(Casas.txt) pois estavam apresentando algum tipo de problema no R
#No site do mapa cultural, quando um lugar faz parte de uma rede, ele o agrupa em uma mesma página com vários endereços
#Por exemplo redes de cinemas
#Separei manualmente para que cada localidade possua seu endereço individual.
#Notei que no Teatro Munipal de Niteroi o endereço estava incompleto, então o completei manualmente
#Por algum motivo, algumas palavras na base de dados estão codificadas de maneira estranha, então  eu apaguei
#Essas mensagens e as reescrevi para que o R não apresentar problema



#Registrar a chave da API do google, confidencial 

  register_google(key = "-") 

#Li a base de dados e a tratei e depois criei colunas de longitude e latitude
#coloquei no final dos endereços "Rio de Janeiro" para aqueles endereços que não contem RJ ou Rio de janeiro
  casas = read_csv2("F://GitHub//Mapa-Cultural//Casas.txt"
                 ,col_names = F,locale = locale(encoding = 'ISO-8859-1'))
  casas = casas %>% 
    rename(lugar = X1, endereco = X2)
  casas = casas %>% 
    mutate(
          latitude = "999",
          longitude = "999")
  qtd = dim(casas)[1]
  for(i in 1:qtd){
    if(!str_detect(casas$endereco[i],c("Rio de Janeiro")) & !str_detect(casas$endereco[i],c("RJ"))  ){
      casas$endereco[i] = paste(casas$endereco[i],", Rio de Janeiro")
    }
  }
  
#Procedimento que busca latitude e longitude dos endereços presentes no banco de dados(está retornando 2 endereços
#  com NA)  
  for ( i in 1:qtd){
    aux = geocode(as.character(casas$endereco[i])) 
    longitudeAux = aux$lon
    latitudeAux = aux$lat
    casas$longitude[i] = longitudeAux
    casas$latitude[i] = latitudeAux
  }
#Salvar o banco de dados pois a API do google é limitada  
  write_rds(casas,"F://GitHub//Mapa-Cultural//BD.rds")
  casas = read_rds("F://GitHub//Mapa-Cultural//BD.rds")


#Tratar a base de dados para que remova os endereços que a latitude e longitude deram NA(Depois terei que 
#ver o porquê desses endereços retornaram NA)  
  casas2 = casas %>% 
    filter(!is.na(longitude))

#Ler a malha dos municípios do RJ
RiodeJaneiro = readOGR(dsn="F://GitHub//Mapa-Cultural//Malha_shp",layer="33MUE250GC_SIR",
                       use_iconv = TRUE,
                       encoding = "UTF-8")
plot(RiodeJaneiro)
#Plotar o mapa(Obtivemos alguns pontos estranhos)
  mapaCirculos = leaflet(RiodeJaneiro,
                         options = leafletOptions(minZoom = 9)) %>% 
    addTiles() %>% 
    setView(lng=-42.5303, lat=-22.1, zoom = 9) %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.2,
              fillColor = 'blue',
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = FALSE),
              popup = RiodeJaneiro$NM_MUNICIP) %>%
    addCircles(lng = as.numeric(casas2$longitude),
               lat =  as.numeric(casas2$latitude),
               popup = casas2$lugar,color = "Red") %>% 
    addProviderTiles("Esri.WorldImagery")
    
  mapaCalor = leaflet(RiodeJaneiro,
                      options = leafletOptions(minZoom = 9)) %>% 
    setView(lng=-42.5303, lat=-22.1, zoom = 9) %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.2,
                fillColor = 'blue',
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = FALSE),
                popup = RiodeJaneiro$NM_MUNICIP) %>%
    addHeatmap(lng = as.numeric(casas2$longitude),lat =  as.numeric(casas2$latitude),radius = 10) %>% 
    addProviderTiles("Esri.WorldImagery")
  
  mapaGeral = leaflet(RiodeJaneiro,
                      options = leafletOptions(minZoom = 9)) %>% 
    addTiles() %>% 
    setView(lng=-42.5303, lat=-22.1, zoom = 9) %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.2,
                fillColor = 'blue',
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = FALSE),
                popup = RiodeJaneiro$NM_MUNICIP) %>%
    addCircles(lng = as.numeric(casas2$longitude),
               lat =  as.numeric(casas2$latitude),
               popup = casas2$lugar,color = "Red",
               group = "Circulos") %>% 
    addHeatmap(lng = as.numeric(casas2$longitude),
               lat =  as.numeric(casas2$latitude)
               ,radius = 10,
               group = "Calor") %>% 
    addProviderTiles(providers$Esri.WorldImagery,group = "Esri.WorldImagery") %>% 
    addProviderTiles(providers$CartoDB,group = "CartoDB") %>% 
    addLayersControl(overlayGroups = c("Circulos","Calor"),
                     baseGroups = c("Esri.WorldImagery","CartoDB"),
                     options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup("Calor")
    
#Os pontos que estão estranhos são : 
errados = casas %>% filter(lugar == "Museu de Cera de Petrópolis" | 
                             lugar=="Centro Cultural Bernardino Lopes"|
                             lugar=="Museu do Cárcere" |
                             lugar =="Biblioteca Leonor Leite Bastos de Souza" |
                             lugar=="Associação Sociocultural e Ambiental de Triunfo" | 
                             lugar == "Biblioteca Córrego do Ouro")    
#Vamos consertar os endereços : 
errados$endereco = if_else(errados$lugar =="Museu de Cera de Petrópolis",
                  "Rua Barão do Amazonas, 35 - Centro, Petrópolis - RJ, 25685-070",
                  errados$endereco)#
errados$endereco = if_else(errados$lugar == "Associação Sociocultural e Ambiental de Triunfo",
                           "R Abdo Felix, Sn Santa Maria Madalena - RJ",
                           errados$endereco)#
errados$endereco = if_else(errados$lugar == "Museu do Cárcere",
                           "RR95+35 Dois Rios, Angra dos Reis - RJ",
                           errados$endereco)#  
errados$endereco = if_else(errados$lugar == "Biblioteca Leonor Leite Bastos de Souza",
                           "35JJ+2M Eldorado, Maricá - RJ",
                           errados$endereco)
errados$endereco = if_else(errados$lugar == "Centro Cultural Bernardino Lopes",
                           "Rua Alexandre Pereira Dos Santos, sn - Boa Esperança, Rio Bonito - RJ, 28810-000",
                           errados$endereco)
errados$endereco = if_else(errados$lugar == "Biblioteca Córrego do Ouro",
                           "Rua Prefeito Antônio Curvelo Benjamin, 226-232,Macaé, RJ",
                           errados$endereco) #                          
#Gerar o geocode : 
for ( i in 1:6){
  aux = geocode(as.character(errados$endereco[i])) 
  longitudeAux = aux$lon
  latitudeAux = aux$lat
  errados$longitude[i] = longitudeAux
  errados$latitude[i] = latitudeAux
}


