library(tidyverse)
library(leaflet)
library(ggmap)
library(readxl)
library(leaflet.extras)

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
  
  
  casas = read_rds("C:/Users/Steven/Documents/GitHub/Mapa-Cultural/BD.rds")


#Tratar a base de dados para que remova os endereços que a latitude e longitude deram NA(Depois terei que 
#ver o porquê desses endereços retornaram NA)  
  casas2 = casas %>% 
    filter(!is.na(longitude))
  
#Plotar o mapa(Obtivemos alguns pontos estranhos)
  mapaPontos = leaflet() %>% 
    addTiles() %>% 
    addMarkers(lng = as.numeric(casas2$longitude),lat =  as.numeric(casas2$latitude), popup = casas2$lugar)
  mapaCalor = leaflet() %>% 
    addTiles() %>% 
    addHeatmap(lng = as.numeric(casas2$longitude),lat =  as.numeric(casas2$latitude))
  

    
