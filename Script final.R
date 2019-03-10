#Bibliotecas :
  library(tidyverse); library(leaflet); library(ggmap); library(readxl); library(leaflet.extras); library(rgdal)

#Corrigi alguns erros de enconding no txt gerado pelo python manualmente.  
#No site do mapa cultural, quando um lugar faz parte de uma rede, ele o agrupa em uma mesma página com vários endereços
#Por exemplo redes de cinemas. Separei manualmente para que cada localidade possua seu endereço individual.
#Por algum motivo, algumas palavras na base de dados estão codificadas de maneira estranha, então  eu apaguei
#Essas mensagens e as reescrevi para que o R não apresentar problema.
  
#Registrar a chave da API do google, confidencial.
  register_google(key = "-") 
  
#Li a base de dados e criei colunas de longitude e latitude.
#coloquei no final dos endereços "Rio de Janeiro" para aqueles endereços que não contem "RJ" ou "Rio de janeiro".
  
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

#Por meio de testes feitos em outro script, notei que há poucos pontos em que estão em endereços errados no mapa.
#então iremos corrigir.
  errados = casas %>% filter(lugar == "Museu de Cera de Petrópolis" | 
                               lugar=="Centro Cultural Bernardino Lopes"|
                               lugar=="Museu do Cárcere" |
                               lugar =="Biblioteca Leonor Leite Bastos de Souza" |
                               lugar=="Associação Sociocultural e Ambiental de Triunfo" | 
                               lugar == "Biblioteca Córrego do Ouro" |
                               (lugar == "Casa de Cultura" & endereco == "Praça Orlando de Barros Pimentel, s/nº, Centro. , Rio de Janeiro"))  
  
  errados$endereco = if_else(errados$lugar =="Museu de Cera de Petrópolis",
                             "Rua Barão do Amazonas, 35 - Centro, Petrópolis - RJ, 25685-070",
                             errados$endereco)
  errados$endereco = if_else(errados$lugar == "Associação Sociocultural e Ambiental de Triunfo",
                             "R Abdo Felix, Sn Santa Maria Madalena - RJ",
                             errados$endereco)
  errados$endereco = if_else(errados$lugar == "Museu do Cárcere",
                             "RR95+35 Dois Rios, Angra dos Reis - RJ",
                             errados$endereco)
  errados$endereco = if_else(errados$lugar == "Biblioteca Leonor Leite Bastos de Souza",
                             "35JJ+2M Eldorado, Maricá - RJ",
                             errados$endereco)
  errados$endereco = if_else(errados$lugar == "Centro Cultural Bernardino Lopes",
                             "Rua Alexandre Pereira Dos Santos, sn - Boa Esperança, Rio Bonito - RJ, 28810-000",
                             errados$endereco)
  errados$endereco = if_else(errados$lugar == "Biblioteca Córrego do Ouro",
                             "Rua Prefeito Antônio Curvelo Benjamin, 226-232,Macaé, RJ",
                             errados$endereco) 
  errados$endereco = if_else(errados$lugar == "Casa de Cultura",
                             " R. Álvares de Castro, 154 - Centro, Maricá - RJ, 24942-395",
                             errados$endereco) 
  
#Agora iremos corrigirr os pontos que deram NA na longitude e latitude.
  nas = casas %>% 
    filter(is.na(latitude))
  
  nas$endereco = if_else(nas$lugar == "Ateliê Artimpério",
                         "Rua Barão de Vassouras, 19,Casario Shopping , Rio de Janeiro",
                         nas$endereco)
  nas$endereco = if_else(nas$lugar == "Talentos da Roça, Cultura e Cidadania",
                         "Rua Geraldino Silva, Porciúncula - RJ",
                         nas$endereco)
  
#Vamos juntar a base de dados dos pontos errados.
  BD_corrigido = full_join(nas,errados)
  
#Vamos gerar o geocode para o BD_corrigido.  
  for ( i in 1:dim(BD_corrigido)[1]){
    aux = geocode(as.character(BD_corrigido$endereco[i])) 
    longitudeAux = aux$lon
    latitudeAux = aux$lat
    BD_corrigido$longitude[i] = longitudeAux
    BD_corrigido$latitude[i] = latitudeAux
  }
  
  
#Irei apagar do tibble casas os nomes com endereços errados. 
#Terei um tratamento diferenciado para Casa de cultura pois há mais de 1 lugar chamado assim no Banco de dados
  
  nomes_corrigir = BD_corrigido %>% 
    filter(lugar != "Casa de Cultura") %>%
    select(lugar) %>% 
    pull()
  
  casas = casas[-538,]
  casas = casas %>% 
    filter(!(lugar %in% nomes_corrigir))
  

  
#Agora iremos juntar todos os lugares em um só banco de dados todo corrigido e salva-lo,pois é custoso
#rodar o código porque utilizamos a API do google.
  casas_corrigidas = full_join(casas,BD_corrigido)
  write_rds(casas_corrigidas,"F://GitHub//Mapa-Cultural//casas_corrigidas.rds")
  casas_corrigidas = read_rds("F://GitHub//Mapa-Cultural//casas_corrigidas.rds")
  
#Ler a malha dos municípios do RJ
  RiodeJaneiro = readOGR(dsn="F://GitHub//Mapa-Cultural//Malha_shp",layer="33MUE250GC_SIR",
                         use_iconv = TRUE,
                         encoding = "UTF-8")
  
#Agora iremos plotar o mapa final 
  
  mapaGeral = leaflet(RiodeJaneiro,
                      options = leafletOptions(minZoom = 1)) %>% 
    addTiles() %>% 
    setView(lng=-42.5303, lat=-22.1, zoom = 9) %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.2,
                fillColor = 'blue',
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = FALSE),
                popup = RiodeJaneiro$NM_MUNICIP) %>%
    addCircles(lng = as.numeric(casas_corrigidas$longitude),
               lat =  as.numeric(casas_corrigidas$latitude),
               popup = casas_corrigidas$lugar,color = "Red",
               group = "Circulos") %>% 
    addHeatmap(lng = as.numeric(casas_corrigidas$longitude),
               lat =  as.numeric(casas_corrigidas$latitude)
               ,radius = 10,
               group = "Calor") %>% 
    addProviderTiles(providers$Esri.WorldImagery,group = "Esri.WorldImagery") %>% 
    addProviderTiles(providers$CartoDB,group = "CartoDB") %>% 
    addLayersControl(overlayGroups = c("Circulos","Calor"),
                     baseGroups = c("Esri.WorldImagery","CartoDB"),
                     options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup("Calor")

  