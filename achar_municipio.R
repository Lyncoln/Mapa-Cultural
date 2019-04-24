banco = readRDS("C://Users//tpc 02//Desktop//casas_corrigidas.rds")
banco

revgeocode(c(as.numeric(banco$longitude[100]),as.numeric(banco$latitude[100])))
library(stringr)
library(ggmap)
library(tidyverse)
           
register_google(key = "-API AQUI-")




str_split_fixed(teste, ",",n=5)[3]


banco$Cidade = revgeocode(c(as.numeric(banco$longitude),as.numeric(banco$latitude)))

aux = c()
for( i in 1:length(banco$longitude)){
  aux[i] =  revgeocode(c(as.numeric(banco$longitude[i]),as.numeric(banco$latitude[i])))
  
    
    
}


aux2 = c()

for(i in 1:length(aux)){
  aux2[i] = str_split_fixed(aux[i], ",",n=6)[3]
}

teste = tibble(aux = aux, aux2 = aux2)
View(teste)


BasesMunicipios <- read_excel("C:/Users/tpc 02/Desktop/BasesMunicipios.xlsx")


municipios = BasesMunicipios$Munic

teste = as.tibble(municipios) %>% 
  mutate("teste 1 " =str_sub(municipios,1,-6))

municipios_corrigidos = pull(teste[,2])
municipios_corrigidos = municipios_corrigidos %>% 
  str_to_lower()

tratar = aux %>% 
  str_to_lower()

str_detect(tratar[446],municipios_corrigidos[1])



  municipios_corrigidos[5]<-"búzios"
  municipios_corrigidos[26]<- "eng. paulo de frontin"
  municipios_corrigidos[87]<- "trajano de morais"


achar = function(endereco,municipios){
  for(i in 1:length(municipios)){
    if(str_detect(endereco,municipios[i])){
      return(municipios[i])
    }
  }
}

final = c()
for( i in 1:length(tratar)){
  if(is.null(achar(tratar[i],municipios_corrigidos))) ajuda = "NA"
  else ajuda = achar(tratar[i],municipios_corrigidos)
  final[i] = ajuda
  
}


final_final = tibble(endereco = tratar, municipio = final)

View(final_final %>% filter(municipio=="NA") )
View(final_final)
final_final$municipio = if_else(final_final$municipio=="NA","petrópolis",final_final$municipio)
View(final_final %>% filter(municipio=="NA") )

tabela<-table(final_final$municipio)
class(tabela)
tabela<-data.frame(tabela)
colnames(tabela)<-c("munic","eqqcultural")


tabela[11]<-"armação dos búzios"
tabela[24]<- "engenheiro paulo de frontin"
tabela[82]<- "trajano de moraes"
