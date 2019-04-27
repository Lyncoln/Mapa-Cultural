banco = readRDS(url("https://github.com/Lyncoln/Mapa-Cultural/raw/master/casas_corrigidas.rds"))
banco

#revgeocode(c(as.numeric(banco$longitude[100]),as.numeric(banco$latitude[100])))
library(stringr)
library(ggmap)
library(tidyverse)

           
register_google(key = "-API AQUI-")




str_split_fixed(teste, ",",n=5)[3]


banco$Cidade = revgeocode(c(as.numeric(banco$longitude),as.numeric(banco$latitude)))

#De acordo com Leo, testar

banco<-banco %>% mutate(cidade = revgeocode(c(as.numeric(longitude),as.numeric(latitude))) )

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

library(readr)

#ad <- read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv")
#head(ad)
BasesMunicipios <- read_delim("https://raw.githubusercontent.com/DATAUNIRIO/Base_de_dados/master/Municipios.csv", 
                         ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                         trim_ws = TRUE)

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

tabela <- edit(tabela)

#tabela[11,1]<-"armação dos búzios"
#tabela[24,1]<- "engenheiro paulo de frontin"
#tabela[82,1]<- "trajano de moraes"

saveRDS(tabela,file = "tabela.RDS")

tabela$Munic<-paste0(tabela$munic," (rj)")




library(readxl)
library(dplyr)
library(stringr)
BasesMunicipios <- read_excel("~/GitHub/Base_de_dados/BasesMunicipios.xlsx")
tabela<-readRDS("C:/Users/Steven/Documents/DIRETORIO DE TRABALHO DO R/tabela.RDS")
View(BasesMunicipios)
View(tabela)

BasesMunicipios$Munic = BasesMunicipios$Munic %>% 
  str_to_lower()
BasesMunicipios<-BasesMunicipios %>% full_join(tabela)

BasesMunicipios$eqqculturalpc<-(100000*BasesMunicipios$eqqcultural/BasesMunicipios$Populacao)
summary(BasesMunicipios$eqqculturalpc)
summary(BasesMunicipios$IDH)

BasesMunicipios$eqqculturalpc[is.na(BasesMunicipios$eqqculturalpc)] <- 0              # Replace by 0

cor(BasesMunicipios$eqqculturalpc,BasesMunicipios$IDH,method = "spearman")
cor.test(BasesMunicipios$eqqculturalpc,BasesMunicipios$IDH,method = "spearman")

library(ggplot2)
library(ggthemes)
ggplot(data = BasesMunicipios) +
  aes(x = IDH, y = eqqculturalpc, color = Regiao, size = Renda_per_capita) +
  geom_point() +
  scale_colour_viridis_d(option  = "magma") +
  labs(title = "",
    x = "IDH",
    y = "Eqq cultural")+
    #subtitle = "bbb") +
  theme_solarized()
