#"File" -> "Reopen with encoding" -> "UTF-8", para a acentuação dos comentários aparecer corretamente

setwd("C:/Users/aline/Dropbox/Documentos/Artigos e etc/Oficinas R/Oficina R 2023") # escolher seu diretório de trabalho 
# Cole o caminho da pasta onde está salvo os arquivos. 
# A barra \ deve ser substituída por / ou por \\

getwd() # verificar qual é o diretório de trabalho salvo



#### Calculadora

1 + 1 

18327 - 7981

24/7

5*78

14^2

soma <- 234 +458 # salvar objetos no environment

rm(soma) # excluir objetos do environment



#### Explorar base de dados

install.packages("data.table") # instalar pacotes
library(data.table) # carregar pacotes
# Vamos utilizar o pacote data.table para importar dados. 
# O data.table não é o único pacote para este fim; existem vários pacotes para importar dados.

download.file("https://raw.githubusercontent.com/alinedalcin/oficinaR2023/main/banco_mundial.csv", "banco_mundial.csv") # salvar base de dados no diretório de trabalho
banco_mundial <- fread("banco_mundial.csv") # importar base de dados que está salva no diretório de trabalho.


#Como montar o banco de dados no R, baixando os dados diretamente do Banco Mundial?

#install.packages(c("wbstats", "countrycode", "dplyr")) # pacotes para baixar dados diretamente do site do banco mundial
#library(wbstats)
#library(countrycode)
#library("dplyr")

#indicadores <- c("pib_dolares_precos_correntes" = "NY.GDP.MKTP.PP.CD",
#                 "populacao" = "SP.POP.TOTL",
#                 "expectativa_de_vida_ao_nascer_mulheres" = "SP.DYN.LE00.FE.IN",
#                 "expectativa_de_vida_ao_nascer_homens" = "SP.DYN.LE00.MA.IN",
#                 "taxa_de_fertilidade" = "SP.DYN.TFRT.IN",
#                 "nascimentos_por_1000_mulheres_entre_15_e_19_anos" = "SP.ADO.TFRT",
#                 "dias_necessarios_para_abrir_negocio" = "IC.REG.DURS",
#                 "proporcao_de_mulheres_no_parlamento_nacional" = "SG.GEN.PARL.ZS")

#banco_mundial <- wb_data(indicadores, start_date = 2019, end_date = 2019)

#indicadores2 <- data.frame("iso3c" = wb_cachelist$countries$iso3c, 
#                           "renda" = wb_cachelist$countries$income_level,
#                           "regiao" = wb_cachelist$countries$region)

#banco_mundial <- left_join(banco_mundial, indicadores2)

#write.csv(banco_mundial, "banco_mundial.csv")
#save(banco_mundial, file = "banco_mundial.RData")


print(banco_mundial) # apresenta toda a base de dados
View(banco_mundial) # abre o banco em outra janela

str(banco_mundial) # apresenta a estrutura da base de dados: dimensão, nomes, classe, dados iniciais...

dim(banco_mundial) # apresenta a dimensão da base de dados

names(banco_mundial) # apresenta os nomes das variáveis da base de dados

class(banco_mundial) # apresenta a classe da base de dados ou da variável
class(banco_mundial$regiao)
class(banco_mundial$populacao)
class(banco_mundial$dias_necessarios_para_abrir_negocio)
# para transformar a classe de uma variável: as.character(), as.numeric(), as.logical(), as.factor(), as.Date(), etc...
banco_mundial$dias_necessarios_para_abrir_negocio_inteiro <- as.integer(banco_mundial$dias_necessarios_para_abrir_negocio)
class(banco_mundial$dias_necessarios_para_abrir_negocio_inteiro)

head(banco_mundial, 10) # apresenta primeiras observações
head(banco_mundial$country, 10)
head(banco_mundial$taxa_de_fertilidade, 10)

tail(banco_mundial, 10) # apresenta últimas observações
tail(banco_mundial$country, 10)
tail(banco_mundial$taxa_de_fertilidade)

table(banco_mundial$renda) # apresenta tabela dos resultados
table(banco_mundial$regiao)

summary(banco_mundial) # resumo da base de dados por variavável
summary(banco_mundial$country)
summary(banco_mundial$taxa_de_fertilidade)

mean(banco_mundial$taxa_de_fertilidade) # apresenta a média da variável
mean(banco_mundial$taxa_de_fertilidade, na.rm=TRUE) # apresenta a média da variável
# Para descartar os missings (NA), colocar na.rm = TRUE 
# Se tiver missing, o resultado será NA
# Outras funções: variância (var), mínimo (min), máximo (max), amplitude (range), mediana (median), quantis (quantile)

any(is.na(banco_mundial$taxa_de_fertilidade)) # saber se existe algum missing (NA)
sum(is.na(banco_mundial$taxa_de_fertilidade)) # saber quantos missings (NA) existem 
any(banco_mundial$taxa_de_fertilidade==3, na.rm=TRUE) 
sum(banco_mundial$taxa_de_fertilidade==3, na.rm=TRUE) 
#Utilizar outras funções is: por exemplo, is.numeric
#Utilizar igualdades ou desigualdades:  >, >=, <, <=, ==, !=



#### Pacote dplyr: 
#selecionar variáveis (select), criar variáveis a partir de outras (mutate), filtrar base de dados (filter), ordenar dados (arrange), agrupar variáveis por soma, média, contagem, etc (summarise)

install.packages("dplyr")
library(dplyr) #pacote para manusear dados

exemplo_filter1 <- filter(banco_mundial, !is.na(banco_mundial$taxa_de_fertilidade)) 
exemplo_filter2 <- filter(banco_mundial, taxa_de_fertilidade>=2) 
exemplo_filter3 <- filter(banco_mundial, taxa_de_fertilidade>=2&regiao!="Sub-Saharan Africa")

# Mais de uma condição: & (para e), | (para ou)

exemplo_mutate <- mutate(banco_mundial, pib_per_capita=pib_dolares_precos_correntes/populacao) 

exemplo_select <- select(banco_mundial, c("country", "regiao", "pib_dolares_precos_correntes")) 

exemplo_arrange <- arrange(banco_mundial, regiao) 

# Ordem decrescente: desc()

exemplo_summarise1 <- summarise(banco_mundial, media=mean(expectativa_de_vida_ao_nascer_mulheres, na.rm=TRUE), minimo=min(expectativa_de_vida_ao_nascer_mulheres, na.rm=TRUE), maximo=max(expectativa_de_vida_ao_nascer_mulheres, na.rm=TRUE))
exemplo_summarise2 <- banco_mundial %>%
  group_by(regiao) %>%
  summarise(media=mean(expectativa_de_vida_ao_nascer_mulheres, na.rm=TRUE), minimo=min(expectativa_de_vida_ao_nascer_mulheres, na.rm=TRUE), maximo=max(expectativa_de_vida_ao_nascer_mulheres, na.rm=TRUE))

exemplo_geral <- banco_mundial %>% 
  filter(!is.na(banco_mundial$pib_dolares_precos_correntes))%>%
  mutate(pib_per_capita=pib_dolares_precos_correntes/populacao)%>%
  group_by(regiao)%>%
  summarise(media=mean(pib_per_capita))%>%
  arrange(regiao)



#### Visualização de dados

install.packages("ggplot2")
library(ggplot2) #pacote para gráficos

#banco_mundial <- fread("banco_mundial.csv")
#rm(exemplo_filter1, exemplo_filter2, exemplo_filter3, exemplo_geral, exemplo_mutate, exemplo_select, exemplo_summarise1, exemplo_summarise2)

banco_mundial <- mutate(banco_mundial, pib_per_capita=pib_dolares_precos_correntes/populacao)

## Passo 1: Criar gráfico
ggplot(banco_mundial)+
  geom_point(aes(x=pib_per_capita, y=expectativa_de_vida_ao_nascer_homens))

## Passo 2: Escala logaritimica em x
ggplot(banco_mundial)+
  geom_point(aes(x=pib_per_capita, y=expectativa_de_vida_ao_nascer_homens))+
  scale_x_log10()

## Passo 3: Tirar a notação da escala de x
ggplot(banco_mundial)+
  geom_point(aes(x=pib_per_capita, y=expectativa_de_vida_ao_nascer_homens))+
  scale_x_log10(labels=function(x) format(x, scientific = FALSE))
  
## Passo 4: Nomes dos eixos e títulos
ggplot(banco_mundial)+
  geom_point(aes(x=pib_per_capita, y=expectativa_de_vida_ao_nascer_homens))+
  scale_x_log10(labels=function(x) format(x, scientific = FALSE))+
  labs(y="Expectativa de vida ao nascer para os Homens", x="PIB per capita (base log)", title="Correlação entre expectativa de vida ao nascer e PIB per capita")
  
## Passo 5: Alterar características
ggplot(banco_mundial)+
  geom_point(aes(x=pib_per_capita, y=expectativa_de_vida_ao_nascer_homens, color=regiao, size=populacao))+
  scale_x_log10(labels=function(x) format(x, scientific = FALSE))+
  labs(y="Expectativa de vida ao nascer para os Homens", x="PIB per capita (base log)", title="Correlação entre expectativa de vida ao nascer e PIB per capita", color="regiao", size="População")

# Funções: geom_point, geom_text, geom_col, geom_line, geom_smooth
# características: x, y, color, fill, size, alpha, linetype, label, shape, width

## Passo 6: Outros passos
ggplot(banco_mundial)+
  geom_point(aes(x=pib_per_capita, y=expectativa_de_vida_ao_nascer_homens, fill=regiao, size=populacao), shape=21, color="black", alpha=0.7)+
  geom_smooth(aes(x=pib_per_capita, y=expectativa_de_vida_ao_nascer_homens), method="lm", se=FALSE, colour="black")+
  geom_text(data=subset(banco_mundial, country=="Brazil"|country=="China"|country=="India"), aes(x=pib_per_capita, y=expectativa_de_vida_ao_nascer_homens, label=paste(iso3c)), size=4, colour="black")+
  scale_x_log10(labels=function(x) format(x, scientific = FALSE))+
  scale_size_continuous(range = c(2, 10))+
  labs(y="Expectativa de vida ao nascer para os Homens", x="PIB per capita (base log)", fill="Legenda:", title="Correlação entre expectativa de vida ao nascer e PIB per capita")+
  guides(fill = guide_legend(override.aes = list(size=4)), size=FALSE)+
  theme(legend.position = "bottom")

