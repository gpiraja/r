# Analise dados Foz do Chapecó
## Qualidade de água superficial
## Bibliotecas
library(stringr)
library(dplyr)
library(lattice)
library(ggplot2)
###Leitura dos dados
agua_sup <- read.csv("Agua_superficiais_so_num.csv")
agua_sup$Data <- as.Date(agua_sup$Data)
agua_sup$Ponto <- toupper(agua_sup$Ponto)
agua_sup$Ponto <- as.factor(str_trim(agua_sup$Ponto))
agua_sup_txt <- read.csv("Agua_superficiais_so_txt.csv")
# agua_sup_alt corresponde ao arquivo em que os sinais de < >  (em geral referentes aos limites de  
# detecção do método) foram removidos
agua_sup_alt <- read.csv("Agua_superficiais_so_num_alt.csv")
agua_sup_alt$Data <- as.Date(agua_sup_alt$Data)
agua_sup_alt$Ponto <- toupper(agua_sup_alt$Ponto)
agua_sup_alt$Ponto <- as.factor(str_trim(agua_sup_alt$Ponto))

# informações dos pontos de monitoramento
pontos_monitoramento <- read.csv("Pontos_de_monitoramento.csv")
pontos_monitoramento$Ponto <-as.factor(str_trim(toupper(pontos_monitoramento$Ponto)))

# incluir coluna indicando a fase do empreendimento
agua_sup_alt$Barramento <- agua_sup_alt$Data > as.Date("2010-11-01")

vet_pt <- agua_sup_alt$Ponto
vet_pt == pontos_reman1

#removendo pontos com número insignificante de amostras
pontos_sign <- summary(agua_sup_alt$Ponto)>9
pontos_nome<-as.vector(names(summary(agua_sup_alt$Ponto)))
pontos_reman <- pontos_nome[pontos_sign]
pontos_reman1 <- as.factor(pontos_nome[pontos_sign])
agua_sup_sign <- subset(agua_sup_alt, Ponto %in% pontos_reman) ###!!!!!--> %in% <<----


# usando o for para separar os pontos em uma lista e data frames
lista_df <- list()
for (i in pontos_reman) { 
    assign( paste0("df_",i ), agua_sup_alt[agua_sup_alt$Ponto == i,]) #assign!!
    lista_df[[i]] <-  agua_sup_alt[agua_sup_alt$Ponto == i,]
  }



# identificando pontos extremos
# para isso é necessário que se verifique quais os valores são considerados outliers
# considerando o parâmetro e o ponto de monitoramento
####### descobrir como detectar outliers


head(merge(agua_sup, pontos_monitoramento))

agua_sup_OD<- select(agua_sup, c(2, 6))
dfs_pts<-split(df, df[n_coluna])
OD_pts <- unstack(agua_sup_OD,  Oxig_Diss ~ Ponto) ### Unstack!!
OD_pts


for (i in 1:21) { plot(OD_pts[[i]], main = names(OD_pts[i]))}

#criar data.frames separados para cada ponto
galton <- split(agua_sup, agua_sup["Ponto"])

separa_pts <- function(df, n_coluna)
{ dfs_pts<-split(df, df[n_coluna])
  tam_vec <- length(dfs_pts)
for (i in 1:tam_vec) {
  assign (names(dfs_pts[i]), as.data.frame(dfs_pts[i]))}
}
# Loop para separar os pontos de monitoramento
df<- agua_sup_OD
n_coluna<- 2
dfs_pts<-split(df, df[n_coluna])
tam_vec <- length(dfs_pts)
for (i in 1:tam_vec) {
  assign (names(dfs_pts[i]), as.data.frame(dfs_pts[i]))}

# Gráficos automáticos para variáveis
for (i in 4:29) {
  plot(UPB[,i], main = names(UJU2[i]), abline(v = 23))  
}
for (i in 4:29) {
     plot(UPB[,i], main = names(UJU2[i]), abline(v=23))  
   }

