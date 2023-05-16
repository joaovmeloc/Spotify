###### TCC 2 #######

# pacotes utilizados

library(spotifyr)
library(tidyverse)
library(ggridges)
library(httpuv)
library(dplyr)
library(purrr)
library(knitr)
library(lubridate)
library(kableExtra)
library(hms)
library(factoextra)
library(GGally)
library(readxl)

Sys.setenv(SPOTIFY_CLIENT_ID = 'f223560d206544d28c09bae26f0c1517')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '806a29f4658b43cf8ac07296ddcbb9cf')

access_token <- get_spotify_access_token()


# importando o banco de dados que foi gerado no Python

dados1 <- read_xlsx("C:/Users/Joao victor melo/OneDrive/Área de Trabalho/UNB/UnB_2023.1/TCC2/dados_topmusicas.xlsx")


view(dados1)


# modificando a variável "key" (notas) e modificando a variável "duration" para minutos

dadosmusicas <- dados1 %>% 
  as.data.frame() %>%
  mutate(key = recode(as.character(key),
                      "0" = "C",
                      "1" = "C#",
                      "2" = "D",
                      "3" = "D#",
                      "4" = "E",
                      "5" = "F",
                      "6" = "F#",
                      "7" = "G",
                      "8" = "G#",
                      "9" = "A",
                      "10" = "A#",
                      "11" = "B")) %>% 
  mutate(duracao = as_hms(round(duracao / 1000)))
  

view(dadosmusicas)



# medidas resumo





summary(dadosmusicas)

sd(dadosmusicas$Danceabilidade)
sd(dadosmusicas$Energia)
sd(dadosmusicas$Discurso)
sd(dadosmusicas$Acústico)
sd(dadosmusicas$Vivacidade)
sd(dadosmusicas$Valência)
sd(dadosmusicas$Popularidade)
sd(dadosmusicas$Sonoridade)
sd(dadosmusicas$Tempo)



# ANÁLISE EXPLORATÓRIA 

## matriz de correlação

library(corrplot)


dados4 <- dadosmusicas %>% 
  select(popularidade, acustico, discurso, vivacidade, instrumentalidade,
         energia, tempo, sonoridade, dancabilidade, valencia)



corrplot(cor(dados4, method = "pearson"))

matriz_corr <- cor(dados4)

corrplot(matriz_corr,method="square",tl.srt=45, 
         title = "Nível de correlação entre as variáveis", 
         mar = c(2,2,2,2), tl.col = "red",diag = F)


cor(dadosmusicas$valencia, dadosmusicas$dancabilidade)
cor(dadosmusicas$valencia, dadosmusicas$sonoridade)
cor(dadosmusicas$energia, dadosmusicas$valencia)
cor(dadosmusicas$energia, dadosmusicas$sonoridade)
cor(dadosmusicas$energia, dadosmusicas$acustico)
cor(dadosmusicas$acustico, dadosmusicas$sonoridade)

# Analisando Popularidade 

pop <- dadosmusicas %>%
  select(musica, artista, popularidade) %>%
  arrange(desc(popularidade)) %>%
  head(30)


pop2 <- dadosmusicas %>%
  select(musica, artista, popularidade) %>%
  arrange(popularidade) %>%
  head(30)


## gráfico top10


musicasmaispopulares <- pop %>%
  select(musica, popularidade) %>%
  head(20)

musicasmaispopulares %>%
  ggplot(aes(reorder(musica, popularidade),popularidade))+ 
  geom_col(color="black", fill= "cyan")+ 
  coord_flip()+ xlab("Nome da música")+ 
  ylab("Popularidade")+ ggtitle("20 Músicas mais famosas ")



musicasmenosfamosas <- pop2 %>%
  arrange(popularidade) %>%
  head(20)


musicasmenosfamosas %>%
  ggplot(aes(reorder(musica, popularidade),popularidade))+ 
  geom_col(color="black", fill= "cyan")+ 
  coord_flip()+ xlab("Nome da música")+ 
  ylab("Popularidade")+ ggtitle("20 Músicas menos famosas ")



## gráfico popularidade x ano 

dadosanos <- dadosmusicas %>%
  group_by(Ano) %>%
  mutate(pop=mean(Popularidade)) %>%
  select(Ano,pop)

dadosanos %>% 
  ggplot(aes(Ano,pop))+
  geom_line(color= "#1ed760")+
  ggtitle("Popularidade x Anos") + 
  xlab("Anos") + ylab("Popularidade")



# Analisando Gênero

gen <- dadosmusicas %>%
        arrange(desc(popularidade))%>% 
        select(genero_principal,  popularidade, musica) %>% 
        head(10)


# Analisando Artistas 

dadosart <- dadosmusicas %>%
  group_by(artista) %>%
  mutate(pop = mean(popularidade)) %>%
  arrange(desc(pop))%>%
  select(artista, pop)

dadosart2 <- dadosmusicas %>%
  select(artista, popularidade) %>%
  arrange(desc(popularidade)) %>%
  head(10)

dadosart %>%
  select(Artistas, pop) %>%
  head(15)





## PLAYLISTS

# clusterização kmeans

variaveis <- dadosmusicas %>%
  select(-key, -duracao, -time_signature) #seleciona variaveis para cluster

dados_kmean <- scale(variaveis[-c(1:6)]) 


fviz_nbclust(dados_kmean, kmeans, method = "gap_stat")+
  geom_vline(xintercept = 3, linetype = 2)#Visualiza quantidade de cluster


# testes energia

teste_energia <- kmeans(variaveis$energia, centers = 3) # CLusterizacao

fviz_cluster(teste_energia, variaveis[-c(1:6)], ellipse.type = "t") #visualizar cluster

cluster_energia <- teste_energia$cluster

tabela_energia <- variaveis %>%
  cbind(cluster_energia)


playlist_energia1 <- tabela_energia %>%
  select(id,artista, musica, genero_principal, energia, cluster_energia) %>%
  filter(cluster_energia == 1) %>%
  arrange(desc(energia))

playlist_energia2 <- tabela_energia %>%
  select(id,artista, musica, genero_principal, energia, cluster_energia) %>%
  filter(cluster_energia == 2) %>%
  arrange(desc(energia))

playlist_energia3 <- tabela_energia %>%
  select(id,artista, musica, genero_principal, energia, cluster_energia) %>%
  filter(cluster_energia == 3) %>%
  arrange(desc(energia))



# testes dancabilidade

teste_dancabilidade <- kmeans(variaveis$dancabilidade, centers = 3) # CLusterizacao

fviz_cluster(teste_dancabilidade, variaveis[-c(1:6)], ellipse.type = "t") #visualizar cluster

cluster_dancabilidade <- teste_dancabilidade$cluster

tabela_dancabilidade <- variaveis %>%
  cbind(cluster_dancabilidade)


playlist_dancabilidade1 <- tabela_dancabilidade %>%
  select(id,artista, musica, genero_principal, dancabilidade, cluster_dancabilidade) %>%
  filter(cluster_dancabilidade == 1) %>%
  arrange(desc(dancabilidade))

playlist_dancabilidade2 <- tabela_dancabilidade %>%
  select(id,artista, musica, genero_principal, dancabilidade, cluster_dancabilidade) %>%
  filter(cluster_dancabilidade == 2) %>%
  arrange(desc(dancabilidade))

playlist_dancabilidade3 <- tabela_dancabilidade %>%
  select(id,artista, musica, genero_principal, dancabilidade, cluster_dancabilidade) %>%
  filter(cluster_dancabilidade == 3) %>%
  arrange(desc(dancabilidade))




# testes valencia

teste_valencia <- kmeans(variaveis$valencia, centers = 3) # CLusterizacao

fviz_cluster(teste_valencia, variaveis[-c(1:6)], ellipse.type = "t") #visualizar cluster

cluster_valencia <- teste_valencia$cluster

tabela_valencia <- variaveis %>%
  cbind(cluster_valencia)


playlist_valencia1 <- tabela_valencia %>%
  select(id,artista, musica, genero_principal, valencia, cluster_valencia) %>%
  filter(cluster_valencia == 1) %>%
  arrange(desc(valencia))

playlist_valencia2 <- tabela_valencia %>%
  select(id,artista, musica, genero_principal, valencia, cluster_valencia) %>%
  filter(cluster_valencia == 2) %>%
  arrange(desc(valencia))

playlist_valencia3 <- tabela_valencia %>%
  select(id,artista, musica, genero_principal, valencia, cluster_valencia) %>%
  filter(cluster_valencia == 3) %>%
  arrange(desc(valencia))


# banco "festa"


festinha <- as.data.frame(playlist_dancabilidade1 %>% inner_join(playlist_energia1))
festinhatop <- as.data.frame(festinha %>% inner_join(playlist_valencia3))


ggplot(festinhatop) +
  geom_bar(aes(x = genero_principal), fill = "tomato", color = "black")

table(festinhatop$genero_principal)


create_playlist("joaovmelo","balada") # criando playlists ("usuario", "nome da playlist")
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[1:50])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[51:100])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[101:150])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[151:200])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[201:250])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[251:300])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[301:351])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[351:400])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[401:450])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[401:450])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[401:450])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[451:500])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[501:550])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[551:600])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[601:650])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[651:660])


# testes sonoridade

teste_sonoridade <- kmeans(variaveis$sonoridade, centers = 3) # CLusterizacao

fviz_cluster(teste_sonoridade, variaveis[-c(1:6)], ellipse.type = "t") #visualizar cluster

cluster_sonoridade <- teste_sonoridade$cluster

tabela_sonoridade <- variaveis %>%
  cbind(cluster_sonoridade)


playlist_sonoridade1 <- tabela_sonoridade %>%
  select(id,artista, musica, genero_principal, sonoridade, cluster_sonoridade) %>%
  filter(cluster_sonoridade == 1) %>%
  arrange(desc(sonoridade))

playlist_sonoridade2 <- tabela_sonoridade %>%
  select(id,artista, musica, genero_principal, sonoridade, cluster_sonoridade) %>%
  filter(cluster_sonoridade == 2) %>%
  arrange(desc(sonoridade))

playlist_sonoridade3 <- tabela_sonoridade %>%
  select(id,artista, musica, genero_principal, sonoridade, cluster_sonoridade) %>%
  filter(cluster_sonoridade == 3) %>%
  arrange(desc(sonoridade))




# banco "academia" 

academia <- as.data.frame(playlist_sonoridade3 %>% inner_join(playlist_energia1))
academia2 <- as.data.frame(academia %>% inner_join(playlist_dancabilidade1))
academiatop <- as.data.frame(academia2 %>% inner_join(playlist_valencia3))
count(academiatop)

create_playlist("joaovmelo","academia") # criando playlists ("usuario", "nome da playlist")
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[1:50])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[51:100])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[101:150])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[151:200])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[201:250])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[251:300])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[301:351])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[351:400])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[401:450])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[401:450])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[401:450])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[451:500])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[501:550])

ggplot(academiatop) +
  geom_bar(aes(x = genero_principal), fill = "tomato", color = "black")

table(academiatop$genero_principal)



# banco "triste" 

triste <- as.data.frame(playlist_energia3 %>% inner_join(playlist_dancabilidade3))

triste2 <- as.data.frame(triste %>% inner_join(playlist_valencia1))

tristes <- as.data.frame(triste2 %>% inner_join(playlist_sonoridade2))



ggplot(tristes) +
  geom_bar(aes(x = genero_principal), fill = "tomato", color = "black")

table(tristes$genero_principal)



create_playlist("joaovmelo","sad") # criando playlists ("usuario", "nome da playlist")
add_tracks_to_playlist("57XVm7Bva3e0iKbkAGjuof", uris = tristes$id[1:50])
add_tracks_to_playlist("57XVm7Bva3e0iKbkAGjuof", uris = tristes$id[51:100])
add_tracks_to_playlist("57XVm7Bva3e0iKbkAGjuof", uris = tristes$id[101:150])
add_tracks_to_playlist("57XVm7Bva3e0iKbkAGjuof", uris = tristes$id[151:200])
add_tracks_to_playlist("57XVm7Bva3e0iKbkAGjuof", uris = tristes$id[201:226])
