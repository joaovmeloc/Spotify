library(shiny)
library(shinydashboard)   
library(shinydashboardPlus)
library(shinyauthr)
library(shinybusy)
library(shinycssloaders)
library(shinyjs)
library(DT)
library(RSQLite)
library(DBI)
library(sodium)
library(htmlwidgets)
library(tidyverse)
library(rvest)
library(lubridate)
library(magrittr)
library(spotifyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(GGally)



Imp <- function() {
  fluidPage(
    br(),
    hr(),
    a(
      href = "",
      h6(strong("Automatização de Playlists no Spotify")),
      target = "_blank",
      style = "display: block; color: white"
    ),
    br(),
    br(),
    strong("Powered by"),
    br(),
    br(),
    a(
      href = c("https://developer.spotify.com/"),
      img(src = "https://raw.githubusercontent.com/KewKalustian/spotify_charts_scraper/master/img/Spotify_Logo_CMYK_White.png",
          width = "50%"),
      target = "_blank"
    ),
    hr(),
    br(),
    h6(strong("Departamento de Estatística 
                da UnB")),
    style = "display: block; color: white",
    target = "_blank",
    hr(),
    a(
      href = c("https://www.aesthetics.mpg.de/en.html"),
      target = "_blank"
    )
  )
}

# H E A D E R

header <- dashboardHeader(
  title = "Criando sua Playlist",
  titleWidth = 500,
  tags$li(
    class = "dropdown",
    a(
      icon("github"),
      href = "https://github.com/josefelipe0036/LabEst",
      title = "Browse Source Code on GitHub",
      target = "_blank",
      style = "font-size: 25px; display: contents"
    )
  ),
  
  tags$li(
    class = "dropdown",
    style = "padding: 8px"
  )
)


sidebar <- dashboardSidebar(
  width = 290,
  sidebarMenu(
    
    # Removing the sidebar toggle element
    tags$script(
      JS(
        "document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"
      )
    ),
    
    br(),
    br(),
    br(),
    
    br(),
    br(),
    tags$style(
      HTML(
        ".shiny-notification {position:fixed; top: 35%; left: 283px; right: 0px;
         width: 270px; background-color: #283747; color: white}"
      )
    ),
    textInput("user", "Insira o user ID"),
    textInput("ID","Insira o Client ID"),
    textInput("secret", "Insira o secret ID"),
    textOutput("text_out")
    ,
    br(),
    br(),
    
    
    actionButton(
      "go",
      label = div("Start", icon("play-circle")),
      width = 270,
      style = "color: white; font-size: 32px; font-weight: bold; text-align:
               center; background-color: #283747;
               display: contents;"
    ),
    
    Imp()
  )
)

# B O D Y
body <-  dashboardBody(
  tags$head(tags$style(
    HTML(
      "
            /* logo */
           .main-header .logo {font-weight: bold;
           font-size: 22px}
           .skin-blue .main-header .logo {
           background-color:#283747}
           
           /* logo when hovered */
           .skin-blue .main-header .logo:hover {
           background-color: #283747}
           
           /* rest */
           .skin-blue .main-header .navbar-static-top {
           background-color: #283747}
           
           /* main sidebar */
           .skin-blue .main-sidebar {
           background-color: #283747}
           
           /* links + when links hovered*/
           a {color: #F47920}
           a:hover {color: #283747 }
           
           /* buttons/icons + when buttons/icons hovered */
           .fa-download:hover,
           .fa-play-circle:hover ,
           .fa-info-circle:hover ,
           .fa-github:hover {color: #F47920}
  
           #login-button {background-color: #283747; border: none}
           #login-button:hover {background-color: #F47920}
           #logout-button {background-color: #8b0000; border: none}
           #logout-button:hover {background-color:#6f0000}
           
           /* github icon position */
           .fa-github {padding-top: 12px; padding-bottom: 10px;
           padding-right:10px}
           
           /* login panel / body */
           #rmve .with-border {display:none}
           #rmve .box-body  {background-color: #283747}
           h6:nth-child(1):hover, br+ h6 strong:hover  {color: #F47920}
          
      "
    )
  )),
  DTOutput("liked_songs")
  
  
)








ui <- dashboardPage(header, sidebar, body)


#####################
#### S E R V E R ####
#####################


server <- function(input, output, session) {
  
  observeEvent(once = T, session, {
    showModal(modalDialog(  
      title = p(
        style = "text-align: center; font-size: 42px; font-weight:bold",
        "Divertindo-se com Spotify",
        br(),
        p(style = "font-weight:normal; font-size: 21px; text-align: center",
          " ")
      ),
      
      style = "text-align: justify; font-size: 14pt;",
      p("",
        strong("Spotify com R"),
        "é uma forma de criar playlists automatizadas no Spotify, é uma ",
        a(href = "https://shiny.rstudio.com/", "aplicação do Shiny", 
          target = "_blank"),
        "com as músicas mais tocadas do ",
        a(href="https://kworb.net/spotify/", "Spotify charts", 
          target ="_blank"),
        em(strong("baseado nas")),
        a(href = "https://developer.spotify.com/discover/", "caracteristicas das músicas", 
          target="_blank"),
        " o aplicativo cria playlist. Para saber mais como a aplicação foi feita acesse",
        
        a(href = "https://github.com/josefelipe0036/LabEst",
          "Spotify com R", target = "_blank"),
      ),
      
      p(
        style = "text-align: justify; font-size: 14pt;",
        ""
      ),
      br(),
      
      hr(),
      h5("Para ter acesso ao id e ao secret é necessário ter uma conta no Spotify developer.
        E para encontrar o user_id, basta ir em perfil e copiar uma parte do link, por exemplo: https://open.spotify.com/user/12345, o user_id é 12345"),
      hr(),
      h6(
        "Criado pelo Laboratório de Estatística, orientado pelo professor Jhames Sampaio."
      ),
      footer = modalButton("OK"),
      size = "l",
      easyClose = T
    ))
  })
  dataset_ls = eventReactive(input$go,{
    
    data = tryCatch({
      pacman::p_load(spotifyr, tidyverse, factoextra, FactoMineR, readxl,cluster)
      Sys.setenv(SPOTIFY_CLIENT_ID = input$ID)#'da313eb7d34948459de44d4f9bc6c206'
      Sys.setenv(SPOTIFY_CLIENT_SECRET = input$secret )#'c91752d38cbf45e1b865c14dcad3a046'
      access_token <- get_spotify_access_token()
      setwd("/home/jose/UnB/2022.2/LABEST/Spotify com R")
      dado <-read_xlsx("8ktopmegablastes.xlsx")
      dado1= dado[,c(831:842)]
      dado_n<-scale(dado1[-3])
      
      gen = dado$generos
      library(tidyverse)
      gen2 = data.frame(str_split(gen,",",simplify = T))
      
      
      
      for(i in 1:ncol(gen2)){
        gen2[,i]=  str_replace_all(gen2[,i],"(\\'|\\[|\\])","")
      }
      
      bd = cbind(dado$artista,gen2)
      names(bd)[1]="artista"
      
      bd2  = pivot_longer(bd,names(bd)[2:14],names_to = "name",values_to = "valores")
      
      bd2= bd2 %>% filter(valores!="")
      
      bd3 = data.frame(str_split(bd2$valores," ",simplify = T))
      
      for(i in 1:ncol(bd3)){
        bd3[,i]=  str_replace_all(bd3[,i]," ","")
      }
      
      bd3 = cbind(bd2$artista,bd3)
      
      names(bd3)[1]="artista"
      bd4  = pivot_longer(bd3,names(bd)[2:7],names_to = "name",values_to = "valores")
      
      bd4= bd4 %>% filter(valores!="")
      
      tabela=data.frame(table(bd4$valores))
      tabela$pct = 100*(tabela$Freq/sum(tabela$Freq))
      
      
      tabela_c = tabela %>% filter(Freq >= 1000)
      
      
      for(i in 1:nrow(tabela_c)){
        v1=as.character(tabela_c$Var1[i])
        dado[,c(v1)] = ifelse(str_detect(dado$generos,v1),1,0)
      }
      
      dado_n2 = dado %>% select(tabela_c$Var1)
      
      
      data_clust = cbind(id = dado$id,dado_n,dado_n2)
      
      library(stats)
      teste_k<-kmeans(data_clust[,-1], centers = 3)#CLusterizacao
      
      fviz_cluster(teste_k, data_clust[-1], ellipse.type = "t")#
      
      
      dado$cluster= as.factor(teste_k$cluster)
      
      dado_c1 = dado %>% filter(cluster==1)
      #summary(dado_c1[,831:842][,-3])
      
      dado_c2 = dado %>% filter(cluster==2)
      #summary(dado_c2[,831:842][,-3])
      
      dado_c3 = dado %>% filter(cluster==3)
      #summary(dado_c3[,831:842][,-3])
      userID<- input$user
      create_playlist(userID, "cluster 1")
      create_playlist(userID, "cluster 2")
      create_playlist(userID, "cluster 3")
      playlists<-head(get_my_playlists(),3)
      add_tracks_to_playlist(playlists$id[3], uris = dado_c1$id[1:80])#cluster1
      add_tracks_to_playlist(playlists$id[2], uris = dado_c2$id[1:80])#cluster2
      add_tracks_to_playlist(playlists$id[1], uris = dado_c3$id[1:80])#cluster3
      head(dado,200)
    })
  })
  output$liked_songs = renderDT({dataset_ls()})
  
}

#####################
#### D E P L O Y ####
#####################

shinyApp(ui = ui, server = server)
