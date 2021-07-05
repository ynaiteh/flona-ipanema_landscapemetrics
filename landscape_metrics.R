# ---------------- LANDSCAPE METRICS ----------------
# Bruna Sacramento, Camille Vasconcellos, Hetiany Costa

# INSTALACAO DE PACOTES
#Pacotes necessarios para o script. Para tirar todos os comentarios e os instalar, selecionar as linhas
#desejadas e pressionar Ctrl + Shift + C
# install.packages(c("tidyverse",
#                    "sf",
#                    "raster",
#                    "rgdal",
#                    "fasterize",
#                    "landscapetools",
#                    "landscapemetrics",
#                    "tmap",
#                  dependencies = TRUE) #instala, automaticamente, pacotes secundarios para que estes listados funcionem

library(sf) #para trabalhar com vetores
library(raster) #para trabalhar com raster
library(rgdal) #auxiliar do pacote raster
library(fasterize) #rapida rasterizacao
library(landscapetools) #ferramentas para trabalhar com ecologia de paisagem
library(landscapemetrics) #para calcular as metricas de paisagem
library(tmap) #faz mapas tematicos
library(tidyverse) #pacote com diversos outros inclusos
library(dplyr) #pacote para trabalhar com data frames
#--

#CARREGAMENTO DOS FRAGMENTOS E LIMITE
setwd("C:\\Users\\hetia\\Desktop\\UNESP\\Disciplinas\\R\\FLONA_landscapemetrics")
img_patch <- stack("fragmentos_wgs_patch.tif") #fragmentos florestais a nivel de mancha
img_class <- stack("fragmentos_wgs_classe.tif") #fragmentos florestais a nivel de classe (de acordo com a �rea)
fragmentos <- shapefile("fragmentos.shp")
limite_flona <- shapefile("limite_oficial_WGS84.shp")
#a funcao stack junta, em uma coluna, dados de varias colunas para que possam ser analisados
#--

#AVALIACAO DAS METRICAS
#Nivel de mancha
lsm_patch <- landscapemetrics::calculate_lsm(landscape=img_patch,
                                             level = "patch", 
                                             edge_depth = 18, #celulas de borda. Adotado uma borda de 36 m, entao 18 celulas de borda (resolu��o espacial do raster de 2 m)
                                             neighbourhood = 8, #oito celulas nas vizinhancas
                                             full_name = TRUE, 
                                             verbose = TRUE, 
                                             progress = TRUE)
#o script aponta "warning" para a metrica "distancia euclidiana do vizinho mais proximo" (ENN). Esta a uma metrica
#gerada para cada mancha (patch) mas que deve ser analisada a nivel de classe, portanto, nao deveria estar
#no level "patch"


#A tabela gerada lista, em uma mesma coluna, os valores das metricas. Para deixar cada metrica com seu 
#valor em uma coluna, fazer o passo abaixo:
#--

#Organizacao da tabela de metricas
rm(lsm_frame)
n_metricas <- 12   #numero de metricas geradas pelo pacote landscapemetrics
n_fragmentos <- 33 #numero de fragmentos florestais a serem avaliados
k <- 1
lsm_frame <- data.frame(1,1) #criacao do data frame mais organizado

for (i in 1:n_fragmentos){
    lsm_frame[i,1]<- i 		                #coluna com valores de id fragmento
    for (j in 1:n_metricas){
        lsm_frame[i,j+1]<- lsm_patch[k,6]		#fazer a linhas com valores das metricas
        k<-k+n_fragmentos
    }
    k <- i+1
}

colnames(lsm_frame) <- c("Id_fragmento",
                         "area_ha",
                         "CAI_%",
                         "CIRCLE",
                         "CONTIG",
                         "Core_ha",
                         "ENN",
                         "FRAC",
                         "GYRATE_m",
                         "NCore",
                         "PARA",
                         "Perimetro_m",
                         "SHAPE")
#essa tabela esta melhor organizada e com uma metrica por coluna
#--

#Classificacao dos fragmentos por classe de tamanho (conforme a area)
for (i in 1:n_fragmentos){
    if (lsm_frame$area_ha[i]<= 10) { #Pequenos sao os menores que 10 ha
        (lsm_frame$Classe[i] = "Pequeno")
    }
    else {
        if (10 < lsm_frame$area_ha[i] &&  lsm_frame$area_ha[i] <= 50){ #Medios tem area entre 10 e 50 ha
            (lsm_frame$Classe[i] = "Medio")
        }
        else {
            if (50 < lsm_frame$area_ha[i] &&  lsm_frame$area_ha[i] <= 100){ #Grandes tem area entre 50 e 100 ha
                (lsm_frame$Classe[i] = "Grande")
            }
            else { #Muito grandes sao os maiores que 100 ha
                (lsm_frame$Classe[i] = "Muito grande")
            }
        }
    }
}
#--

#Criacao da tabela de metricas para analise
rm(tabela_metricas)
n_metricas <- 5 #numero de metricas que serao analisadas
tabela_metricas <- data.frame(lsm_frame[1], #Id_fragmento
                              lsm_frame[2], #area_ha
                              lsm_frame[12],#Perimetro_m
                              lsm_frame[6], #Core_ha 
                              lsm_frame[10],#NCore
                              lsm_frame[8], #FRAC
                              lsm_frame[14])#Classe
#--

#Exportacao dos dados
readr::write_csv2(tabela_metricas, "tabela_metricas.csv") 
#comando csv2 para que o separador decimal da tabela seja virgula

# -----------------------------------------------------------------------
#ESTATASTICA DESCRITIVA
#Analise a nivel de mancha 
sapply(tabela_metricas[,-c(1,(n_metricas+2))], summary) #resumo da estatastica basica sem a primeira coluna (id_fragmentos) e a �ltima (Classe)
sapply(tabela_metricas[,-c(1,(n_metricas+2))], sd) #desvio padrao (sd) sem a primeira e a ultima coluna
sapply(tabela_metricas[,-c(1,(n_metricas+2))], cv) #coeficiente de variacao (cv) sem a primeira e a ultima coluna

#Analise a nivel de classe
#resumo da estatastica basica sem a primeira coluna (id_fragmentos)
summary(tabela_metricas[,-1], Classe=="Pequeno")
summary(tabela_metricas[,-1], Classe=="Medio")
summary(tabela_metricas[,-1], Classe=="Grande")
summary(tabela_metricas[,-1], Classe=="Muito grande")

#CV e SD por classe de tamanho dos fragmentos florestais
CV_SD_classe <- t(data.frame(tabela_metricas %>% #funcao "t" transpot a matriz
                                 group_by(Classe) %>%            
                                 summarise(SD_Area=sd(area_ha),CV_Area=cv(area_ha),
                                           SD_Core=sd(Core_ha),CV_Core=cv(Core_ha),
                                           SD_FRAC=sd(FRAC),CV_DF=cv(FRAC),
                                           SD_NCore=sd(NCore),CV_NCore=cv(NCore),
                                           SD_Perimetro=sd(Perimetro_m),CV_Perimetro=cv(Perimetro_m))))

# -----------------------------------------------------------------------

#ELABORACAO DOS MAPA DOS FRAGMENTOS FLORESTAIS
#Espacializacao da area
Area_esp <- landscapemetrics::spatialize_lsm(img_class,
                                             what = "lsm_p_area", 
                                             progress = TRUE)
#--

#Reclassificacao do mapa de acordo com as classes de area estabelecidas
recl <-  (c(5,10, 1,
         10,50,2,
         50,100,3,
         100,Inf,4))
r_Area <- raster::reclassify(x = Area_esp[[1]]$lsm_p_area, rcl= matrix(recl, ncol=3, byrow=TRUE))
#--

#Mapa Fragmentos florestais (usando raster a nivel de classe)
tm_shape(limite_flona)+ #limite da Flona
    tm_borders(col="black")+
    tm_shape(r_Area, bbox = raster::bbox(r_Area))+ #plotar o raster de fragmentos
    tm_raster(style = "cat", palette = c("red", "orange", "green", "forestgreen"),
              title = "Classes", 
              labels= c("Pequeno","Medio", "Grande" ,"Muito Grande"))+
    tm_layout(main.title = "Mapa dos Fragmentos Florestais", 
              main.title.position = 'center', 
              main.title.size=.90)+
    tm_grid(lines = FALSE,
            labels.rot = c(0,90), labels.size = (.50))+
    tm_compass(position=c("right","top"), 
               size = 1.5, 
               text.size = 0.6) +
    tm_scale_bar(breaks= c(0,0.5,1),
                 color.dark = "black",
                 color.light = "white",
                 position=c("right","top")) +
    tm_legend(position=c("center","center"), 
              legend.outside=T, 
              text.size=.7)+
    tm_shape(fragmentos) + #numerar os fragmentos
    tm_borders(col="transparent", lwd=1)+ 
    tm_text("FRAG", size=0.75)
#--

#Exportacao do mapa
tmap::tmap_save(filename = "MapaFragmentos.jpeg",
                width = 20,
                height = 20,
                units = "cm",
                dpi = 300)

