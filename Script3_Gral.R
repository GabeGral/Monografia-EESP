## Gabriel Gral
## June 2022
## Script 03

rm(list=ls())

## Keeping all databases in the same directory makes importing easier
getwd() #- This is the directory in which the bases are
list.files() #- These are the available files and databases

## Datasets used in this script: 
## Make sure all the datasets in the 'Load Data' section are in your working directory

# Libraries ---------------------------------------------------------------
library(readxl)
library(tidyverse)
library(dplyr)
library(writexl)
library(stargazer)
library(ggplot2)
library(ggalt)
library(ggtext)
library(extrafont)


library(expss) #- Apply labels to dataframes
library(geobr) #- Brazil Maps
library(ipeadatar) #- IPEA data info
library(maps) #- Maps formating
library(ggspatial) #- Maps formating
library(sf) #- Maps formating
library(tmap) #- Maps formating | Interactive maps
library(scales) #- Maps formating
library(miceadds) #- Cluster regression

# Load Data ---------------------------------------------------------------

Gini20 <- read_excel("C:/Users/gabri/OneDrive/Área de Trabalho/IC/Omega/R_Omega/land_gini1920.xlsx")

Capitais <- c('2704302', # Maceió
              '2927408', # Salvador
              '2304400', # Fortaleza
              '2111300', # São Luís
              '2507507', # Parahyba
              '2611606', # Recife
              '2211001', # Teresina
              '2408102', # Natal
              '2800308', # Aracaju
              '3205309', # Vitória
              '3106200', # Belo Horizonte
              '4106902', # Curitiba
              '3303302', # Niterói
              '4314902', # Porto Alegre
              '4205407', # Florianópolis
              '3550308') # São Paulo
              
Gini20$Capital <- ifelse(Gini20$code_muni %in% Capitais, yes = TRUE, no = FALSE)

# Functions ---------------------------------------------------------------


## ggplot theme for maps

theme_map_gabe <- function(base_size=2.5) {
  
  color.background = alpha("cornsilk",0.2)
  color.border = "black"
  color.grid.major = "gray50"
  color.axis.text = "black"
  color.axis.title = "black"
  color.title = "black"
  color.subtitle = "black"
  
  # Begin construction of chart
  theme_bw(base_size= base_size * 9) +
    
    #Background 
    theme(plot.background=element_rect(fill="transparent")) +
    theme(panel.background=element_rect(fill=color.background)) +
    theme(panel.border=element_rect(color=color.border, size = 0.5)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major, size=.5, linetype = "dashed")) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_line(colour = color.grid.major, size = 0.5, linetype = "dashed")) +
    
    # Format the legend
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size= 12, color=color.axis.title)) +
    theme(legend.title=element_text(color = color.title, size = 14)) + 
    theme(legend.position=c(1,0)) +
    theme(legend.justification=c(1,0)) + 
    theme(legend.direction="vertical") + 
    theme(legend.box="horizontal") +
    theme(legend.box.just = c("top")) +
    
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(face = "bold", color=color.title, size=20, vjust=1.25, hjust = 0)) + 
    theme(plot.subtitle=element_text(face = "italic", color=color.subtitle, size=15, vjust=1.25)) +
    theme(plot.caption = element_text(face = "plain", size = 13)) +
    theme(axis.title = element_blank()) +
    theme(axis.text = element_text(size = base_size*6)) 
}


theme_boxplot_gabe <- function(base_size = 2.5) {
  color.background = alpha("cornsilk",0.2)
  color.border = "black"
  color.grid.major = "gray50"
  color.axis.text = "black"
  color.axis.title = "black"
  color.title = "black"
  color.subtitle = "black"
  
  theme(legend.position="bottom") +
    theme(legend.title = element_text(face = 'bold', size = 15)) +
    theme(plot.title=element_text(face = "bold",  size=16, vjust=1.25, hjust = 0)) + 
    theme(plot.subtitle=element_text(face = "italic",  size=15, vjust=1.25)) +
    theme(plot.caption = element_text(face = "plain", size = 13)) + 
    theme(legend.background = element_rect(fill = 'lightgrey', size = 0.5, linetype = 'solid'))
}


theme_hist_gabe <- function(base_size = 2.5) {
  color.background = alpha("cornsilk",0.2)
  color.border = "black"
  color.grid.major = "gray50"
  color.axis.text = "black"
  color.axis.title = "black"
  color.title = "black"
  color.subtitle = "black"
  
  theme(legend.position="none") +
    theme(plot.title=element_text(face = "bold",  size=16, vjust=1.25, hjust = 0)) + 
    theme(plot.subtitle=element_text(face = "italic",  size=15, vjust=1.25)) +
    theme(plot.caption = element_text(face = "plain", size = 13)) 
}


# Histograms Gini ---------------------------------------------------------

{H20_Gini_Land <- ggplot(Gini20, aes(x= land_gini))+
    geom_histogram(color="black", fill="grey", alpha = 0.7, bins = 80) + 
    labs(title = "Hisograma do índice de Gini por município (Land)" ,
         x = "Índice de Gini", y = "Número de Cidades", subtitle = "Índice calculado entre proprietários de terra. (Mediana em vermelho)",
         caption = '' ) +
    theme_minimal(base_size = 14) +
    geom_vline(aes(xintercept= median(land_gini), color="red"), linetype="dashed", size = 1.5) +
    geom_vline(aes(xintercept=0), size = 1) +
    theme_hist_gabe()
  
  ggsave(filename = 'H20_Gini_Land.jpg', plot = H20_Gini_Land,
         width = 1920/144, height = 1080/144)
  
} 


{H20_Gini_All <- ggplot(Gini20, aes(x= all_gini))+
    geom_histogram(color="black", fill="grey", alpha = 0.7, bins = 80) + 
    labs(title = "Hisograma do índice de Gini por município (All)" ,
         x = "Índice de Gini", y = "Número de Cidades", subtitle = "Índice calculado entre toda a população. (Mediana em vermelho)",
         caption = '' ) +
    theme_minimal(base_size = 14) +
    geom_vline(aes(xintercept= median(all_gini), color="red"), linetype="dashed", size = 1.5) +
    geom_vline(aes(xintercept=0), size = 1) +
    theme_hist_gabe()
  
  ggsave(filename = 'H20_Gini_All.jpg', plot = H20_Gini_All,
         width = 1920/144, height = 1080/144)
  
} 

# Maps Gini ---------------------------------------------------------------

MuniGeo <- geobr::read_municipality(year=1920)
MapaGini <- left_join(MuniGeo, Gini20, by = 'code_muni')
MapaGini <- cbind(MapaGini, st_coordinates(st_centroid(MapaGini$geom)))

{maxMap <- plyr::round_any(max(MapaGini$land_gini), 0.1, f = ceiling)
minMap <- plyr::round_any(min(MapaGini$land_gini), 0.1, f = floor)

Estados20 <- read_state(year = 1920, code_state = 'all')
M20_Gini_Land <- ggplot() + 
  geom_sf(data = MapaGini$geom, aes(fill = unlist(MapaGini$land_gini))) + 
  geom_sf(data = Estados20, fill = NA, size = .8, color = 'gray20') +
  scale_fill_distiller(palette = "Spectral", na.value= "gray85",
                       name = 'Índice de Gini', limits = c(minMap, maxMap)) +
  labs(title = "Land Gini 1920",
       # subtitle = paste0('Ajustado por AMCs - ', tp_Cortadas, ' AMCs Cortadas. Ajustado por crescimento populacional.'),
       caption = '') +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme_map_gabe()

ggsave(filename = 'M20_Gini_Land.jpg', plot = M20_Gini_Land,
       width = 1080/96, height = 1080/96)

}

{maxMap <- plyr::round_any(max(MapaGini$all_gini), 0.1, f = ceiling)
  minMap <- plyr::round_any(min(MapaGini$all_gini), 0.1, f = floor)
  
  Estados20 <- read_state(year = 1920, code_state = 'all')
  M20_Gini_All <- ggplot() + 
    geom_sf(data = MapaGini$geom, aes(fill = unlist(MapaGini$all_gini))) + 
    geom_sf(data = Estados20, fill = NA, size = .8, color = 'gray20') +
    scale_fill_distiller(palette = "Spectral", na.value= "gray85",
                         name = 'Índice de Gini', limits = c(0.8,1)) +
    labs(title = "Land Gini (All) 1920",
         # subtitle = paste0('Ajustado por AMCs - ', tp_Cortadas, ' AMCs Cortadas. Ajustado por crescimento populacional.'),
         caption = '') +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'M20_Gini_All.jpg', plot = M20_Gini_All,
         width = 1080/96, height = 1080/96)
  
}

# Aggregate per UF

Gini20UF <- Gini20 %>% 
  group_by(estado) %>% 
  summarise(land_gini = mean(land_gini, na.rm = T),
            all_gini = mean(all_gini, na.rm = T))


states <- read_state(year = 1920, code_state = 'all')
states$estado <- c('Alagoas',
  'Amazonas',
  'Bahia',
  'Ceará',
  'Distrito Federal',
  'Espirito Santo',
  'Goiás',
  'Maranhão',
  'Mato Grosso',
  'Minas Gerais',
  'Pará',
  'Paraiba',
  'Paraná',
  'Pernambuco',
  'Piauí',
  'Rio de Janeiro',
  'Rio Grande do Norte',
  'Rio Grande do Sul',
  'Santa Catarina',
  'São Paulo',
  'Sergipe',
  'Acre')
states$Estado20 <- c('AL',
                     'AM',
                     'BA',
                     'CE',
                     'DF',
                     'ES',
                     'GO',
                     'MA',
                     'MT',
                     'MG',
                     'PA',
                     'PB',
                     'PR',
                     'PE',
                     'PI',
                     'RJ',
                     'RN',
                     'RS',
                     'SC',
                     'SP',
                     'SE',
                     'AC')


MapGiniUF <- left_join(states, Gini20UF)
UF_Points <- cbind(MapGiniUF, st_coordinates(st_centroid(MapGiniUF$geom)))
maxMapUF <- plyr::round_any(max(MapGiniUF$land_gini), 0.1, f = ceiling)
minMapUF <- plyr::round_any(min(MapGiniUF$land_gini), 0.1, f = floor)

{
M20UF_Gini_Land <- ggplot() +
  geom_sf(data = MapGiniUF$geom, 
          aes(fill = MapGiniUF$land_gini)) + 
  scale_fill_gradient(low = "Azure", high = "MidnightBlue",
                      na.value="gray85",
                      name = 'Porcentagem (%)',
                      limits = c(minMapUF, maxMapUF)) +
  geom_label(data = UF_Points,
             aes(X, Y, label = Estado20),
             size = 3,
             fontface = "bold") +
  labs(title = 'Média de Gini dos municípios por UF em 1920',
       subtitle = 'Índice calculado entre proprietários', 
       caption='') +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme_map_gabe()
  
  ggsave(filename = 'M20UF_Gini_Land.jpg', plot = M20UF_Gini_Land,
         width = 1080/96, height = 1080/96)
  
}

maxMapUF <- plyr::round_any(max(MapGiniUF$all_gini), 0.1, f = ceiling)
minMapUF <- plyr::round_any(min(MapGiniUF$all_gini), 0.1, f = floor)

{
  M20UF_Gini_All <- ggplot() +
    geom_sf(data = MapGiniUF$geom, 
            aes(fill = MapGiniUF$all_gini)) + 
    scale_fill_gradient(low = "Azure", high = "MidnightBlue",
                        na.value="gray85",
                        name = 'Porcentagem (%)',
                        limits = c(minMapUF, maxMapUF)) +
    geom_label(data = UF_Points,
               aes(X, Y, label = Estado20),
               size = 3,
               fontface = "bold") +
    labs(title = 'Média de Gini dos municípios por UF em 1920',
         subtitle = 'Índice calculado entre toda a população', 
         caption='') +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'M20UF_Gini_All.jpg', plot = M20UF_Gini_All,
         width = 1080/96, height = 1080/96)
  
}


# Boxplot Gini ------------------------------------------------------------
    #Box Plots:
      #B_All_Gini
      #B_Land_Gini

# Base Gini Boxplot
# Adjust data 
    states_Gini <- as.data.frame(unique(Gini20$estado))
    colnames(states_Gini)[1] <- 'estado'
    
    states_Gini$Estado20 <- c('AL',
                              'BA',
                              'CE',
                              'MA',
                              'PB',
                              'PI',
                              'PE',
                              'RN',
                              'SE',
                              'ES',
                              'MG',
                              'PR',
                              'RJ',
                              'RS',
                              'SC',
                              'SP')
    
    Region_1_Sul_1 <- c('RS', 'SC', 'PR')
    Region_1_Sudeste_2 <- c('SP', 'MG', 'RJ', 'ES')
    Region_1_Nordeste_3 <- c('BA', 'SE', 'AL', 'PE', 'PB', 'RN', 'CE', 'PI', 'MA')
    
    for (i in 1:length(states_Gini$Estado20)){
      if (substr(states_Gini$Estado20[i],1,2) %in% Region_1_Sul_1){
        states_Gini$Region[i] <- 'Sul'
      } else if (substr(states_Gini$Estado20[i],1,2) %in% Region_1_Sudeste_2){
        states_Gini$Region[i] <- 'Sudeste'
      } else {
        states_Gini$Region[i] <- 'Nordeste'
      }
    }
    
    Gini20_B <- Gini20 %>% 
      left_join(states_Gini)


{
  B_Gini_All <- ggplot(Gini20_B, aes(x = reorder(Estado20, all_gini, mean,  na.rm = T), # Clusters
                                      y = all_gini,  # Variable
                                      fill = Region)) + # Criteria for colors
    geom_boxplot() + 
    coord_cartesian(ylim = c(0.5,1)) +
    geom_hline(aes(yintercept= median(all_gini), color="Mediana"), size = 1 ) +
    geom_hline(aes(yintercept= mean(all_gini), color="Média"), size = 1 ) +
    geom_point( shape = 23, data=subset(Gini20_B, Capital == TRUE), 
                aes(x=reorder(Estado20, all_gini, median,  na.rm = T), y=all_gini), position=position_dodge(1),
                color="black", size=4, show.legend = FALSE) +
    scale_color_manual(name = 'Estatísticas', values = c(Mediana = 'blue', Média = 'red')) + 
    scale_fill_discrete(name = 'Região') + 
    theme_minimal(base_size = 14) + 
    labs(title = "Distribuição do índice de Gini de terra por Estado em 1920",
         subtitle = ' Pontos representam índice de Gini de terra por município.  \n Ordenado por médias. Capitais são os losangos em destaque. \n Cálculo considera toda a população. ',
         x = 'Estado', y = "Índice de Gini", 
         caption = '') +
    theme_boxplot_gabe()
  
  ggsave(filename = 'B_Gini_All.jpg', plot = B_Gini_All,
         width = 1920/144, height = 1080/144)
}

{
  B_Gini_Land <- ggplot(Gini20_B, aes(x = reorder(Estado20, land_gini, mean,  na.rm = T), # Clusters
                                     y = land_gini,  # Variable
                                     fill = Region)) + # Criteria for colors
    geom_boxplot() + 
    coord_cartesian(ylim = c(0,1)) +
    geom_hline(aes(yintercept= median(land_gini), color="Mediana"), size = 1 ) +
    geom_hline(aes(yintercept= mean(land_gini), color="Média"), size = 1 ) +
    geom_point( shape = 23, data=subset(Gini20_B, Capital == TRUE), 
                aes(x=reorder(Estado20, all_gini, median,  na.rm = T), y=all_gini), position=position_dodge(1),
                color="black", size=4, show.legend = FALSE) +
    scale_color_manual(name = 'Estatísticas', values = c(Mediana = 'blue', Média = 'red')) + 
    scale_fill_discrete(name = 'Região') + 
    theme_minimal(base_size = 14) + 
    labs(title = "Distribuição do índice de Gini de terra por Estado em 1920",
         subtitle = ' Pontos representam índice de Gini de terra por município.  \n Ordenado por médias. Capitais são os losangos em destaque. \n Cálculo considera somente proprietários de terra. ',
         x = 'Estado', y = "Índice de Gini", 
         caption = '') +
    theme_boxplot_gabe()
  
  ggsave(filename = 'B_Gini_Land.jpg', plot = B_Gini_Land,
         width = 1920/144, height = 1080/144)
}
    
    summary(Gini20)
    