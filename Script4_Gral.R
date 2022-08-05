## Gabriel Gral
## June 2022
## Script 04

## Keeping all databases in the same directory makes importing easier
getwd() #- This is the directory in which the bases are
list.files() #- These are the available files and databases

rm(list=ls())

# Libraries ---------------------------------------------------------------

#- Ctrl+Shit+F10 restarts R section and unloads packages
#- When {plyr} commands are necessary, use plyr:: so that conflicts with dplyr are avoided

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



rm(list=ls())

# Load Data ---------------------------------------------------------------

AMCs2040 <- readRDS('AMCs2040')
AMCs20 <- readRDS('AMCs20')
AMCs40 <- readRDS('AMCs40')
Censo2040_UF <- readRDS('Censo2040_UF')
Censo2040_Reg1 <- readRDS('Censo2040_Reg1')
Censo2040_Reg2 <- readRDS('Censo2040_Reg2')
Censo20_UF <- readRDS('Censo20_UF')
Censo40_UF <- readRDS('Censo40_UF')
D_UF <- readRDS('D_UF')
D_AMC <- readRDS('D_AMC')
D_Reg1 <- readRDS('D_Reg1')
D_Reg2 <- readRDS('D_Reg2')
font_import('LMRoman')
 font_import(pattern = "lmr*")

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

theme_hist_gabe <- function(base_size = 2.5) {
  color.background = alpha("cornsilk",0.2)
  color.border = "black"
  color.grid.major = "gray50"
  color.axis.text = "black"
  color.axis.title = "black"
  color.title = "black"
  color.subtitle = "black"
  
  theme(legend.position="none") +
    theme(plot.title=element_text( face = "bold",  size=16, vjust=1.25, hjust = 0)) + 
    theme(plot.subtitle=element_text( face = "italic",  size=15, vjust=1.25)) +
    theme(plot.caption = element_text( face = "plain", size = 13)) 
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


theme_dumbbell_gabe <- function(base_size = 2.5) {
  color.background = alpha("cornsilk",0.2)
  color.border = "black"
  color.grid.major = "gray50"
  color.axis.text = "black"
  color.axis.title = "black"
  color.title = "black"
  color.subtitle = "black"
  
  theme(legend.position="none") +
    theme(legend.title = element_text(face = 'bold', size = 15)) +
    theme(plot.title=element_text(face = "bold",  size=16, vjust=1.25, hjust = 0)) + 
    theme(plot.subtitle=element_text(face = "italic",  size=15, vjust=1.25)) +
    theme(plot.caption = element_markdown(size = 14)) + 
    theme(legend.background = element_rect(fill = 'lightgrey', size = 0.5, linetype = 'solid'))
}

var_graphs_gabe <- function(X){
  if (X == 'AdmPub_PT'){
    x <<- 11
    y <<- 'Funcionários Públicos'
    z <<- 7
  } else {
    x <<- 12
    y <<- 'Servidores da Segurança Pública'
    z <<- 8
  }
}

# Histograms PIB ----------------------------------------------------------


 
{ANO <- 20
X <- 'PIB_Capita'
Q <- 0.95}

{
  var_graphs_gabe(X)
  tp_AMC <- filter(AMCs2040, AMCs2040$Ano == ANO)
  AMC_Corte <- filter(tp_AMC, tp_AMC[,x] < quantile(tp_AMC[,x], Q, na.rm = T))
  tp_Cortadas <- length(tp_AMC[,x]) - length(AMC_Corte[,x]) 
  tp_Subtitle <- paste0('Foram cortadas ', tp_Cortadas, ' cidades de um total de ', length(tp_AMC[,x]))
  
  tp_H <- ggplot(AMC_Corte, aes(x= AMC_Corte[,x]))+
    geom_histogram(color="black", fill="grey", alpha = 0.7, bins = 80) + 
    labs(title = paste0("Histograma de PIB per Capita em 19", ANO) ,
         x = paste0(y, ' por mil habitantes'), y = "Número de Cidades", subtitle = tp_Subtitle,
         caption = '') + 
    theme_minimal(base_size = 14) +
    geom_vline(aes(xintercept= median(AMC_Corte[,x]), color="red"), linetype="dashed", size = 1.5) +
    geom_vline(aes(xintercept=0), size = 1) +
    theme_hist_gabe()
  
  tp_H
  tp_Name <- paste0('H', ANO, '_AMC_PIB')
  assign(tp_Name, tp_H)
  
  ggsave(filename = 'H20_AMC_PIB.jpg', plot = H20_AMC_PIB,
         width = 1920/144, height = 1080/144)
}
H20_AMC_PIB
warnings()
{ANO <- 40
  X <- 'PIB_Capita'
  Q <- 0.95}

{
  var_graphs_gabe(X)
  tp_AMC <- filter(AMCs2040, Ano == ANO)
  AMC_Corte <- filter(tp_AMC, tp_AMC[,x] < quantile(tp_AMC[,x], Q, na.rm = T))
  tp_Cortadas <- length(tp_AMC[,x]) - length(AMC_Corte[,x]) 
  tp_Subtitle <- paste0('Foram cortadas ', tp_Cortadas, ' cidades de um total de ', length(tp_AMC[,x]))
  
  tp_H <- ggplot(AMC_Corte, aes(x= AMC_Corte[,x]))+
    geom_histogram(color="black", fill="grey", alpha = 0.7, bins = 80) + 
    labs(title = paste0("Histograma de PIB per Capita em 19", ANO) ,
         x = paste0(y, ' por mil habitantes'), y = "Número de Cidades", subtitle = tp_Subtitle,
         caption = '')  + 
    theme_minimal(base_size = 14) +
    geom_vline(aes(xintercept= median(AMC_Corte[,x]), color="red"), linetype="dashed", size = 1.5) +
    geom_vline(aes(xintercept=0), size = 1) +
    theme_hist_gabe()
  
  tp_H
  tp_Name <- paste0('H', ANO, '_AMC_PIB')
  assign(tp_Name, tp_H)
  
  ggsave(filename = 'H40_AMC_PIB.jpg', plot = H40_AMC_PIB,
         width = 1920/144, height = 1080/144)
}

Q <- 0.95
{D_AMC_Corte <- filter(D_AMC, D_PIB_Capita < quantile(D_PIB_Capita, Q, na.rm=T))
  Cortadas <- length(D_AMC$D_PIB_Capita) - length(D_AMC_Corte$D_PIB_Capita) 
  Subtitle <- paste0('Foram cortadas ', Cortadas, ' cidades de um total de ', length(D_AMC$D_PIB_Capita))
  
  HD_AMC_PIB <- ggplot(D_AMC_Corte, aes(x= D_PIB_Capita))+
    geom_histogram(color="black", fill="grey", alpha = 0.7, bins = 80) + 
    labs(title = "Histograma de diferença percentual de PIB per Capita entre 1920 e 1940" ,
         x = "Diferença Percentual (%)", y = "Número de Cidades", subtitle = Subtitle,
         caption = '')  + 
    theme_minimal(base_size = 14) +
    geom_vline(aes(xintercept= median(D_PIB_Capita), color="red"), linetype="dashed", size = 1.5) +
    geom_vline(aes(xintercept=0), size = 1) +
    theme_hist_gabe()
  
  ggsave(filename = 'HD_AMC_PIB.jpg', plot = HD_AMC_PIB,
         width = 1920/144, height = 1080/144)
}

# Maps PIB ----------------------------------------------------------------


AMCgeo <- geobr::read_comparable_areas(1920, 1940) 
colnames(AMCgeo)[c(1,2)] <- c('AMC', 'codMun')

# M20_AMC_AdmPub
{ANO <- 20
  X <- 'PIB_Capita'
  Q <- 0.98}

{var_graphs_gabe(X)
  
  tp_AMC <- filter(AMCs2040, Ano == ANO)
  AMC_Corte <- filter(tp_AMC, tp_AMC[,X] < quantile(tp_AMC[,X], Q, na.rm = T))
  tp_Cortadas <- length(tp_AMC[,X]) - length(AMC_Corte[,X]) 
  tp_Subtitle <- paste0('Foram cortadas ', tp_Cortadas, ' cidades de um total de ', length(tp_AMC[,X]))
  AMC_Corte$AMC <- as.numeric(AMC_Corte$AMC)
  
  Mapa <- right_join(AMC_Corte, AMCgeo, by = 'AMC')
  Mapa <- cbind(Mapa, st_coordinates(st_centroid(Mapa$geom)))
  maxMap <- plyr::round_any(max(AMC_Corte$PIB_Capita),1, f = ceiling)
  minMap <- plyr:: round_any(min(AMC_Corte$PIB_Capita),1, f = floor)
  Estados20 <- read_state(year = 1920, code_state = 'all')
  
  M20_AMC_PIB <- ggplot() + 
    geom_sf(data = Mapa$geom, aes(fill = unlist(Mapa['PIB_Capita']))) + 
    geom_sf(data = Estados20, fill = NA, size = .8, color = 'gray20') +
    scale_fill_distiller(palette = "Spectral", na.value= "gray85",
                         name = 'PIB per Capita', limits = c(minMap, maxMap)) +
    labs(title = paste0('Distribuição de PIB per Capita em 19', ANO), 
         subtitle = paste0('Ajustado por AMCs - ', tp_Subtitle), 
         caption = '') +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'M20_AMC_PIB.jpg', plot = M20_AMC_PIB,
         width = 1080/96, height = 1080/96)
}

# M20_AMC_AdmPub
{ANO <- 40
  X <- 'PIB_Capita'
  Q <- 0.98}

{var_graphs_gabe(X)
  
  tp_AMC <- filter(AMCs2040, Ano == ANO)
  AMC_Corte <- filter(tp_AMC, tp_AMC[,X] < quantile(tp_AMC[,X], Q, na.rm = T))
  tp_Cortadas <- length(tp_AMC[,X]) - length(AMC_Corte[,X]) 
  tp_Subtitle <- paste0('Foram cortadas ', tp_Cortadas, ' cidades de um total de ', length(tp_AMC[,X]))
  AMC_Corte$AMC <- as.numeric(AMC_Corte$AMC)
  
  Mapa <- right_join(AMC_Corte, AMCgeo, by = 'AMC')
  Mapa <- cbind(Mapa, st_coordinates(st_centroid(Mapa$geom)))
  maxMap <- plyr::round_any(max(AMC_Corte$PIB_Capita),1, f = ceiling)
  minMap <- plyr:: round_any(min(AMC_Corte$PIB_Capita),1, f = floor)
  Estados20 <- read_state(year = 1920, code_state = 'all')
  
  M40_AMC_PIB <- ggplot() + 
    geom_sf(data = Mapa$geom, aes(fill = unlist(Mapa['PIB_Capita']))) + 
    geom_sf(data = Estados20, fill = NA, size = .8, color = 'gray20') +
    scale_fill_distiller(palette = "Spectral", na.value= "gray85",
                         name = 'PIB per Capita', limits = c(minMap, maxMap)) +
    labs(title = 'Distribuição de PIB per Capita em 1939', 
         subtitle = paste0('Ajustado por AMCs - ', tp_Subtitle), 
         caption = '') +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'M40_AMC_PIB.jpg', plot = M40_AMC_PIB,
         width = 1080/96, height = 1080/96)
}

Q <- 0.98
{D_AMC_Corte <- filter(D_AMC, D_PIB_Capita < quantile(D_AMC$D_PIB_Capita, Q, na.rm = T))
  Cortadas <- length(D_AMC$D_PIB_Capita) - length(D_AMC_Corte$D_PIB_Capita) 
  Subtitle <- paste0('Foram cortadas ', Cortadas, ' cidades de um total de ', length(D_AMC$D_PIB_Capita))
  
  D_AMC_Corte$AMC <- as.numeric(D_AMC_Corte$AMC)
  Mapa <- right_join(D_AMC_Corte, AMCgeo, by = 'AMC')
  Mapa <- cbind(Mapa, st_coordinates(st_centroid(Mapa$geom)))
  
  maxMap <- plyr::round_any(max(D_AMC_Corte$D_PIB_Capita),1, f = ceiling)
  minMap <- plyr:: round_any(min(D_AMC_Corte$D_PIB_Capita),1, f = floor)
  Estados20 <- read_state(year = 1920, code_state = 'all')
  
  MD_AMC_PIB <- ggplot() + 
    geom_sf(data = Mapa$geom, aes(fill = unlist(Mapa['D_AdmPub_PT']))) + 
    geom_sf(data = Estados20, fill = NA, size = .8, color = 'gray20') +
    scale_fill_distiller(palette = "Spectral", na.value= "gray85",
                         name = 'Porcentagem (%)', limits = c(minMap, maxMap)) +
    labs(title = 'Diferença percentual de PIB per Capita entre 1920 e 1939', 
         subtitle = paste0('Ajustado por AMCs - ', tp_Cortadas, ' AMCs Cortadas. '), 
         caption = '') +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'MD_AMC_PIB.jpg', plot = MD_AMC_PIB,
         width = 1080/96, height = 1080/96)
}


# Maps PIB UF -------------------------------------------------------------

states <- read_state(year = 1940, code_state = 'all')

# Adm Pub Level

{MapUF20 <- left_join(states, Censo20_UF, by = 'name_state')
  UF_Points <- cbind(MapUF20, st_coordinates(st_centroid(MapUF20$geom)))
  maxMapUF <- plyr::round_any(max(Censo20_UF$PIB_Capita), 1, f = ceiling)
  minMapUF <- plyr::round_any(min(Censo20_UF$PIB_Capita), 1, f = floor)
  
  M20_UF_PIB <- ggplot() + 
    geom_sf(data = MapUF20$geom, 
            aes(fill = MapUF20$PIB_Capita)) + 
    scale_fill_gradient(low = "Azure", high = "MidnightBlue",
                        na.value="gray85",
                        name = 'PIB per Capita',
                        limits = c(minMapUF, maxMapUF)) +
    geom_label(data = UF_Points,
               aes(X, Y, label = Estado20),
               size = 3,
               fontface = "bold") +
    labs(title = 'PIB per Capita em 1920',
         subtitle = 'Por Estado', 
         caption='') +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  ggsave(filename = 'M20_UF_PIB.jpg', plot = M20_UF_PIB,
         width = 1080/96, height = 1080/96)
}

{MapUF40 <- left_join(states, Censo40_UF, by = 'name_state')
  UF_Points <- cbind(MapUF40, st_coordinates(st_centroid(MapUF40$geom)))
  maxMapUF <- plyr::round_any(max(Censo40_UF$PIB_Capita), 1, f = ceiling)
  minMapUF <- plyr::round_any(min(Censo40_UF$PIB_Capita), 1, f = floor)
  
  M40_UF_PIB <- ggplot() + 
    geom_sf(data = MapUF40$geom, 
            aes(fill = MapUF40$PIB_Capita)) + 
    scale_fill_gradient(low = "Azure", high = "MidnightBlue",
                        na.value="gray85",
                        name = 'PIB per Capita',
                        limits = c(minMapUF, maxMapUF)) +
    geom_label(data = UF_Points,
               aes(X, Y, label = Estado20),
               size = 3,
               fontface = "bold") +
    labs(title = 'PIB per Capita em 1939',
         subtitle = 'Por Estado', 
         caption='') +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  ggsave(filename = 'M40_UF_PIB.jpg', plot = M40_UF_PIB,
         width = 1080/96, height = 1080/96)
}


{MapUF <- left_join(states, D_UF, by = 'name_state')
  UF_Points <- cbind(MapUF, st_coordinates(st_centroid(MapUF$geom)))
  maxMapUF <- plyr::round_any(max(D_UF$D_PIB_Capita), 1, f = ceiling)
  minMapUF <- plyr::round_any(min(D_UF$D_PIB_Capita), 1, f = floor)
  
  MD_UF_PIB <- ggplot() + 
    geom_sf(data = MapUF$geom, 
            aes(fill = MapUF$D_PIB_Capita)) + 
    scale_fill_gradient(low = "Azure", high = "MidnightBlue",
                        na.value="gray85",
                        name = 'Porcentagem (%)',
                        limits = c(minMapUF, maxMapUF)) +
    geom_label(data = UF_Points,
               aes(X, Y, label = Estado20),
               size = 3,
               fontface = "bold") +
    labs(title = 'Diferença percentual de PIB per Capita entre 1920 e 1939 ',
         subtitle = 'Por Estado (1940)', 
         caption='') +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'MD_UF_PIB.jpg', plot = MD_UF_PIB,
         width = 1080/96, height = 1080/96)
  
}


# BoxPlot PIB -------------------------------------------------------------

{
  B20_PIB <- ggplot(AMCs20, aes(x = reorder(Estado20, PIB_Capita, mean,  na.rm = T), # Clusters
                                     y = PIB_Capita,  # Variable
                                     fill = Region_1)) + # Criteria for colors
    geom_boxplot() + 
    coord_cartesian(ylim = c(0, 2.5)) +
    geom_hline(aes(yintercept= median(PIB_Capita, na.rm = T), color="Mediana"), size = 1 ) +
    geom_hline(aes(yintercept= mean(PIB_Capita, na.rm = T), color="Média"), size = 1 ) +
    geom_point( shape = 23, data=subset(AMCs20, Capital == TRUE), 
                aes(x=reorder(Estado20, PIB_Capita, median,  na.rm = T), y=PIB_Capita), position=position_dodge(1),
                color="black", size=4, show.legend = FALSE) +
    scale_color_manual(name = 'Estatísticas', values = c(Mediana = 'blue', Média = 'red')) + 
    scale_fill_discrete(name = 'Região') + 
    theme_minimal(base_size = 14) + 
    labs(title = "Distribuição do PIB per Capita por Estado em 1920",
         subtitle = ' Pontos representam PIB per Capita por município.  \n Ordenado por médias. Capitais são os losangos em destaque. ',
         x = 'Estado', y = "PIB per Capita", 
         caption = '') +
    theme_boxplot_gabe()
  
  ggsave(filename = "B20_PIB.jpg", plot = B20_PIB,
         width = 1920/144, height = 1080/144)
}


{
  B40_PIB <- ggplot(AMCs40, aes(x = reorder(Estado20, PIB_Capita, mean,  na.rm = T), # Clusters
                                        y = PIB_Capita,  # Variable
                                        fill = Region_1)) + # Criteria for colors
    geom_boxplot() + 
    coord_cartesian(ylim = c(0,7.5)) +
    geom_hline(aes(yintercept= median(PIB_Capita, na.rm = T), color="Mediana"), size = 1 ) +
    geom_hline(aes(yintercept= mean(PIB_Capita[PIB_Capita<= quantile(PIB_Capita, 0.985, na.rm=T)]   , na.rm = T), color="Média"), size = 1 ) +
    geom_point( shape = 23, data=subset(AMCs40, Capital == TRUE), 
                aes(x=reorder(Estado20, PIB_Capita, median,  na.rm = T), y=PIB_Capita), position=position_dodge(1),
                color="black", size=4, show.legend = FALSE) +
    scale_color_manual(name = 'Estatísticas', values = c(Mediana = 'blue', Média = 'red')) + 
    scale_fill_discrete(name = 'Região') + 
    theme_minimal(base_size = 14) + 
    labs(title = "Distribuição do PIB per Capita por Estado em 1939",
         subtitle = ' Pontos representam PIB per Capita por município.  \n Ordenado por médias. Capitais são os losangos em destaque. ',
         x = 'Estado', y = "PIB per Capita", 
         caption = '') +
    theme_boxplot_gabe()

  ggsave(filename = "B40_PIB.jpg", plot = B40_PIB,
         width = 1920/144, height = 1080/144)
}


# Dumbbell PIB ------------------------------------------------------------

Censo20_Dumbbell <- filter(Censo20_UF, !is.na(Censo20_UF$PIB_1920))
Censo40_Dumbbell <- filter(Censo40_UF, !is.na(Censo40_UF$PIB_1939))

Censo_DDD <- rbind(Censo20_Dumbbell, Censo40_Dumbbell)
Censo_DDD$Ano <- as.factor(Censo_DDD$Ano)

Region_1_Sul_1 <- c('RS', 'SC', 'PR')
Region_1_Sudeste_2 <- c('SP', 'MG', 'RJ', 'ES')
Region_1_Nordeste_3 <- c('BA', 'SE', 'AL', 'PE', 'PB', 'RN', 'CE', 'PI', 'MA')

Censo_DDD$Region <- c()
for (i in 1:length(Censo_DDD$Estado20)){
  if (substr(Censo_DDD$Estado20[i],1,2) %in% Region_1_Sul_1){
    Censo_DDD$Region[i] <- 'Sul'
  } else if (substr(Censo_DDD$Estado20[i],1,2) %in% Region_1_Sudeste_2){
    Censo_DDD$Region[i] <- 'Sudeste'
  } else {
    Censo_DDD$Region[i] <- 'Nordeste'
  }
}

{
C20 <- Censo_DDD %>% 
  filter(Ano == 20)
C40 <- Censo_DDD %>% 
  filter(Ano == 40)

dif <- plyr::round_any(C40$PIB_Capita - C20$PIB_Capita, 0.1, f = floor)
order <-  C20$PIB_Capita + (dif/2)
stats <- Censo_DDD %>%
  group_by(Ano) %>%
  summarise(mean = mean(PIB_Capita),
            SE = sd(PIB_Capita)) %>%
  mutate(meanpos = mean + 1 *SE, 
         meanneg = mean - 1 *SE)
stats_20 <- stats %>%
  filter(Ano == 20)
stats_40 <- stats %>%
  filter(Ano  == 40)
diff <- Censo_DDD %>% 
  filter(Ano == 20) %>% 
  mutate(x_pos = C20$PIB_Capita + (dif/2)) 
}

{DP_PIB_UF <- ggplot(Censo_DDD) + 
  geom_rect(xmin = stats_20$meanneg, xmax = stats_20$meanpos,
            ymin = 0, ymax = 17, fill = "#762a83", alpha = .005)+
  geom_vline(xintercept = stats_20$mean, linetype = "solid", size = .5, alpha = .8, color = "#762a83") +
  geom_rect(xmin = stats_40$meanneg, xmax = stats_40$meanpos,
            ymin = 0, ymax = 17, fill = "#009688", alpha = .005)+
  geom_vline(xintercept = stats_40$mean, color = "#009688", linetype = "solid",  size = .5, alpha = .8) +
  geom_segment(data = C20,
               aes(x = PIB_Capita, y = reorder(Estado20,order),
                   yend = C40$Estado20, xend = C40$PIB_Capita), 
               color = "#aeb6bf",
               size = 5.5, 
               alpha = .5) +
  geom_point(aes(x = PIB_Capita, y = Estado20, color = Ano), size = 6, show.legend = TRUE) +
  geom_text(data = diff,
            aes(label = paste("D: ",dif), x = (PIB_Capita + (dif/2)), y = Estado20), 
            # fill = "white",
            color = "#4a4e4d",
            size = 3.5) +
  geom_text(x = (stats_40$mean + 0.13) , y = 2, label = "MÉDIA", angle = 90, size = 4, color = "#009688")+
  geom_text(x = (stats_40$meanpos +0.13), y = 2, label = "DESVIO PADRÃO", angle = 90, size = 4, color = "#009688")+
  scale_color_manual(values = c("#762a83", "#009688")) +
  labs(title = 'PIB per Capita',
       subtitle = "Valores por Estado em <span style = 'color: #762a83;'>**1920**</span> e <span style = 'color: #009688;'>**1939**</span> <br>",
       caption = "",
       x = 'PIB per Capita',
       y = '') +
  theme_minimal(base_size = 15) +
  theme(plot.subtitle = element_markdown(size = 15)) +
  theme(legend.position="none") +
  theme(plot.title=element_text(face = "bold",  size=16, vjust=1.25, hjust = 0)) +
  theme(plot.caption = element_text(face = "plain", size = 13)) +
  theme(axis.text.y=element_text(size=12, angle=0,hjust=0.95,vjust=0.2))
  
  #Save Plot
  ggsave(filename = 'DP_PIB_UF.jpg', plot = DP_PIB_UF,
         width = 1920/144, height = 1080/144)
}