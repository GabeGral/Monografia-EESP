## Gabriel Gral
## June 2022
## Script 02

## Keeping all databases in the same directory makes importing easier
getwd() #- This is the directory in which the bases are
list.files() #- These are the available files and databases

## Datasets used in this script: 
## Make sure all the datasets in the 'Load Data' section are in your working directory

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
    theme(plot.title=element_text(face = "bold",  size=16, vjust=1.25, hjust = 0)) + 
    theme(plot.subtitle=element_text(face = "italic",  size=15, vjust=1.25)) +
    theme(plot.caption = element_text(face = "plain", size = 13)) 
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





# Rank --------------------------------------------------------------------

Rank20_UF <- Censo2040_UF %>% 
  filter(Ano == 20) %>% 
  arrange(-SegPub_PT) %>% 
  add_column(c(1:16)) %>%
  setNames(c('Estado20', 'Ano', 'AdmPub', 'SegPub','PopTotal', 'PIB_1920', 'PIB_1939', 'PIB_Capita', 'AdmPub_PT', 'SegPub_PT', 'Rank20'))

Rank40_UF <- Censo2040_UF %>% 
  filter(Ano == 40) %>% 
  arrange(-SegPub_PT) %>% 
  add_column(c(1:16)) %>%
  setNames(c('Estado20', 'Ano', 'AdmPub', 'SegPub','PopTotal', 'PIB_1920', 'PIB_1939', 'PIB_Capita', 'AdmPub_PT', 'SegPub_PT', 'Rank40'))

RankAdmPub_UF <- Rank20_UF %>% 
  bind_rows(Rank40_UF) %>% 
  mutate(Rank = coalesce(Rank20, Rank40)) %>% 
  select(-c('Rank20', 'Rank40')) %>% 
  arrange(Estado20, Ano) 

Gain <- c()

for (i in 1:(length(RankAdmPub_UF$Rank)/2)){
  tp_v20 <- RankAdmPub_UF$Rank[2*i-1]
  tp_v40 <- RankAdmPub_UF$Rank[2*i]
  Gain[2*i] <- (tp_v20 - tp_v40)
  rm(list=ls(pattern="^tp_"))
}

RankAdmPub_UF <- cbind(RankAdmPub_UF, Gain = Gain)
RankAdmPub_UF

# Histograms AMC ----------------------------------------------------------------

# What do I want to produce? (Names of objects)
  ## Histogram for data on 1920 and 1940 
          # H20_AMC_AdmPub 
          # H40_AMC_AdmPub
          # H20_AMC_SegPub
          # H40_AMC_SegPub
  
  ## Histogram for difference
          # HD_AMC_AdmPub
          # HD_AMC_SegPub
  
#-- Histogram for data on 1920 and 1940 

## Auxiliary function
var_graphs_gabe <- function(X){
  if (X == 'AdmPub_PT'){
    x <<- 11
    y <<- 'funcionários da administração pública'
    z <<- 7
    } else {
      x <<- 12
      y <<- 'servidores da segurança pública'
      z <<- 8
    }
}

# H20_AMC_AdmPub 
{ANO <- 20
X <- 'AdmPub_PT'
Q <- 0.95}

{
  var_graphs_gabe(X)
  tp_AMC <- filter(AMCs2040, Ano == ANO)
  AMC_Corte <- filter(tp_AMC, tp_AMC[,x] < quantile(tp_AMC[,x], Q, na.rm = T))
  tp_Cortadas <- length(tp_AMC[,x]) - length(AMC_Corte[,x]) 
  tp_Subtitle <- paste0('Foram cortadas ', tp_Cortadas, ' cidades de um total de ', length(tp_AMC[,x]))
  
  tp_H <- ggplot(AMC_Corte, aes(x= AMC_Corte[,x]))+
    geom_histogram(color="black", fill="grey", alpha = 0.7, bins = 80) + 
    labs(title = paste0("Histograma de ", y, " por mil habitantes em 19", ANO) ,
         x = paste0(y, ' por mil habitantes'), y = "Número de Cidades", subtitle = tp_Subtitle,
         caption = "") + 
    theme_minimal(base_size = 14) +
    geom_vline(aes(xintercept= median(AMC_Corte[,x]), color="red"), linetype="dashed", size = 1.5) +
    geom_vline(aes(xintercept=0), size = 1) +
    theme_hist_gabe()
  
  tp_H
  tp_Name <- paste0('H', ANO, '_AMC_', substr(X, 1, 6))
  assign(tp_Name, tp_H)
  
  ggsave(filename = 'H20_AMC_AdmPub.jpg', plot = H20_AMC_AdmPub,
         width = 1920/144, height = 1080/144)
}


# H40_AMC_AdmPub
{ANO <- 40
X <- 'AdmPub_PT'
Q <- 0.95}

{
  var_graphs_gabe(X)
  tp_AMC <- filter(AMCs2040, Ano == ANO)
  AMC_Corte <- filter(tp_AMC, tp_AMC[,x] < quantile(tp_AMC[,x], Q, na.rm = T))
  tp_Cortadas <- length(tp_AMC[,x]) - length(AMC_Corte[,x]) 
  tp_Subtitle <- paste0('Foram cortadas ', tp_Cortadas, ' cidades de um total de ', length(tp_AMC[,x]))
  
  tp_H <- ggplot(AMC_Corte, aes(x= AMC_Corte[,x]))+
    geom_histogram(color="black", fill="grey", alpha = 0.7, bins = 80) + 
    labs(title = paste0("Histograma de ", y, " por mil habitantes em 19", ANO) ,
         x = paste0(y, ' por mil habitantes'), y = "Número de Cidades", subtitle = tp_Subtitle,
         caption = "") + 
    theme_minimal(base_size = 14) +
    geom_vline(aes(xintercept= median(AMC_Corte[,x]), color="red"), linetype="dashed", size = 1.5) +
    geom_vline(aes(xintercept=0), size = 1) +
    theme_hist_gabe()
  
  tp_H
  tp_Name <- paste0('H', ANO, '_AMC_', substr(X, 1, 6))
  assign(tp_Name, tp_H)
  rm(list=ls(pattern="^tp_"))
  
  ggsave(filename = 'H40_AMC_AdmPub.jpg', plot = H40_AMC_AdmPub,
         width = 1920/144, height = 1080/144)
}

# H20_AMC_SegPub
{ANO <- 20
X <- 'SegPub_PT'
Q <- 0.95}

{
  var_graphs_gabe(X)
  tp_AMC <- filter(AMCs2040, Ano == ANO)
  AMC_Corte <- filter(tp_AMC, tp_AMC[,x] < quantile(tp_AMC[,x], Q, na.rm = T))
  tp_Cortadas <- length(tp_AMC[,x]) - length(AMC_Corte[,x]) 
  tp_Subtitle <- paste0('Foram cortadas ', tp_Cortadas, ' cidades de um total de ', length(tp_AMC[,x]))
  
  tp_H <- ggplot(AMC_Corte, aes(x= AMC_Corte[,x]))+
    geom_histogram(color="black", fill="grey", alpha = 0.7, bins = 80) + 
    labs(title = paste0("Histograma de ", y, " por mil habitantes em 19", ANO) ,
         x = paste0(y, ' por mil habitantes'), y = "Número de Cidades", subtitle = tp_Subtitle,
         caption = "") + 
    theme_minimal(base_size = 14) +
    geom_vline(aes(xintercept= median(AMC_Corte[,x]), color="red"), linetype="dashed", size = 1.5) +
    geom_vline(aes(xintercept=0), size = 1) +
    theme_hist_gabe()
  
  tp_H
  tp_Name <- paste0('H', ANO, '_AMC_', substr(X, 1, 6))
  assign(tp_Name, tp_H)
  rm(list=ls(pattern="^tp_"))
  
  ggsave(filename = 'H20_AMC_SegPub.jpg', plot = H20_AMC_SegPub,
         width = 1920/144, height = 1080/144)
}

# H40_AMC_SegPub
{ANO <- 40
X <- 'SegPub_PT'
Q <- 0.95}

{
  var_graphs_gabe(X)
  tp_AMC <- filter(AMCs2040, Ano == ANO)
  AMC_Corte <- filter(tp_AMC, tp_AMC[,x] < quantile(tp_AMC[,x], Q, na.rm = T))
  tp_Cortadas <- length(tp_AMC[,x]) - length(AMC_Corte[,x]) 
  tp_Subtitle <- paste0('Foram cortadas ', tp_Cortadas, ' cidades de um total de ', length(tp_AMC[,x]))
  
  tp_H <- ggplot(AMC_Corte, aes(x= AMC_Corte[,x]))+
    geom_histogram(color="black", fill="grey", alpha = 0.7, bins = 80) + 
    labs(title = paste0("Histograma de ", y, " por mil habitantes em 19", ANO) ,
         x = paste0(y, ' por mil habitantes'), y = "Número de Cidades", subtitle = tp_Subtitle,
         caption = "") + 
    theme_minimal(base_size = 14) +
    geom_vline(aes(xintercept= median(AMC_Corte[,x]), color="red"), linetype="dashed", size = 1.5) +
    geom_vline(aes(xintercept=0), size = 1) +
    theme_hist_gabe()
  
  tp_H
  tp_Name <- paste0('H', ANO, '_AMC_', substr(X, 1, 6))
  assign(tp_Name, tp_H)
  rm(list=ls(pattern="^tp_"))
  
  ggsave(filename = 'H40_AMC_SegPub.jpg', plot = H40_AMC_SegPub,
         width = 1920/144, height = 1080/144)
}

#--Histogram for difference

# HD_AMC_AdmPub

Q <- 0.95
{D_AMC_Corte <- filter(D_AMC, D_AdmPub_PT < quantile(D_AdmPub_PT, Q, na.rm=T))
  Cortadas <- length(D_AMC$D_AdmPub) - length(D_AMC_Corte$D_AdmPub) 
  Subtitle <- paste0('Foram cortadas ', Cortadas, ' cidades de um total de ', length(D_AMC$D_AdmPub))
  
  HD_AMC_AdmPub <- ggplot(D_AMC_Corte, aes(x= D_AdmPub_PT))+
    geom_histogram(color="black", fill="grey", alpha = 0.7, bins = 80) + 
    labs(title = "Histograma da diferença de funcionários da administração pública por mil habitantes entre 1920 e 1940" ,
         x = "Diferença Percentual (%)", y = "Número de Cidades", subtitle = Subtitle,
         caption = "") + 
    theme_minimal(base_size = 14) +
    geom_vline(aes(xintercept= median(D_AdmPub_PT), color="red"), linetype="dashed", size = 1.5) +
    geom_vline(aes(xintercept=0), size = 1) +
    theme_hist_gabe()
  
  ggsave(filename = 'HD_AMC_AdmPub.jpg', plot = HD_AMC_AdmPub,
         width = 1920/144, height = 1080/144)
}
  
# HD_AMC_SegPub

Q <- 0.95
{D_AMC_Corte <- filter(D_AMC, D_SegPub_PT < quantile(D_SegPub_PT, Q, na.rm=T))
  Cortadas <- length(D_AMC$D_SegPub) - length(D_AMC_Corte$D_SegPub) 
  Subtitle <- paste0('Foram cortadas ', Cortadas, ' cidades de um total de ', length(D_AMC$D_SegPub))
  
  HD_AMC_SegPub <- ggplot(D_AMC_Corte, aes(x= D_SegPub_PT))+
    geom_histogram(color="black", fill="grey", alpha = 0.7, bins = 80) + 
    labs(title = "Histograma da diferença de Servidores da Segurança Pública por mil habitantes entre 1920 e 1940" ,
         x = "Diferença Percentual (%)", y = "Número de Cidades", subtitle = Subtitle,
         caption = "") + 
    theme_minimal(base_size = 14) +
    geom_vline(aes(xintercept= median(D_SegPub_PT), color="red"), linetype="dashed", size = 1.5) +
    geom_vline(aes(xintercept=0), size = 1) +
    theme_hist_gabe()
 
  ggsave(filename = 'HD_AMC_SegPub.jpg', plot = HD_AMC_SegPub,
         width = 1920/144, height = 1080/144)
}


# Maps AMC ----------------------------------------------------------------

# What do I want to produce? (Names of objects)

## Maps for data on 1920 and 1940
# M20_AMC_AdmPub
# M40_AMC_AdmPub
# M20_AMC_SegPub
# M40_AMC_SegPub

## Map for difference 
# MD_AMC_AdmPub
# MD_AMC_AdmPub


AMCgeo <- geobr::read_comparable_areas(1920, 1940) 
colnames(AMCgeo)[c(1,2)] <- c('AMC', 'codMun')

# M20_AMC_AdmPub
{ANO <- 20
  X <- 'AdmPub_PT'
  Q <- 0.95}

{var_graphs_gabe(X)

  tp_AMC <- filter(AMCs2040, Ano == ANO)
  AMC_Corte <- filter(tp_AMC, tp_AMC[,x] < quantile(tp_AMC[,x], Q, na.rm = T))
  tp_Cortadas <- length(tp_AMC[,x]) - length(AMC_Corte[,x]) 
  tp_Subtitle <- paste0('Foram cortadas ', tp_Cortadas, ' cidades de um total de ', length(tp_AMC[,x]))
  AMC_Corte$AMC <- as.numeric(AMC_Corte$AMC)

  Mapa <- right_join(AMC_Corte, AMCgeo, by = 'AMC')
  Mapa <- cbind(Mapa, st_coordinates(st_centroid(Mapa$geom)))
  maxMap <- plyr::round_any(max(AMC_Corte$AdmPub_PT),1, f = ceiling)
  minMap <- plyr:: round_any(min(AMC_Corte$AdmPub_PT),1, f = floor)
  Estados20 <- read_state(year = 1920, code_state = 'all')

  M20_AMC_AdmPub <- ggplot() + 
    geom_sf(data = Mapa$geom, aes(fill = unlist(Mapa['AdmPub_PT']))) + 
    geom_sf(data = Estados20, fill = NA, size = .8, color = 'gray20') +
    scale_fill_distiller(palette = "Spectral", na.value= "gray85",
                         name = 'Funcionários por mil habitantes', limits = c(minMap, maxMap)) +
    labs(title = paste0('Funcionários da administração pública em 19', ANO), 
         subtitle = paste0('Ajustado por AMCs - ', tp_Cortadas, ' AMCs Cortadas. Ajustado por crescimento populacional.'), 
         caption = "") +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'M20_AMC_AdmPub.jpg', plot = M20_AMC_AdmPub,
         width = 1080/96, height = 1080/96)
}

# M40_AMC_AdmPub
{ANO <- 40
  X <- 'AdmPub_PT'
  Q <- 0.95}

{var_graphs_gabe(X)
  
  tp_AMC <- filter(AMCs2040, Ano == ANO)
  AMC_Corte <- filter(tp_AMC, tp_AMC[,x] < quantile(tp_AMC[,x], Q, na.rm = T))
  tp_Cortadas <- length(tp_AMC[,x]) - length(AMC_Corte[,x]) 
  tp_Subtitle <- paste0('Foram cortadas ', tp_Cortadas, ' cidades de um total de ', length(tp_AMC[,x]))
  AMC_Corte$AMC <- as.numeric(AMC_Corte$AMC)
  
  Mapa <- right_join(AMC_Corte, AMCgeo, by = 'AMC')
  Mapa <- cbind(Mapa, st_coordinates(st_centroid(Mapa$geom)))
  maxMap <- plyr::round_any(max(AMC_Corte$AdmPub_PT),1, f = ceiling)
  minMap <- plyr:: round_any(min(AMC_Corte$AdmPub_PT),1, f = floor)
  Estados20 <- read_state(year = 1920, code_state = 'all')
  
  M40_AMC_AdmPub <- ggplot() + 
    geom_sf(data = Mapa$geom, aes(fill = unlist(Mapa['AdmPub_PT']))) + 
    geom_sf(data = Estados20, fill = NA, size = .8, color = 'gray20') +
    scale_fill_distiller(palette = "Spectral", na.value= "gray85",
                         name = 'Funcionários por mil habitantes', limits = c(minMap, maxMap)) +
    labs(title = paste0('Funcionários da administração pública em 19', ANO), 
         subtitle = paste0('Ajustado por AMCs - ', tp_Cortadas, ' AMCs Cortadas. Ajustado por crescimento populacional.'), 
         caption = "") +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'M40_AMC_AdmPub.jpg', plot = M40_AMC_AdmPub,
         width = 1080/96, height = 1080/96)
}

# M20_AMC_SegPub

{ANO <- 20
  X <- 'SegPub_PT'
  Q <- 0.92}

{var_graphs_gabe(X)
  
  tp_AMC <- filter(AMCs2040, Ano == ANO)
  AMC_Corte <- filter(tp_AMC, tp_AMC[,x] < quantile(tp_AMC[,x], Q, na.rm = T))
  tp_Cortadas <- length(tp_AMC[,x]) - length(AMC_Corte[,x]) 
  tp_Subtitle <- paste0('Foram cortadas ', tp_Cortadas, ' cidades de um total de ', length(tp_AMC[,x]))
  AMC_Corte$AMC <- as.numeric(AMC_Corte$AMC)
  
  Mapa <- right_join(AMC_Corte, AMCgeo, by = 'AMC')
  Mapa <- cbind(Mapa, st_coordinates(st_centroid(Mapa$geom)))
  maxMap <- plyr::round_any(max(AMC_Corte$SegPub_PT),1, f = ceiling)
  minMap <- plyr:: round_any(min(AMC_Corte$SegPub_PT),1, f = floor)
  Estados20 <- read_state(year = 1920, code_state = 'all')
  
  M20_AMC_SegPub <- ggplot() + 
    geom_sf(data = Mapa$geom, aes(fill = unlist(Mapa['SegPub_PT']))) + 
    geom_sf(data = Estados20, fill = NA, size = .8, color = 'gray20') +
    scale_fill_distiller(palette = "Spectral", na.value= "gray85",
                         name = 'Servidores por mil habitantes', limits = c(minMap, maxMap)) +
    labs(title = paste0('Servidores da segurança pública em 19', ANO), 
         subtitle = paste0('Ajustado por AMCs - ', tp_Cortadas, ' AMCs Cortadas. Ajustado por crescimento populacional.'), 
         caption = "") +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'M20_AMC_SegPub.jpg', plot = M20_AMC_SegPub,
         width = 1080/96, height = 1080/96)
}

# M40_AMC_SegPub

{ANO <- 40
  X <- 'SegPub_PT'
  Q <- 0.97}

{var_graphs_gabe(X)
  
  tp_AMC <- filter(AMCs2040, Ano == ANO)
  AMC_Corte <- filter(tp_AMC, tp_AMC[,x] > quantile(tp_AMC[,x], Q, na.rm = T))
  tp_Cortadas <- length(tp_AMC[,x]) - length(AMC_Corte[,x]) 
  tp_Subtitle <- paste0('Foram cortadas ', tp_Cortadas, ' cidades de um total de ', length(tp_AMC[,x]))
  AMC_Corte$AMC <- as.numeric(AMC_Corte$AMC)
  
  Mapa <- right_join(AMC_Corte, AMCgeo, by = 'AMC')
  Mapa <- cbind(Mapa, st_coordinates(st_centroid(Mapa$geom)))
  maxMap <- plyr::round_any(max(AMC_Corte$SegPub_PT),1, f = ceiling)
  minMap <- plyr:: round_any(min(AMC_Corte$SegPub_PT),1, f = floor)
  Estados20 <- read_state(year = 1920, code_state = 'all')
  
  M40_AMC_SegPub <- ggplot() + 
    geom_sf(data = Mapa$geom, aes(fill = unlist(Mapa['SegPub_PT']))) + 
    geom_sf(data = Estados20, fill = NA, size = .8, color = 'gray20') +
    scale_fill_distiller(palette = "Spectral", na.value= "gray85",
                         name = 'Servidores por mil habitantes', limits = c(minMap, maxMap)) +
    labs(title = paste0('Servidores da segurança pública em 19', ANO), 
         subtitle = paste0('Ajustado por AMCs - ', tp_Cortadas, ' AMCs Cortadas. Ajustado por crescimento populacional.'), 
         caption = "") +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'M40_AMC_SegPub.jpg', plot = M40_AMC_SegPub,
         width = 1080/96, height = 1080/96)
}
M40_AMC_SegPub

# MD_AMC_AdmPub

Q <- 0.95
{D_AMC_Corte <- filter(D_AMC, D_AdmPub_PT < quantile(D_AMC$D_AdmPub_PT, Q, na.rm = T))
  Cortadas <- length(D_AMC$D_AdmPub) - length(D_AMC_Corte$D_AdmPub) 
  Subtitle <- paste0('Foram cortadas ', Cortadas, ' cidades de um total de ', length(D_AMC$D_AdmPub))
  
  D_AMC_Corte$AMC <- as.numeric(D_AMC_Corte$AMC)
  Mapa <- right_join(D_AMC_Corte, AMCgeo, by = 'AMC')
  Mapa <- cbind(Mapa, st_coordinates(st_centroid(Mapa$geom)))

  maxMap <- plyr::round_any(max(D_AMC_Corte$D_AdmPub_PT),1, f = ceiling)
  minMap <- plyr:: round_any(min(D_AMC_Corte$D_AdmPub_PT),1, f = floor)
  Estados20 <- read_state(year = 1920, code_state = 'all')
  
  MD_AMC_AdmPub <- ggplot() + 
    geom_sf(data = Mapa$geom, aes(fill = unlist(Mapa['D_AdmPub_PT']))) + 
    geom_sf(data = Estados20, fill = NA, size = .8, color = 'gray20') +
    scale_fill_distiller(palette = "Spectral", na.value= "gray85",
                         name = 'Porcentagem (%)', limits = c(minMap, maxMap)) +
    labs(title = 'Diferença percentual de funcionários da administração pública', 
         subtitle = paste0('Ajustado por AMCs - ', Cortadas, ' AMCs Cortadas. Ajustado por crescimento populacional.'), 
         caption = "") +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'MD_AMC_AdmPub.jpg', plot = MD_AMC_AdmPub,
         width = 1080/96, height = 1080/96)
}

# MD_AMC_SegPub

Q <- 0.95
{D_AMC_Corte <- filter(D_AMC, D_SegPub_PT < quantile(D_AMC$D_SegPub_PT, Q, na.rm = T))
  Cortadas <- length(D_AMC$D_SegPub) - length(D_AMC_Corte$D_SegPub) 
  Subtitle <- paste0('Foram cortadas ', Cortadas, ' cidades de um total de ', length(D_AMC$D_SegPub))
  
  D_AMC_Corte$AMC <- as.numeric(D_AMC_Corte$AMC)
  Mapa <- right_join(D_AMC_Corte, AMCgeo, by = 'AMC')
  Mapa <- cbind(Mapa, st_coordinates(st_centroid(Mapa$geom)))
  
  maxMap <- plyr::round_any(max(D_AMC_Corte$D_SegPub_PT),1, f = ceiling)
  minMap <- plyr:: round_any(min(D_AMC_Corte$D_SegPub_PT),1, f = floor)
  Estados20 <- read_state(year = 1920, code_state = 'all')
  
  MD_AMC_SegPub <- ggplot() + 
    geom_sf(data = Mapa$geom, aes(fill = unlist(Mapa['D_SegPub_PT']))) + 
    geom_sf(data = Estados20, fill = NA, size = .8, color = 'gray20') +
    scale_fill_distiller(palette = "Spectral", na.value= "gray85",
                         name = 'Porcentagem (%)', limits = c(minMap, maxMap)) +
    labs(title = 'Diferença percentual de servidores da segurança pública', 
         subtitle = paste0('Ajustado por AMCs - ', Cortadas, ' AMCs Cortadas. Ajustado por crescimento populacional.'), 
         caption = "") +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'MD_AMC_SegPub.jpg', plot = MD_AMC_SegPub,
         width = 1080/96, height = 1080/96)
}


# Maps UF -----------------------------------------------------------------


states <- read_state(year = 1940, code_state = 'all')

# Adm Pub Level

{MapUF20 <- left_join(states, Censo20_UF, by = 'name_state')
UF_Points <- cbind(MapUF20, st_coordinates(st_centroid(MapUF20$geom)))
maxMapUF <- plyr::round_any(max(Censo20_UF$AdmPub_PT), 1, f = ceiling)
minMapUF <- plyr::round_any(min(Censo20_UF$AdmPub_PT), 1, f = floor)

M20_UF_AdmPub <- ggplot() + 
  geom_sf(data = MapUF20$geom, 
          aes(fill = MapUF20$AdmPub_PT)) + 
  scale_fill_gradient(low = "Azure", high = "MidnightBlue",
                      na.value="gray85",
                      name = 'Funcionários por mil habitantes',
                      limits = c(minMapUF, maxMapUF)) +
  geom_label(data = UF_Points,
             aes(X, Y, label = Estado20),
             size = 3,
             fontface = "bold") +
  labs(title = 'Funcionários da administração pública por mil habitantes em 1920',
       subtitle = 'Por Estado', 
       caption='') +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme_map_gabe()
ggsave(filename = 'M20_UF_AdmPub.jpg', plot = M20_UF_AdmPub,
       width = 1080/96, height = 1080/96)
}



{MapUF40 <- left_join(states, Censo40_UF, by = 'name_state')
  UF_Points <- cbind(MapUF40, st_coordinates(st_centroid(MapUF40$geom)))
  maxMapUF <- plyr::round_any(max(Censo40_UF$AdmPub_PT), 1, f = ceiling)
  minMapUF <- plyr::round_any(min(Censo40_UF$AdmPub_PT), 1, f = floor)
  
  M40_UF_AdmPub <- ggplot() + 
    geom_sf(data = MapUF40$geom, 
            aes(fill = MapUF40$AdmPub_PT)) + 
    scale_fill_gradient(low = "Azure", high = "MidnightBlue",
                        na.value="gray85",
                        name = 'Funcionários por mil habitantes',
                        limits = c(minMapUF, maxMapUF)) +
    geom_label(data = UF_Points,
               aes(X, Y, label = Estado20),
               size = 3,
               fontface = "bold") +
    labs(title = 'Funcionários da administração pública por mil habitantes em 1940',
         subtitle = 'Por Estado', 
         caption='') +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'M40_UF_AdmPub.jpg', plot = M40_UF_AdmPub,
         width = 1080/96, height = 1080/96)
  
}

# Seg Pub Level

{MapUF20 <- left_join(states, Censo20_UF, by = 'name_state')
  UF_Points <- cbind(MapUF20, st_coordinates(st_centroid(MapUF20$geom)))
  maxMapUF <- plyr::round_any(max(Censo20_UF$SegPub_PT), 1, f = ceiling)
  minMapUF <- plyr::round_any(min(Censo20_UF$SegPub_PT), 1, f = floor)
  
  M20_UF_SegPub <- ggplot() + 
    geom_sf(data = MapUF20$geom, 
            aes(fill = MapUF20$SegPub_PT)) + 
    scale_fill_gradient(low = "Azure", high = "MidnightBlue",
                        na.value="gray85",
                        name = 'Servidores por mil habitantes',
                        limits = c(minMapUF, maxMapUF)) +
    geom_label(data = UF_Points,
               aes(X, Y, label = Estado20),
               size = 3,
               fontface = "bold") +
    labs(title = 'Servidores da segurança pública por mil habitantes em 1920',
         subtitle = 'Por Estado', 
         caption='') +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'M20_UF_SegPub.jpg', plot = M20_UF_SegPub,
         width = 1080/96, height = 1080/96)
  
}


{MapUF40 <- left_join(states, Censo40_UF, by = 'name_state')
  UF_Points <- cbind(MapUF40, st_coordinates(st_centroid(MapUF40$geom)))
  maxMapUF <- plyr::round_any(max(Censo40_UF$SegPub_PT), 1, f = ceiling)
  minMapUF <- plyr::round_any(min(Censo40_UF$SegPub_PT), 1, f = floor)
  
  M40_UF_SegPub <- ggplot() + 
    geom_sf(data = MapUF40$geom, 
            aes(fill = MapUF40$SegPub_PT)) + 
    scale_fill_gradient(low = "Azure", high = "MidnightBlue",
                        na.value="gray85",
                        name = 'Servidores por mil habitantes',
                        limits = c(minMapUF, maxMapUF)) +
    geom_label(data = UF_Points,
               aes(X, Y, label = Estado20),
               size = 3,
               fontface = "bold") +
    labs(title = 'Servidores da segurança pública por mil habitantes em 1940',
         subtitle = 'Por Estado', 
         caption='') +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'M40_UF_SegPub.jpg', plot = M40_UF_SegPub,
         width = 1080/96, height = 1080/96)
  
}


{MapUF <- left_join(states, D_UF, by = 'name_state')
  UF_Points <- cbind(MapUF, st_coordinates(st_centroid(MapUF$geom)))
  maxMapUF <- plyr::round_any(max(D_UF$D_AdmPub_PT), 1, f = ceiling)
  minMapUF <- plyr::round_any(min(D_UF$D_AdmPub_PT), 1, f = floor)
  
  MD_UF_AdmPub <- ggplot() + 
    geom_sf(data = MapUF$geom, 
            aes(fill = MapUF$D_AdmPub_PT)) + 
    scale_fill_gradient(low = "Azure", high = "MidnightBlue",
                        na.value="gray85",
                        name = 'Porcentagem (%)',
                        limits = c(minMapUF, maxMapUF)) +
    geom_label(data = UF_Points,
               aes(X, Y, label = Estado20),
               size = 3,
               fontface = "bold") +
    labs(title = 'Diferença percentual de funcionários da administração pública ',
         subtitle = 'Por Estado (1940), ajustado por crescimento populacional', 
         caption='') +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'MD_UF_AdmPub.jpg', plot = MD_UF_AdmPub,
         width = 1080/96, height = 1080/96)
  
}


{MapUF <- left_join(states, D_UF, by = 'name_state')
  UF_Points <- cbind(MapUF, st_coordinates(st_centroid(MapUF$geom)))
  maxMapUF <- plyr::round_any(max(D_UF$D_SegPub_PT), 1, f = ceiling)
  minMapUF <- plyr::round_any(min(D_UF$D_SegPub_PT), 1, f = floor)
  
  MD_UF_SegPub <- ggplot() + 
    geom_sf(data = MapUF$geom, 
            aes(fill = MapUF$D_SegPub_PT)) + 
    scale_fill_gradient(low = "Azure", high = "MidnightBlue",
                        na.value="gray85",
                        name = 'Porcentagem (%)',
                        limits = c(minMapUF, maxMapUF)) +
    geom_label(data = UF_Points,
               aes(X, Y, label = Estado20),
               size = 3,
               fontface = "bold") +
    labs(title = 'Diferença percentual de servidores da segurança pública ',
         subtitle = 'Por Estado (1940), ajustado por crescimento populacional', 
         caption='') +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme_map_gabe()
  
  ggsave(filename = 'MD_UF_SegPub.jpg', plot = MD_UF_SegPub,
         width = 1080/96, height = 1080/96)
  
}



## Maps for data on 1920 and 1940
## Histogram for data on 1920 and 1940 
## Map for difference 
## Histogram for difference



# Boxplots UF -------------------------------------------------------------

#B20_AdmPub
#B40_AdmPub
#20_SegPub
#40_SegPub


# Administração Pública 1920 
{
  # AMC20_Corte <- filter(AMCs20, SegPub_PT < 50)
  B20_AdmPub_UF <- ggplot(AMCs20, aes(x = reorder(Estado20, AdmPub_PT, mean,  na.rm = T), # Clusters
                                           y = AdmPub_PT, # Variable
                                           fill = Region_1)) + # Criteria for colors
    geom_boxplot() + 
    # coord_cartesian(ylim = c(0,10)) +
    geom_hline(aes(yintercept= median(AdmPub_PT), color="Mediana"), size = 1 ) +
    geom_hline(aes(yintercept= mean(AdmPub_PT), color="Média"), size = 1 ) + 
    geom_point( shape = 23, data=subset(AMCs20, Capital == TRUE), 
      aes(x=reorder(Estado20, AdmPub_PT, median,  na.rm = T), y=AdmPub_PT), position=position_dodge(1),
      color="black", size=4, show.legend = FALSE) +
    scale_color_manual(name = 'Estatísticas', values = c(Mediana = 'blue', Média = 'red')) + 
    scale_fill_discrete(name = 'Região') + 
    # scale_shape_manual(values = c(23), labels = c('Capitais')) +
    # stat_summary(fun.y=mean, geom="point", shape=20, size = 4, color="blue", fill="black") +
    theme_minimal(base_size = 14) + 
    labs(title = "Distribuição de funcionários da administração pública por Estado em 1920",
         subtitle = 'Pontos representam número de funcionários da administração pública por mil habitantes por AMC. \n Ordenado por médias. Capitais são os losangos em destaque.',
         x = 'Estado', y = "Servidores da Segurança Pública por Mil Habitantes", 
         caption = '') +
    theme_boxplot_gabe()
  
  ggsave(filename = 'B20_AdmPub_UF.jpg', plot = B20_AdmPub_UF,
         width = 1920/144, height = 1080/144)
  
}

# Administração Pública 1940
{
  # AMC20_Corte <- filter(AMCs20, SegPub_PT < 50)
  B40_AdmPub_UF <- ggplot(AMCs40, aes(x = reorder(Estado20, AdmPub_PT, mean,  na.rm = T), # Clusters
                                      y = AdmPub_PT, # Variable
                                      fill = Region_1)) + # Criteria for colors
    geom_boxplot() + 
    # coord_cartesian(ylim = c(0,30)) +
    geom_hline(aes(yintercept= median(AdmPub_PT, na.rm = TRUE), color="Mediana"), size = 1 ) +
    geom_hline(aes(yintercept= mean(AdmPub_PT, na.rm = TRUE), color="Média"), size = 1 ) + 
    geom_point( shape = 23, data=subset(AMCs40, Capital == TRUE), 
                aes(x=reorder(Estado20, AdmPub_PT, median,  na.rm = T), y=AdmPub_PT), position=position_dodge(1),
                color="black", size=4, show.legend = FALSE) +
    scale_color_manual(name = 'Estatísticas', values = c(Mediana = 'blue', Média = 'red')) + 
    scale_fill_discrete(name = 'Região') + 
    theme_minimal(base_size = 14) + 
    labs(title = "Distribuição de funcionários da administração pública por Estado em 1940",
         subtitle = 'Pontos representam número de funcionários da administração pública por mil habitantes por AMC.  \n Ordenado por médias. Capitais são os losangos em destaque.',
         x = 'Estado', y = "Servidores da Segurança Pública por Mil Habitantes", 
         caption = '') +
    theme_boxplot_gabe()
  
  ggsave(filename = 'B40_AdmPub_UF.jpg', plot = B40_AdmPub_UF,
         width = 1920/144, height = 1080/144)
  
}

# Segurança Pública 1920 
{
  AMC20_Corte <- filter(AMCs20, SegPub_PT < 50)
  B20_SegPub_UF <- ggplot(AMC20_Corte, aes(x = reorder(Estado20, SegPub_PT, mean,  na.rm = T), # Clusters
                                           y = SegPub_PT, # Variable
                                           fill = Region_1)) + # Criteria for colors
    geom_boxplot() + 
    coord_cartesian(ylim = c(0,35)) +
    geom_hline(aes(yintercept= median(SegPub_PT), color="Mediana"), size = 1 ) +
    geom_hline(aes(yintercept= mean(SegPub_PT), color="Média"), size = 1 ) + 
    geom_point( shape = 23, data=subset(AMCs20, Capital == TRUE), 
                aes(x=reorder(Estado20, SegPub_PT, median,  na.rm = T), y= SegPub_PT), position=position_dodge(1),
                color="black", size=4, show.legend = FALSE) +
    scale_color_manual(name = 'Estatísticas', values = c(Mediana = 'blue', Média = 'red')) + 
    scale_fill_discrete(name = 'Região') + 
    theme_minimal(base_size = 14) + 
    labs(title = "Distribuição de servidores da segurança pública por Estado em 1920" ,
         subtitle = 'Pontos representam número de servidores da segurança pública por mil habitantes por AMC.  \n Ordenado por médias. Capitais são os losangos em destaque.',
       x = 'Estado', y = "Servidores da Segurança Pública",
       caption = '') +
    theme_boxplot_gabe()
  
  ggsave(filename = 'B20_SegPub_UF.jpg', plot = B20_SegPub_UF,
         width = 1920/144, height = 1080/144)
  
}


# Segurança Pública 1940 
{
  AMC40_Corte <- filter(AMCs40, SegPub_PT < 50)
  B40_SegPub_UF <- ggplot(AMC40_Corte, aes(x = reorder(Estado20, SegPub_PT, mean,  na.rm = T), y = SegPub_PT, fill = Region_1)) + 
    geom_boxplot() + 
    coord_cartesian(ylim = c(0,30)) +
    geom_hline(aes(yintercept= median(SegPub_PT), color="Mediana"), size = 1 ) +
    geom_hline(aes(yintercept= mean(SegPub_PT), color="Média"), size = 1 ) + 
    geom_point( shape = 23, data=subset(AMCs40, Capital == TRUE), 
                aes(x=reorder(Estado20, SegPub_PT, median,  na.rm = T), y=SegPub_PT), position=position_dodge(1),
                color="black", size=4, show.legend = FALSE) +
    scale_color_manual(name = 'Estatísticas', values = c(Mediana = 'blue', Média = 'red')) + 
    scale_fill_discrete(name = 'Região') + 
    # scale_y_continuous(breaks = seq(0,30, by = 2.5)) +
    theme_minimal(base_size = 14) + 
    labs(title = "Distribuição de servidores da segurança pública por Estado em 1940" ,
         subtitle = 'Pontos representam número de servidores da segurança pública por mil habitantes por AMC.  \n Ordenado por médias. Capitais são os losangos em destaque.',
         x = 'Estado', y = "Servidores da Segurança Pública",
         caption = '') +
    theme_boxplot_gabe()
  
  ggsave(filename = 'B40_SegPub_UF.jpg', plot = B40_SegPub_UF,
         width = 1920/144, height = 1080/144)
}


# Dumbbell Plot UF --------------------------------------------------------

## DP_AdmPub_UF
## DP_SegPub_UF

#--- based on https://r-graph-gallery.com/web-extended-dumbbell-plot-ggplot2.html

Censo20_Dumbbell <- filter(Censo20_UF, !is.na(Censo20_UF$AdmPub_PT))
Censo40_Dumbbell <- filter(Censo40_UF, !is.na(Censo40_UF$AdmPub_PT))

Censo_DDD <- rbind(Censo20_Dumbbell, Censo40_Dumbbell)
Censo_DDD$Ano <- as.factor(Censo_DDD$Ano)

Region_1_Sul_1 <- c('RS', 'SC', 'PR')
Region_1_Sudeste_2 <- c('SP', 'MG', 'RJ', 'ES')
Region_1_Nordeste_3 <- c('BA', 'SE', 'AL', 'PE', 'PB', 'RN', 'CE', 'PI', 'MA')

Censo_DDD$Region <- c()
for (i in 1:length(Censo_DDD$Estado20)){
  if (substr(Censo_DDD$Estado20[i],1,2) %in% Region_1_Sul_1){
    Censo_DDD$Region[i] <- 3
  } else if (substr(Censo_DDD$Estado20[i],1,2) %in% Region_1_Sudeste_2){
    Censo_DDD$Region[i] <- 2
  } else {
    Censo_DDD$Region[i] <- 1
  }
}
# Censo_DDD$Region <- as.factor(Censo_DDD$Region)

C20 <- Censo_DDD %>% 
  filter(Ano == 20)
C40 <- Censo_DDD %>% 
  filter(Ano == 40)

{ # First some Data Wrangling
  dif <- plyr::round_any(C40$AdmPub_PT - C20$AdmPub_PT, 0.1, f = floor)
  order <-  C20$AdmPub_PT + (dif/2)
  stats <- Censo_DDD %>%
    group_by(Ano) %>%
    summarise(mean = mean(AdmPub_PT),
              SE = sd(AdmPub_PT)) %>%
    mutate(meanpos = mean + 1 *SE, 
           meanneg = mean - 1 *SE)
  stats_20 <- stats %>%
    filter(Ano == 20)
  stats_40 <- stats %>%
    filter(Ano  == 40)
  diff <- Censo_DDD %>% 
    filter(Ano == 20) %>% 
    mutate(x_pos = C20$AdmPub_PT + (dif/2)) 
  
  #Dumbbell Plot
  DP_AdmPub_UF <- ggplot(Censo_DDD) + 
    # geom_rect(xmin = stats_20$meanneg, xmax = stats_20$meanpos,
    #           ymin = 0, ymax = 17, fill = "#762a83", alpha = .005)+
    geom_vline(xintercept = stats_20$mean, linetype = "solid", size = .5, alpha = .8, color = "#762a83") +
    # geom_rect(xmin = stats_40$meanneg, xmax = stats_40$meanpos,
    #           ymin = 0, ymax = 17, fill = "#009688", alpha = .005)+
    geom_vline(xintercept = stats_40$mean, color = "#009688", linetype = "solid",  size = .5, alpha = .8) +
    geom_segment(data = C20,
                 aes(x = AdmPub_PT, y = reorder(Estado20,Region),
                     yend = C40$Estado20, xend = C40$AdmPub_PT), 
                 color = "#aeb6bf",
                 size = 5.5, 
                 alpha = .5) +
    geom_point(aes(x = AdmPub_PT, y = Estado20, color = Ano), size = 6, show.legend = TRUE) +
    geom_text(data = diff,
              aes(label = paste("D: ",dif), x = (AdmPub_PT + (dif/2)), y = Estado20), 
              # fill = "white",
              color = "#4a4e4d",
              size = 3.5) +
    geom_text(x = (stats_40$mean + 0.13) , y = 13, label = "MÉDIA", angle = 90, size = 4, color = "#009688")+
    # geom_text(x = (stats_40$meanpos +0.13), y = 2, label = "DESVIO PADRÃO", angle = 90, size = 4, color = "#009688")+
    scale_color_manual(values = c("#762a83", "#009688")) +
    geom_hline(aes(yintercept=9.5), linetype = 'dashed') +
    geom_hline(aes(yintercept=13.5), linetype = 'dashed') +
    labs(title = 'Funcionários da administração pública por mil habitantes',
         subtitle = "Valores por Estado em <span style = 'color: #762a83;'>**1920**</span> e <span style = 'color: #009688;'>**1940**</span> <br>",
         caption = "",
         x = 'Funcionários da administração pública por mil habitantes',
         y = '') +
    theme_minimal(base_size = 15) +
    theme(plot.subtitle = element_markdown(size = 15)) +
    theme(legend.position="none") +
    theme(plot.title=element_text(face = "bold",  size=16, vjust=1.25, hjust = 0)) +
    theme(plot.caption = element_text(face = "plain", size = 13)) +
    theme(axis.text.y=element_text(size=12, angle=0,hjust=0.95,vjust=0.2))
  
  #Save Plot
  ggsave(filename = 'DP_AdmPub_UF.jpg', plot = DP_AdmPub_UF,
         width = 1920/144, height = 1080/144)
  }

{ # First some Data Wrangling
  dif <- plyr::round_any(C40$SegPub_PT - C20$SegPub_PT, 0.1, f = floor)
  order <-  C20$SegPub_PT + (dif/2)
  stats <- Censo_DDD %>%
    group_by(Ano) %>%
    summarise(mean = mean(SegPub_PT),
              SE = sd(SegPub_PT)) %>%
    mutate(meanpos = mean + 1 *SE, 
           meanneg = mean - 1 *SE)
  stats_20 <- stats %>%
    filter(Ano == 20)
  stats_40 <- stats %>%
    filter(Ano  == 40)
  diff <- Censo_DDD %>% 
    filter(Ano == 20) %>% 
    mutate(x_pos = C20$SegPub_PT + (dif/2)) 
  
  #Dumbbell Plot
  DP_SegPub_UF <- ggplot(Censo_DDD) + 
    # geom_rect(xmin = stats_20$meanneg, xmax = stats_20$meanpos,
    #           ymin = 0, ymax = 17, fill = "#762a83", alpha = .005)+
    geom_vline(xintercept = stats_20$mean, linetype = "solid", size = .5, alpha = .8, color = "#762a83") +
    # geom_rect(xmin = stats_40$meanneg, xmax = stats_40$meanpos,
    #           ymin = 0, ymax = 17, fill = "#009688", alpha = .005)+
    geom_vline(xintercept = stats_40$mean, color = "#009688", linetype = "solid",  size = .5, alpha = .8) +
    geom_segment(data = C20,
                 aes(x = SegPub_PT, y = reorder(Estado20,Region),
                     yend = C40$Estado20, xend = C40$SegPub_PT), 
                 color = "#aeb6bf",
                 size = 5.5, 
                 alpha = .5) +
    geom_point(aes(x = SegPub_PT, y = Estado20, color = Ano), size = 6, show.legend = TRUE) +
    geom_text(data = diff,
              aes(label = paste("D: ",dif), x = (SegPub_PT + (dif/2)), y = Estado20), 
              # fill = "white",
              color = "#4a4e4d",
              size = 3.5) +
    geom_text(x = (stats_40$mean + 0.13) , y = 2, label = "MÉDIA", angle = 90, size = 4, color = "#009688")+
    # geom_text(x = (stats_40$meanpos +0.13), y = 2, label = "DESVIO PADRÃO", angle = 90, size = 4, color = "#009688")+
    scale_color_manual(values = c("#762a83", "#009688")) +
    geom_hline(aes(yintercept=9.5), linetype = 'dashed') +
    geom_hline(aes(yintercept=13.5), linetype = 'dashed') +
    labs(title = 'Servidores da segurança pública por mil habitantes',
         subtitle = "Valores por Estado em <span style = 'color: #762a83;'>**1920**</span> e <span style = 'color: #009688;'>**1940**</span> <br>",
         caption = "",
         x = 'Servidores da segurança pública por mil habitantes',
         y = '') +
    theme_minimal(base_size = 15) +
    theme(plot.subtitle = element_markdown(size = 15)) +
    theme(legend.position="none") +
    theme(plot.title=element_text(face = "bold",  size=16, vjust=1.25, hjust = 0)) +
    theme(plot.caption = element_text(face = "plain", size = 13)) +
    theme(axis.text.y=element_text(size=12, angle=0,hjust=0.95,vjust=0.2))
  
  #Save Plot
  ggsave(filename = 'DP_SegPub_UF.jpg', plot = DP_SegPub_UF,
         width = 1920/144, height = 1080/144)
}


# Variance Analysis -------------------------------------------------------
VarUF20 <- AMCs20 %>% 
  group_by(Estado20) %>% 
  summarise(SD20_AdmPub_PT = sd(AdmPub_PT, na.rm = T),
            SD20_SegPub_PT = sd(SegPub_PT, na.rm = T))
VarUF40 <- AMCs40 %>% 
  group_by(Estado20) %>% 
  summarise(SD40_AdmPub_PT = sd(AdmPub_PT, na.rm = T),
            SD40_SegPub_PT = sd(SegPub_PT, na.rm = T))
VarUF <- left_join(VarUF20, VarUF40)

## não há padrão claro para admpub, mas sim para segpub
## No sul e sudeste há uma variância claramente maior 

Region_1_Sul_1 <- c('RS', 'SC', 'PR')
Region_1_Sudeste_2 <- c('SP', 'MG', 'RJ', 'ES')
Region_1_Nordeste_3 <- c('BA', 'SE', 'AL', 'PE', 'PB', 'RN', 'CE', 'PI', 'MA')

for (i in 1:length(VarUF$Estado20)){
  if (substr(VarUF$Estado20[i],1,2) %in% Region_1_Sul_1){
    VarUF$Region[i] <- 'Sul'
  } else if (substr(VarUF$Estado20[i],1,2) %in% Region_1_Sudeste_2){
    VarUF$Region[i] <- 'Sudeste'
  } else {
    VarUF$Region[i] <- 'Nordeste'
  }
}

## Vamos ver em média o quanto o desvio padrão é maior
stargazer(lm(VarUF$SD20_SegPub_PT ~ VarUF$Region), type = 'text')
stargazer(lm(VarUF$SD40_SegPub_PT ~ VarUF$Region), type = 'text')


# Border Analysis ---------------------------------------------------------

# Importar AMCs das unidades fronteiriças

Borders_AMC <- read_excel('Border_AMC.xlsx')
Borders_AMC$AMC <- as.character(Borders_AMC$AMC)

AMCs20_Border <- AMCs20 %>% 
  filter(AMC %in% Borders_AMC$AMC)
AMCs20_Border$AMC <- as.numeric(AMCs20_Border$AMC)

AMCgeo <- geobr::read_comparable_areas(1920, 1940) 
colnames(AMCgeo)[c(1,2)] <- c('AMC', 'codMun')

{
Mapa <- right_join(AMCs20_Border, AMCgeo, by = 'AMC')
Mapa <- cbind(Mapa, st_coordinates(st_centroid(Mapa$geom)))

Plot_Borders_AMC <- ggplot() +
  geom_sf(data = Estados20, fill = "gray90", size = 1, color = 'gray15') +
  geom_sf(data = Mapa$geom, aes(fill = unlist(Mapa['Estado']))) + 
  geom_sf(data = Estados20, fill = NA, size = 1, color = 'gray15') +
  labs(title = 'AMCs de fronteira', 
       subtitle = 'Gráfico para visualização da subamostra de AMCs de fronteira', 
       caption = '') +
  annotation_scale(location = "bl", width_hint = 0.3) + theme_map_gabe() +
  theme(legend.position = 'none')
}
Plot_Borders_AMC


## Regressões
R1_AdmPub_20 <- lm(data = AMCs20_Border, AdmPub_PT ~ PIB_Capita + Estado20)
R2_AdmPub_20 <- lm(data = AMCs20_Border, AdmPub_PT ~ PIB_Capita + Estado20 + City_Size)
R1_SegPub_20 <- lm(data = AMCs20_Border, SegPub_PT ~ PIB_Capita + Estado20)
R2_SegPub_20 <- lm(data = AMCs20_Border, SegPub_PT ~ PIB_Capita + Estado20 + City_Size)

AMCs40_Border <- AMCs40 %>% 
  filter(AMC %in% Borders_AMC$AMC)
AMCs40_Border$AMC <- as.numeric(AMCs40_Border$AMC)


R1_AdmPub_40 <- lm(data = AMCs40_Border, AdmPub_PT ~ PIB_Capita + Estado20)
R2_AdmPub_40 <- lm(data = AMCs40_Border, AdmPub_PT ~ PIB_Capita + Estado20 + City_Size)
R1_SegPub_40 <- lm(data = AMCs40_Border, SegPub_PT ~ PIB_Capita + Estado20)
R2_SegPub_40 <- lm(data = AMCs40_Border, SegPub_PT ~ PIB_Capita + Estado20 + City_Size)

# summary(R1_AdmPub_20)
# summary(R2_AdmPub_20)
# summary(R1_AdmPub_40)
# summary(R2_AdmPub_40)
# 
# summary(R1_SegPub_20)
# summary(R2_SegPub_20)
# summary(R1_SegPub_40)
# summary(R2_SegPub_40)

stargazer(R1_AdmPub_20, R2_AdmPub_20, R1_AdmPub_40, R2_AdmPub_40, type = 'text')
stargazer(R1_SegPub_20, R2_SegPub_20, R1_SegPub_40, R2_SegPub_40, type = 'text')

# Sem resultados interessantes -> Mudar dummy de referência



