## Gabriel Gral
## June 2022
## Script 01

## 1920 and 1940 Census Manipulation

## Keeping all databases in the same directory makes importing easier
getwd() #- This is the directory in which the bases are
list.files() #- These are the available files and databases

## Datasets used in this script: 
## Make sure these specific ones are in your working directory
    
    # dados_censo1940.xlsx
    # Censo PEAdes 20.xls
    # PIB.1920-1939.xls
    # Elections1930UF.xlsx
    # AMC1920_40.xlsx

# Libraries ---------------------------------------------------------------

#- Ctrl+Shit+F10 restarts R section and unloads packages
#- When {plyr} commands are necessary, use plyr:: so that conflicts with dplyr are avoided

library(readxl)
library(tidyverse)
library(dplyr)

library(expss) #- Apply labels to dataframes
library(ipeadatar) #- IPEA data info
library(sf) #- Maps formating
library(tmap) #- Maps formating | Interactive maps

rm(list=ls())

library(extrafont) 
library(tidyverse)

# link www.fontsquirrel.com/fonts/latin-modern-roman

# execute once to add fonts:
font_import(pattern = "LM*")
font_install('fontcm')

# 1940 Census -------------------------------------------------------------

Censo40 <- read_excel("dados_censo1940.xlsx", sheet = "Profissões (completo)", range = "D3:J16862",
                      col_names = c("Estado", "Mun", "codMun", "Categoria", "Total", "Homens", "Mulheres"))

## Let's transform Censo40 into a more convenient data frame
## Many professional categories have more than one name, to verify it:
Censo40_1 <- with(Censo40, tapply(Total, list(codMun, Categoria), I))
Cat40_1 <- colnames(Censo40_1)

## Let's fix the name mismatch problem manually
Censo40["Categoria"][Censo40["Categoria"]==Cat40_1[1]] <- Cat40_1[2]
Censo40["Categoria"][Censo40["Categoria"]==Cat40_1[4]] <- Cat40_1[3]
Censo40["Categoria"][Censo40["Categoria"]==Cat40_1[5]] <- Cat40_1[6]
Censo40["Categoria"][Censo40["Categoria"]==Cat40_1[7]] <- Cat40_1[8]
Censo40["Categoria"][Censo40["Categoria"]==Cat40_1[9]] <- Cat40_1[10]
Censo40["Categoria"][Censo40["Categoria"]==Cat40_1[11]] <- Cat40_1[12]
Censo40["Categoria"][Censo40["Categoria"]==Cat40_1[13]] <- Cat40_1[14]
Censo40["Categoria"][Censo40["Categoria"]==Cat40_1[15]] <- Cat40_1[16]
Censo40["Categoria"][Censo40["Categoria"]==Cat40_1[17]] <- Cat40_1[18]
Censo40["Categoria"][Censo40["Categoria"]==Cat40_1[19]] <- Cat40_1[20]
Censo40["Categoria"][Censo40["Categoria"]==Cat40_1[21]] <- Cat40_1[22]
Censo40["Categoria"][Censo40["Categoria"]==Cat40_1[24]] <- Cat40_1[23]

Censo40["Categoria"][Censo40["Categoria"]== "Transportes e  Comunicações"] <- c("Transportes e Comunicações" )

rm(Censo40_1)

Cat <- unique(Censo40$Categoria) #Verify that all the categories names are unique
Cat
Gen = c('Total', 'Homens', 'Mulheres')

# The following loop transforms our original base into a format with... 
## ...cities in the rows and professional categories in the columns. 
## 3 separate data frames will be created: one for men, one for women and one for the total population.

for (i in 1:length(Gen)){
  tp_List <- list()
  tp_ListNames <- c()
  for (j in 1:length(Cat)){
    tp_Values <- Censo40 %>%
      filter(Categoria == Cat[j]) %>%
      select(Estado, Mun, codMun, Gen[i]) ## Gets only the entries for a specific category and gender
    names(tp_Values)[names(tp_Values) == Gen[i]] <- Cat[j] ## Fix column names
    tp_Name <- paste0('tp_Censo40_', substr(Gen[i],1,1),'_', as.character(j))
    assign(tp_Name, tp_Values)
    ## For each category, a data frame is created, and is further appended to our tp_List()
    tp_List[[length(tp_List)+1]] <- get(tp_Name) ## Append data frame to list
    tp_ListNames <- append(tp_ListNames, tp_Name) 
    names(tp_List) <- tp_ListNames ##Fix list names
  }
  ## Now we want to merge all categories into a single data frame
  #- We are combining Reduce(){base} and left_join(){dplyr} for this purpose
  tp_CensoFull <- tp_List %>% 
    Reduce(function(a,b) left_join(a,b,by = c("Estado", "Mun", "codMun")),.)
  tp_CensoFull$codMun <- substr(tp_CensoFull$codMun, 1, 6) ## Get only the first 6 numbers of codMun
  tp_CensoFull <- cbind(tp_CensoFull[,c(3,1,2)], tp_CensoFull[,-c(1,2,3)]) ## Set order of columns equal to that of 1920
  tp_CensoFull$Pop <- rowSums(tp_CensoFull[,-c(1,2,3)], na.rm = T) ## Create variable of total population
  assign(paste0('Censo40_', substr(Gen[i],1,1)), tp_CensoFull)
  rm(list=ls(pattern="^tp_")) ## Finally, remove all temporary data frames and values
}

## Let's save our new data in our working directory
## RDS is a convenient format
saveRDS(Censo40_T, file = paste0(getwd(), '/Censo40_T'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)
saveRDS(Censo40_H, file = paste0(getwd(), '/Censo40_H'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)
saveRDS(Censo40_M, file = paste0(getwd(), '/Censo40_M'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)

## Alternatively, we can use csv format, but we must take care with encoding
# write_csv(Censo40_T, file = paste0(getwd(), '/Censo40_T'))
# write_csv(Censo40_H, file = paste0(getwd(), '/Censo40_H'))
# write_csv(Censo40_M, file = paste0(getwd(), '/Censo40_M'))

rm(Censo40)
rm(Cat, Cat40_1, Gen, i, j)


# Total Population --------------------------------------------------------

## So far the total population has been calculated based on the sum of employed people, and thus does not consider unemployed or young people 
## I will fix this problem, but only for the whole population, and not separately for men and women
## This is an issue that can be later improved

PopTotal <- read_excel('PopTotal40.xlsx')
colnames(PopTotal)[2] <- 'codMun'
PopTotal$codMun <- as.character(substr(as.character(PopTotal$codMun),1,6)) %>% 
  replace_na(replace = '0')
Censo40_T <- left_join(Censo40_T, PopTotal[,c(2,4)], by = 'codMun') 
Censo40_T$PopTotal <- Censo40_T$PopTotal %>% replace_na(0)
Censo40_T[is.na(Censo40_T$'PopTotal.y')] <- 0
Censo40_T <- subset(Censo40_T, select = -c(Pop))

saveRDS(Censo40_T, file = paste0(getwd(), '/Censo40_T'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)


# 1920 Census -------------------------------------------------------------

Prof20 <- read_excel("Censo PEAdes 20.xls", range = "BC5:CX5")

## Some columns were imported with a wrong name, let's correct them mannualy
colnames(Prof20)[startsWith(colnames(Prof20), "...")] <- c(
  "Maritimos e Fluxiais", "Terrestres e aereos", "Correos, Telegrafos e telefones", "Bancos, cambio, seguro, comissões, etc.",
  "Comercio propriamente dito", "Outras especies de comercio", "Particular",
  "Religiosas", "Judiciarias", "Médicas", "Magistério","Ciências, letras e Artes", 
  "Pessoas que vivem de sua renda", "Serviço domestico", "Mal Definido",
  "0 a 14 anos", "15 a 20 anos", "21 e mais anos")

## Specifying military types
colnames(Prof20)[c(26,27,28,29,30,31,32,33)] <- c("Oficiais (Ex)", "Praças (Ex)",
                                                  "Oficiais (Ar)", "Praças (Ar)", 
                                                  "Oficiais (Pol)", "Praças (Pol)",
                                                  "Oficiais (Bom)", "Praças (Bom)")
Local20 <- read_excel("Censo PEAdes 20.xls", range = "A7:C1382")

Value20_M <- read_excel("Censo PEAdes 20.xls", range = "BC7:CX1382")
Value20_H <- read_excel("Censo PEAdes 20.xls", range = "F7:BA1382")

Gen <- c('M', 'H')

## We have got to remove the rows which aggregate state data, and those used to separate data from one state of the other

for(i in 1:length(Gen)) {
  Value20 <- get(paste0('Value20_',Gen[i]))
  Value20 <- Value20[-which(Local20[,3]=="Estado"),]
  assign(paste0('Value20_',Gen[i]),Value20)
  rm(Value20)
}

Local20 <- Local20[-which(Local20[,3]=="Estado"),] 

for(i in 1:length(Gen)) {
  Value20 <- get(paste0('Value20_',Gen[i]))
  Value20 <- Value20[-which(is.na(Local20[,2])),]
  assign(paste0('Value20_',Gen[i]),Value20)
  rm(Value20)
}

Local20 <- Local20[-which(is.na(Local20[,2])),] 

## Rename the columns and get only the first six numbers of city code
colnames(Local20) <- c("codMun", "Estado","Mun")
Local20$codMun <-as.numeric(substr(as.character(Local20$codMun),1,6))

## Bind Values and Locals
for(i in 1:length(Gen)){
  Value20 <- get(paste0('Value20_',Gen[i]))
  DataFrame <- cbind(Local20,Value20)
  colnames(DataFrame)[-c(1,2,3)] <- colnames(Prof20)
  DataFrame[is.na(DataFrame)] <- 0
  assign(paste0('Censo20_',Gen[i]),DataFrame)
  rm(DataFrame)
}

## Create data frame for both men and women 
Value20_T <- Censo20_H[,-c(1,2,3)] + Censo20_M[,-c(1,2,3)]
Censo20_T <- cbind(Local20, Value20_T)

## Create variable of total population
Gen <- c('M', 'H', 'T')

for (i in 1:length(Gen)){
  tp_Censo <- get(paste0('Censo20_', Gen[i]))
  tp_Censo$PopTotal <- rowSums(tp_Censo[,-c(1,2,3)], na.rm = T)
  assign(paste0('Censo20_', Gen[i]), tp_Censo)
  rm(list=ls(pattern = "^tp_"))
}

## Let's save our new data in our working directory
## RDS is a convenient format
saveRDS(Censo20_T, file = paste0(getwd(), '/Censo20_T'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)
saveRDS(Censo20_H, file = paste0(getwd(), '/Censo20_H'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)
saveRDS(Censo20_M, file = paste0(getwd(), '/Censo20_M'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)

## Alternatively, we can use csv format, but we must take care with encoding
# write_csv(Censo20_T, file = paste0(getwd(), '/Censo20_T'))
# write_csv(Censo20_H, file = paste0(getwd(), '/Censo20_H'))
# write_csv(Censo20_M, file = paste0(getwd(), '/Censo20_M'))

rm(Local20, Prof20, Value20, Value20_T, Value20_H, Value20_M)
rm(Gen, i)

# GDP Data ----------------------------------------------------------------
## Import GDP data for cities

PIB_1920_1939 <- read_excel("PIB.1920-1939.xls")
colnames(PIB_1920_1939) <- c('PIB', 'Mun', 'PIB_1920', 'PIB_1939')
PIB_1920_1939 <- PIB_1920_1939[,-1]

## As municipalities names are different in the Census and in the IPEA database, we must join bases by 'codMun'

## Let's get municipalities codes from {ipeadatar}

available_territories() -> Territories
Muni <- filter(Territories, uname == 'Municipality') #- Select only those which refer to municipalities
Muni <- as.data.frame(Muni[,c(2,3)])
Muni$codMun <- substr(Muni$tcode, 1, 6) #- Get only the first 6 characters of codMun
Muni <- Muni[,-1] #- Get rid of the 7 digit codMun
colnames(Muni) <- c('Mun', 'codMun')

## We finally have our GDP Data

PIB_20_39 <- left_join(PIB_1920_1939, Muni, by = 'Mun')
# PIB_20_39$codMun <- as.numeric(PIB_20_39$codMun)
colnames(PIB_20_39) <- c('MunIPEA', 'PIB_1920', 'PIB_1939', 'codMun')

rm(Muni)
rm(Territories)
rm(PIB_1920_1939)

# Elections Data ----------------------------------------------------------

## Elections data come from https://atlas.fgv.br/

Elec_30_UF <- read_excel('Elections1930UF.xlsx')
Elec_30_UF <- Elec_30_UF[,-2]

# Variables of Interest ---------------------------------------------------

Censo20_T <- readRDS('Censo20_T')
Censo40_T <- readRDS('Censo40_T')

## GDP per Capita shall be calculated after Adjusting for AMCs

## I am creating my 'State Capacity' data frames (_SC) for 1920 and 1940

tp_AP <- Censo20_T %>% select(37:39, 42, 44) %>% rowSums(na.rm = T)
tp_SP <- Censo20_T %>% select(29:36) %>% rowSums(na.rm = T)
tp_APpt <- (tp_AP/Censo20_T$PopTotal)*1000
tp_SPpt <- (tp_SP/Censo20_T$PopTotal)*1000
Censo20_SC <- cbind(Censo20_T[,c(1:3)], tp_AP, tp_SP, tp_APpt, tp_SPpt, Censo20_T$PopTotal)
colnames(Censo20_SC)[c(4:8)] <- c('AdmPub', 'SegPub', 'AdmPub_PT', 'SegPub_PT', 'PopTotal')
rm(list=ls(pattern="^tp_"))
Censo20_SC$codMun <- as.character(Censo20_SC$codMun)
Censo20_SC <- left_join(Censo20_SC, PIB_20_39, by = 'codMun')
# Censo20_SC$PIB_Capita_20 <- Censo20_SC$PIB_1920/Censo20_SC$PopTotal20


tp_AP <- Censo40_T[10]
tp_SP <- Censo40_T[11]
tp_APpt <- (tp_AP/Censo40_T$PopTotal)*1000
tp_SPpt <- (tp_SP/Censo40_T$PopTotal)*1000
Censo40_SC <- cbind(Censo40_T[,c(1:3)], tp_AP, tp_SP, tp_APpt, tp_SPpt, Censo40_T$PopTotal)
colnames(Censo40_SC)[c(4:8)] <- c('AdmPub', 'SegPub', 'AdmPub_PT', 'SegPub_PT', 'PopTotal')
rm(list=ls(pattern="^tp_"))
Censo40_SC <- left_join(Censo40_SC, PIB_20_39, by = 'codMun')
# Censo40_SC$PIB_Capita_39 <- Censo40_SC$PIB_1939/Censo40_SC$PopTotal40

summary(Censo20_SC)
summary(Censo40_SC)

# Minimum Comparable Areas (AMC) ------------------------------------------

AMC <- read_excel('AMC1920_40.xlsx')
colnames(AMC)[2] <- 'codMun'
AMC$codMun <- as.numeric(substr(as.character(AMC$codMun),1,6))

for (i in c(20,40)){
  tp_Data <- get(paste0('Censo', i, '_SC'))
  tp_Data$codMun <- as.numeric(tp_Data$codMun)
  tp_Data <- left_join(tp_Data, AMC, by = 'codMun')
  tp_Data <- tp_Data %>% filter(is.na(amc) == F)
  tp_AMC_Lev <- levels(factor(tp_Data$amc))
  Estado <- c()
  codMun <- c()
  Mun <- c()
  
  for (j in 1:length(tp_AMC_Lev)){
    codMun <- rbind(codMun, paste(na.omit(tp_Data[tp_Data$amc == tp_AMC_Lev[j], 1]), sep = ',', collapse = ','))
    Mun <- rbind(Mun, paste(na.omit(tp_Data[tp_Data$amc == tp_AMC_Lev[j], 3]), sep = ',', collapse = ',' ))
  }
  
  for (k in 1:length(tp_AMC_Lev)){
    tp_NomesEstado <- na.omit(tp_Data[tp_Data$amc == tp_AMC_Lev[k], 2])
    tp_NoChange <- as.matrix(table(tp_NomesEstado))[1] == length(tp_NomesEstado)
    if (tp_NoChange == TRUE){
      Estado <- rbind(Estado, tp_NomesEstado[1])
    } else {
      Estado <- rbind(Estado, paste(tp_NomesEstado, collapse = ','))
    }
  }
  
  tp_AMCs<- as.data.frame(cbind(codMun,Estado,Mun,tp_AMC_Lev))
  colnames(tp_AMCs) <- c("codMun","Estado","Mun","AMC")
  tp_AMCs <- tp_AMCs %>% mutate(Reg = case_when(Estado == 'RJ' | Estado == 'SP'|
                                                  Estado == 'MG' | Estado == "ES" ~ 'Sudeste',
                                                Estado == 'RS' | Estado == 'SC'|
                                                  Estado == 'PR' ~ 'Sul',
                                                Estado == 'AC' | Estado == 'AM'|
                                                  Estado == 'PA' ~ 'Norte',
                                                Estado == 'GO' | Estado == 'MT'
                                                ~ 'Centro Oeste',
                                                TRUE ~ "Nordeste"))
  tp_AMC_SC <- tp_Data %>%
    select(AdmPub, SegPub, AdmPub_PT, SegPub_PT, amc, PIB_1920, PIB_1939, PopTotal) %>% 
    group_by(amc) %>% 
    summarise(AdmPub = sum(AdmPub),
              SegPub = sum(SegPub),
              # AdmPub_PT = sum(AdmPub_PT),
              # SegPub_PT = sum(SegPub_PT),
              PIB_1920 = sum(PIB_1920, na.rm = T),
              PIB_1939 = sum(PIB_1939, na.rm = T),
              PopTotal = sum(PopTotal, na.rm = T))
  tp_AMC_SC$AdmPub_PT = (tp_AMC_SC$AdmPub/tp_AMC_SC$PopTotal)*1000
  tp_AMC_SC$SegPub_PT = (tp_AMC_SC$SegPub/tp_AMC_SC$PopTotal)*1000
  tp_AMC_SC <- tp_AMC_SC[,-1]
  tp_AMCs <- as.data.frame(cbind(tp_AMCs, tp_AMC_SC, i))
  colnames(tp_AMCs)[ncol(tp_AMCs)] <- "Ano"
  tp_NComple <- nrow(tp_AMCs) 
  
  assign(paste0('CensoAMC',i, '_SC'), tp_Data)
  assign(paste0('AMCs',i), tp_AMCs)
  assign(paste0('NComple', i), tp_NComple)
  rm(list=ls(pattern="^tp_"))
}

AMCs2040 <- rbind(AMCs20, AMCs40)
Ocurrences <- as.data.frame(table(AMCs2040$AMC)) %>% 
  setNames(., c('AMC', 'Freq'))
AMCs2040 <- left_join(AMCs2040,Ocurrences, by = 'AMC')
Lost <- subset(AMCs2040, Freq == 1)[,- ncol(AMCs2040)]
AMCs2040 <- subset(AMCs2040, Freq == 2)[,-ncol(AMCs2040)]
DuasOcurr <- nrow(AMCs2040)
AMCs2040 <- left_join(AMCs2040, AMCs20[,c(2,4)], by = 'AMC')
colnames(AMCs2040)[c(2,ncol(AMCs2040))] <- c('Estado', 'Estado20')  

summary(AMCs2040$AdmPub)
summary(AMCs2040$AdmPub_PT)

## Calculate PIB Per Capita

AMCs2040$PIB_Capita <- ifelse(AMCs2040$Ano == 20, AMCs2040$PIB_1920/AMCs2040$PopTotal, AMCs2040$PIB_1939/AMCs2040$PopTotal)

## Replace 0's in GDP data with NA

AMCs2040$PIB_Capita[0==AMCs2040$PIB_Capita] <- NA
AMCs2040$PIB_1920[0==AMCs2040$PIB_1920] <- NA
AMCs2040$PIB_1939[0==AMCs2040$PIB_1939] <- NA

## Add Election Data

AMCs2040 <- left_join(AMCs2040, Elec_30_UF, by = 'Estado20')

## Add election winner in each state
AMCs2040$Win_Prestes <- ifelse(AMCs2040$`% Prestes_UF` > AMCs2040$`% Vargas_UF`, 1, 0)
AMCs2040$Win_Vargas <- ifelse(AMCs2040$`% Vargas_UF` > AMCs2040$`% Prestes_UF`, 1, 0)
AMCs2040$Winner <- ifelse(AMCs2040$Win_Prestes == 1, 'Prestes Venceu', 'Vargas Venceu' )

## Dummy for 1940 
AMCs2040$Year40 <- ifelse(AMCs2040$Ano == 40, 1, 0)

## Interaction variable between 1940 and Win_Prestes
AMCs2040$Year40_Prestes <- AMCs2040$Year40*AMCs2040$Win_Prestes



# Descriptive Statistics --------------------------------------------------


## Create Statistics
## Differences per AMC

D_AMC <- group_by(AMCs2040, AMC) %>% summarise(D_AdmPub = 100*(last(AdmPub) - first(AdmPub))/first(AdmPub),
                                               D_SegPub = 100*(last(SegPub) - first(SegPub))/first(SegPub),
                                               D_AdmPub_PT = 100*(last(AdmPub_PT) - first(AdmPub_PT))/first(AdmPub_PT),
                                               D_SegPub_PT = 100*(last(SegPub_PT) - first(SegPub_PT))/first(SegPub_PT),
                                               D_PIB_Capita = 100*(last(PIB_Capita) - first(PIB_Capita))/first(PIB_Capita),
                                               Diff_PIB_Capita =  last(PIB_Capita) - first(PIB_Capita),
                                               D_PopTotal = 100*(last(PopTotal) - first(PopTotal))/first(PopTotal),
                                               `% Prestes_UF` = first(`% Prestes_UF`),
                                               `% Vargas_UF` = first(`% Vargas_UF`),
                                               Win_Prestes = first(Win_Prestes),
                                               Win_Vargas = first(Win_Vargas))

D_AMC <- inner_join(D_AMC, AMCs2040[1:(nrow(AMCs2040)/2), c(1:4)], by = "AMC") %>% select(codMun, Estado, Mun, AMC, D_AdmPub, D_SegPub, D_AdmPub_PT, D_SegPub_PT,
                                                                                          D_PIB_Capita, Diff_PIB_Capita, D_PopTotal, `% Prestes_UF`, `% Vargas_UF`, Win_Prestes, Win_Vargas)

D_AMC <- expss::apply_labels(D_AMC, D_AdmPub = "Diferença percentual de funcionários públicos (federal, estadual e municipal)",
                             D_SegPub = "Diferença percentual de defesa e segurança pública (exército, polícia, bombeiros, aeronáutica)",
                             D_AdmPub_PT = "Diferença percentual de funcionários públicos (federal, estadual e municipal) por mil habitantes",
                             D_SegPub_PT = "Diferença percentual de defesa e segurança pública (exército, polícia, bombeiros, aeronáutica) por mil habitantes",
                             D_PIB_Capita = "Diferença percentual no PIB per Capita da AMC",
                             Diff_PIB_Capita = "Diferença nominal no PIB per Capita da AMC")

## Replace infinite values with NA

D_AMC[sapply(D_AMC, is.infinite)] <- NA

D_AMC$Winner <- ifelse(D_AMC$Win_Prestes == 1, 'Prestes Venceu', 'Vargas Venceu')



# Aggregated Data ---------------------------------------------------------

Censo2040_UF <- AMCs2040 %>%
  group_by(Estado20, Ano) %>% 
  summarise(AdmPub = sum(AdmPub, na.rm = T),
            SegPub = sum(SegPub, na.rm = T),
            PopTotal = sum(PopTotal, na.rm = T),
            PIB_1920 = sum(PIB_1920, na.rm = T),
            PIB_1939 = sum(PIB_1939, na.rm = T))
Censo2040_UF$AdmPub_PT = (Censo2040_UF$AdmPub/Censo2040_UF$PopTotal)*1000
Censo2040_UF$SegPub_PT = (Censo2040_UF$SegPub/Censo2040_UF$PopTotal)*1000
Censo2040_UF$PIB_Capita = ifelse(Censo2040_UF$Ano == 20, Censo2040_UF$PIB_1920/Censo2040_UF$PopTotal, Censo2040_UF$PIB_1939/Censo2040_UF$PopTotal)


D_UF <- Censo2040_UF %>% 
  group_by(Estado20) %>% 
  summarise(D_AdmPub = 100*(last(AdmPub) - first(AdmPub))/first(AdmPub),
            D_SegPub = 100*(last(SegPub) - first(SegPub))/first(SegPub),
            D_AdmPub_PT = 100*(last(AdmPub_PT) - first(AdmPub_PT))/first(AdmPub_PT),
            D_SegPub_PT = 100*(last(SegPub_PT) - first(SegPub_PT))/first(SegPub_PT),
            D_PIB_Capita = 100*(last(PIB_Capita) - first(PIB_Capita))/first(PIB_Capita),
            Diff_PIB_Capita =  last(PIB_Capita) - first(PIB_Capita),
            D_PopTotal = 100*(last(PopTotal) - first(PopTotal))/first(PopTotal))

D_UF <- apply_labels(D_UF, D_AdmPub = "Diferença percentual de funcionários públicos (federal, estadual e municipal)",
                     D_SegPub = "Diferença percentual de defesa e segurança pública (exército, polícia, bombeiros, aeronáutica)",
                     D_AdmPub_PT = "Diferença percentual de funcionários públicos (federal, estadual e municipal) por mil habitantes",
                     D_SegPub_PT = "Diferença percentual de defesa e segurança pública (exército, polícia, bombeiros, aeronáutica) por mil habitantes",
                     D_PIB_Capita = "Diferença percentual no PIB per Capita da AMC",
                     Diff_PIB_Capita = "Diferença nominal no PIB per Capita da AMC")

Estados <- c('Alagoas', 'Bahia', 'Ceará', 'Espírito Santo', 
             'Maranhão', 'Minas Gerais', 'Paraíba', 'Pernambuco', 'Piauí',
             'Paraná', 'Rio de Janeiro', 'Rio Grande do Norte', 'Rio Grande do Sul',
             'Santa Catarina', 'Sergipe', 'São Paulo')

D_UF$name_state <- Estados

Elec_30_UF <- read_excel('Elections1930UF.xlsx')

D_UF <- left_join(Elec_30_UF, D_UF)

Censo20_UF <- filter(Censo2040_UF, Ano == 20)
Censo20_UF$name_state <- Estados
Censo20_UF <- Elec_30_UF %>%
  left_join(Censo20_UF) %>% 
  select(-c(3:7))

Censo40_UF <- filter(Censo2040_UF, Ano == 40)
Censo40_UF$name_state <- Estados
Censo40_UF <- Elec_30_UF %>%
  left_join(Censo40_UF) %>% 
  select(-c(3:7))


# Brazil Regions ----------------------------------------------------------

## Divisão Atual IBGE 
Region_1_Sul_1 <- c('RS', 'SC', 'PR')
Region_1_Sudeste_2 <- c('SP', 'MG', 'RJ', 'ES')
Region_1_Nordeste_3 <- c('BA', 'SE', 'AL', 'PE', 'PB', 'RN', 'CE', 'PI', 'MA')

for (i in 1:length(AMCs2040$Estado)){
  if (substr(AMCs2040$Estado[i],1,2) %in% Region_1_Sul_1){
    AMCs2040$Region_1[i] <- 'Sul'
  } else if (substr(AMCs2040$Estado[i],1,2) %in% Region_1_Sudeste_2){
    AMCs2040$Region_1[i] <- 'Sudeste'
  } else {
    AMCs2040$Region_1[i] <- 'Nordeste'
  }
}

## Divisão 1942 - Baseada em geografia
Region_2_Sul_1 <- c('RS', 'SC', 'PR', 'SP')
Region_2_LesteMeridional_2 <- c('MG', 'RJ', 'ES')
Region_2_LesteSetentrional_3 <- c('BA', 'SE')
Region_2_NordesteOriental_4 <- c('AL', 'PE', 'PB', 'RN', 'CE')
Region_2_NordesteOcidental_5 <- c('PI', 'MA')

for (i in 1:length(AMCs2040$Estado)){
  if (substr(AMCs2040$Estado[i],1,2) %in% Region_2_Sul_1){
    AMCs2040$Region_2[i] <- 'Sul'
  } else if (substr(AMCs2040$Estado[i],1,2) %in% Region_2_LesteMeridional_2){
    AMCs2040$Region_2[i] <- 'Leste Meridional'
  } else if (substr(AMCs2040$Estado[i],1,2) %in% Region_2_LesteSetentrional_3){
    AMCs2040$Region_2[i] <- 'Leste Setentrional'
  } else if (substr(AMCs2040$Estado[i],1,2) %in% Region_2_NordesteOriental_4){
    AMCs2040$Region_2[i] <- 'Nordeste Oriental'
  } else {
    AMCs2040$Region_2[i] <- 'Nordeste Ocidental'
  }
}

# First Regional division (1969)

## As there are different observations for 1920 and 1940, when we aggregate GDP
## we shall find different levels of GDP for the same year, because we're aggregating different AMCs
## Therefore, we should not compare the GDP in level, but only in per capita terms


Censo2040_Reg1 <- AMCs2040 %>%
  group_by(Region_1, Ano) %>% 
  summarise(AdmPub = sum(AdmPub, na.rm = T),
            SegPub = sum(SegPub, na.rm = T),
            PopTotal = sum(PopTotal, na.rm = T),
            PIB_1920 = sum(PIB_1920, na.rm = T),
            PIB_1939 = sum(PIB_1939, na.rm = T))
Censo2040_Reg1$AdmPub_PT = (Censo2040_Reg1$AdmPub/Censo2040_Reg1$PopTotal)*1000
Censo2040_Reg1$SegPub_PT = (Censo2040_Reg1$SegPub/Censo2040_Reg1$PopTotal)*1000
Censo2040_Reg1$PIB_Capita = ifelse(Censo2040_Reg1$Ano == 20, Censo2040_Reg1$PIB_1920/Censo2040_Reg1$PopTotal, Censo2040_Reg1$PIB_1939/Censo2040_Reg1$PopTotal)
Censo2040_Reg1 <- Censo2040_Reg1 %>% select(- c(PIB_1920, PIB_1939))

# Second Regional division (1942)
Censo2040_Reg2 <- AMCs2040 %>%
  group_by(Region_2, Ano) %>% 
  summarise(AdmPub = sum(AdmPub, na.rm = T),
            SegPub = sum(SegPub, na.rm = T),
            PopTotal = sum(PopTotal, na.rm = T),
            PIB_1920 = sum(PIB_1920, na.rm = T),
            PIB_1939 = sum(PIB_1939, na.rm = T))
Censo2040_Reg2$AdmPub_PT = (Censo2040_Reg2$AdmPub/Censo2040_Reg2$PopTotal)*1000
Censo2040_Reg2$SegPub_PT = (Censo2040_Reg2$SegPub/Censo2040_Reg2$PopTotal)*1000
Censo2040_Reg2$PIB_Capita = ifelse(Censo2040_Reg2$Ano == 20, Censo2040_Reg2$PIB_1920/Censo2040_Reg2$PopTotal, Censo2040_Reg2$PIB_1939/Censo2040_Reg2$PopTotal)

D_Reg1 <- Censo2040_Reg1 %>% 
  group_by(Region_1) %>% 
  summarise(D_AdmPub = 100*(last(AdmPub) - first(AdmPub))/first(AdmPub),
            D_SegPub = 100*(last(SegPub) - first(SegPub))/first(SegPub),
            D_AdmPub_PT = 100*(last(AdmPub_PT) - first(AdmPub_PT))/first(AdmPub_PT),
            D_SegPub_PT = 100*(last(SegPub_PT) - first(SegPub_PT))/first(SegPub_PT),
            D_PIB_Capita = 100*(last(PIB_Capita) - first(PIB_Capita))/first(PIB_Capita),
            Diff_PIB_Capita =  last(PIB_Capita) - first(PIB_Capita),
            D_PopTotal = 100*(last(PopTotal) - first(PopTotal))/first(PopTotal))

D_Reg1 <- apply_labels(D_Reg1, D_AdmPub = "Diferença percentual de funcionários públicos (federal, estadual e municipal)",
                     D_SegPub = "Diferença percentual de defesa e segurança pública (exército, polícia, bombeiros, aeronáutica)",
                     D_AdmPub_PT = "Diferença percentual de funcionários públicos (federal, estadual e municipal) por mil habitantes",
                     D_SegPub_PT = "Diferença percentual de defesa e segurança pública (exército, polícia, bombeiros, aeronáutica) por mil habitantes",
                     D_PIB_Capita = "Diferença percentual no PIB per Capita da Região",
                     Diff_PIB_Capita = "Diferença nominal no PIB per Capita da Região")


D_Reg2 <- Censo2040_Reg2 %>% 
  group_by(Region_2) %>% 
  summarise(D_AdmPub = 100*(last(AdmPub) - first(AdmPub))/first(AdmPub),
            D_SegPub = 100*(last(SegPub) - first(SegPub))/first(SegPub),
            D_AdmPub_PT = 100*(last(AdmPub_PT) - first(AdmPub_PT))/first(AdmPub_PT),
            D_SegPub_PT = 100*(last(SegPub_PT) - first(SegPub_PT))/first(SegPub_PT),
            D_PIB_Capita = 100*(last(PIB_Capita) - first(PIB_Capita))/first(PIB_Capita),
            Diff_PIB_Capita =  last(PIB_Capita) - first(PIB_Capita),
            D_PopTotal = 100*(last(PopTotal) - first(PopTotal))/first(PopTotal))

D_Reg2 <- apply_labels(D_Reg2, D_AdmPub = "Diferença percentual de funcionários públicos (federal, estadual e municipal)",
                       D_SegPub = "Diferença percentual de defesa e segurança pública (exército, polícia, bombeiros, aeronáutica)",
                       D_AdmPub_PT = "Diferença percentual de funcionários públicos (federal, estadual e municipal) por mil habitantes",
                       D_SegPub_PT = "Diferença percentual de defesa e segurança pública (exército, polícia, bombeiros, aeronáutica) por mil habitantes",
                       D_PIB_Capita = "Diferença percentual no PIB per Capita da Região",
                       Diff_PIB_Capita = "Diferença nominal no PIB per Capita da Região")

## As we have different samples and methodologies, the measurement of differences in total population probably does not make sense
## We should get external sources for population growth


# City Size ---------------------------------------------------------------

## I shall analyze separately the distribution of populations for 1920 and 1940
AMCs20 <- filter(AMCs2040, AMCs2040$Ano == 20)
AMCs40 <- filter(AMCs2040, AMCs2040$Ano == 40)

quantile(AMCs20$PopTotal, probs = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
quantile(AMCs40$PopTotal, probs = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))

## There seems to be a concentration trend from 1920 to  1940
## For lower quantiles, the population is smaller, but for higher quantiles it is bigger

City_Size <- c(10000,30000)

for (i in 1:length(AMCs40$AMC)){
  if (AMCs40$PopTotal[i] < City_Size[1]){
    AMCs40$City_Size[i] <- 1
  } else if (AMCs40$PopTotal[i] >= City_Size[1] && AMCs40$PopTotal[i] < City_Size[2]){
    AMCs40$City_Size[i] <- 2
  } else {
    AMCs40$City_Size[i] <- 3
  }
}


AMCsCitySizes <- cbind.data.frame(AMCs40$AMC, AMCs40$City_Size)
colnames(AMCsCitySizes) <- c('AMC', 'City_Size')
AMCs2040 <- left_join(AMCsCitySizes, AMCs2040)

for (i in 1:length(AMCs2040)){
  if (AMCs2040$City_Size[i] == 1){
    AMCs2040$City_Small[i] <- 1
  } else {
    AMCs2040$City_Small[i] <- 0
  }
}

for (i in 1:length(AMCs2040)){
  if (AMCs2040$City_Size[i] == 2){
    AMCs2040$City_Medium[i] <- 1
  } else {
    AMCs2040$City_Medium[i] <- 0
  }
}


for (i in 1:length(AMCs2040)){
  if (AMCs2040$City_Size[i] == 3){
    AMCs2040$City_Large[i] <- 1
  } else {
    AMCs2040$City_Large[i] <- 0
  }
}

AMCs2040$City_Size <- as.factor(AMCs2040$City_Size)

## Interacted variables for DD analysis
  # AMCs2040$Year40_Prestes_Small <- AMCs2040$Year40_Prestes * AMCs2040$City_Small
  # AMCs2040$Year40_Prestes_Medium <- AMCs2040$Year40_Prestes * AMCs2040$City_Medium
  # AMCs2040$Year40_Prestes_Large <- AMCs2040$Year40_Prestes * AMCs2040$City_Large
  # 
  # AMCs2040$Year40_Small <- AMCs2040$Year40 * AMCs2040$City_Small
  # AMCs2040$Year40_Medium <- AMCs2040$Year40 * AMCs2040$City_Medium
  # AMCs2040$Year40_Large <- AMCs2040$Year40 * AMCs2040$City_Large


# Frontiers Vargas ---------------------------------------------------------

## These variables are creating to indicate the 3 different areas in Brazil in which there was regional variations in the 1930 elections result
## Regarding election variations, I will create a variable indicating the three diferent regions in which Vargas won

Region1 <- c('RS','SC')
Region2 <- c('MG', 'SP', 'RJ', 'ES', 'BA')
Region3 <- c('PB', 'PE', 'CE', 'RN')

for (i in 1:length(AMCs2040$Estado)){
  if (substr(AMCs2040$Estado[i],1,2) %in% Region1){
    AMCs2040$Frontier[i] <- 1
  } else if (substr(AMCs2040$Estado[i],1,2) %in% Region2){
    AMCs2040$Frontier[i] <- 2
  } else {
    AMCs2040$Frontier[i] <- 3 
  }
}

AMCs2040$Frontier <- as.factor(AMCs2040$Frontier)
AMCs2040 <- AMCs2040 %>% 
  relocate(City_Size, .before = City_Small)



# Capitals ----------------------------------------------------------------

Capitais <- c('São Luiz', 
              'Terezina', 
              'Fortaleza',
              'Natal', 
              'Paraíba', 
              'Recife',
              'Maceió',
              'Aracaju',
              'Salvador',
              'Belo Horizonte',
              'Vitória',
              'Niterói',
              'Rio de Janeiro', # Falta
              'Distrito Federal', # Falta
              'São Paulo', 
              'Curitiba', 
              'Florianópolis',
              'Porto Alegre')

AMCsCapitais <- data.frame()
for(i in 1:length(Capitais)){
  AMCsCapitais <- rbind.data.frame(AMCsCapitais, AMCs20[grep(Capitais[i], AMCs20$Mun), ])
}

CapitaisAMCs <- c("8010", # Maceió
                  "10096", # Salvador
                  "4050", # Fortaleza
                  "11191", # Vitória
                  "11026", # Belo Horizonte
                  "7044", # Recife
                  "14012", # Curitiba
                  "12024", # Niterói
                  "13183", # São Paulo
                  "14057", # Florianópolis
                  "15026", # Porto Alegre
                  "6011", # Paraíba
                  "4025", # Terezina
                  "5021", # Natal
                  "9003", # Aracaju)
                  "3034") # São Luiz
                  
# CapitaisAMCs <- filter(AMCs20, AMC %in% CapitaisAMCs)

AMCs2040$Capital <- ifelse(AMCs2040$AMC %in% CapitaisAMCs,yes = TRUE, no = FALSE)


# Frontiers Dataset -------------------------------------------------------


#- Create map as sf object

## This part of the code was to help me create an excel sheet with the AMC codes for the frontier areas 
  
  # AMCgeo <- geobr::read_comparable_areas(1920, 1940) 
  # colnames(AMCgeo)[c(1,2)] <- c('AMC', 'codMun')
  # D_AMC$AMC <- as.numeric(D_AMC$AMC)
  # Mapa <- right_join(D_AMC, AMCgeo, by = 'AMC')
  # Mapa <- cbind(Mapa, st_coordinates(st_centroid(Mapa$geom)))
  # Mapa_sf <- sf::st_as_sf(Mapa)
  # 
  # #- Tell tmap to read objects in an interactive way
  # tmap:: tmap_mode('view')
  # 
  # #- Create our map
  # Plot_Interactive <- tm_shape(Mapa_sf) + 
  #   tm_polygons(col = 'Estado', border.col = 'grey10') + 
  #   tm_markers(text = 'AMC') +
  #   tm_layout(title= 'Áreas Mínimas Comparáveis', title.position = c('right', 'top'))
  # 
  # Plot_Interactive

# Save Data ---------------------------------------------------------------
  
  ## Differences between 1940 and 1920 - AMC level
  saveRDS(D_AMC, file = paste0(getwd(), '/D_AMC'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)
  
  ## Differences between 1940 and 1920 - UF level
  saveRDS(D_UF, file = paste0(getwd(), '/D_UF'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)
  
  ## Differences between 1940 and 1920 - Region 1 Level
  saveRDS(D_Reg1, file = paste0(getwd(), '/D_Reg1'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)
  
  ## Differences between 1940 and 1920 - Region 2 Level
  saveRDS(D_Reg2, file = paste0(getwd(), '/D_Reg2'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)
  
  ## Census Data - AMC Level
  saveRDS(AMCs2040, file = paste0(getwd(), '/AMCs2040'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)
  AMCs20 <- filter(AMCs2040, Ano == 20)
  saveRDS(AMCs20, file = paste0(getwd(), '/AMCs20'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)
  AMCs40 <- filter(AMCs2040, Ano == 40)
  saveRDS(AMCs40, file = paste0(getwd(), '/AMCs40'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)
  
  ## Census Data - UF Level
  saveRDS(Censo2040_UF, file = paste0(getwd(), '/Censo2040_UF'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)
  
  ## Census Data - Region 1 Level
  saveRDS(Censo2040_Reg1, file = paste0(getwd(), '/Censo2040_Reg1'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)
  write.csv(Censo2040_Reg1, file = paste0(getwd(), '/Censo2040_Reg1.csv'))
  
  ## Census Data - Region 2 Level
  saveRDS(Censo2040_Reg2, file = paste0(getwd(), '/Censo2040_Reg2'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)
  
  ## Census Data - UF Level 1920
  saveRDS(Censo20_UF, file = paste0(getwd(), '/Censo20_UF'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)
  
  ## Census Data - UF Level 1940
  saveRDS(Censo40_UF, file = paste0(getwd(), '/Censo40_UF'), ascii = FALSE, version = NULL, compress = F, refhook = NULL)
  
