#############################################
########## REPRODUCTIVE PHENOLOGY TIME SERIES
#############################################
library(dplyr)
library(purrr)
library(ggplot2)
library(ggpubr)
library(car)
library(viridisLite)
library(viridis)
library(tidyverse)
library(readxl)

cmed<-read.csv("./raw_data/temporal_series_C.mediterranea.txt",header=T,dec=".",sep="\t", check.names = F); 

#1: Data preparation --------
#1.1: Mediterranea
cmed <- cmed %>%  
  mutate(Leave_size = as.numeric(gsub(",", ".", Leave_size)), Quadrat = as.factor(as.character(Quadrat)),
         Cauloide_size = as.numeric(gsub(",", ".", Cauloide_size))) %>% group_by(Population, Species, Date, Quadrat) %>% 
  summarise(caul_size = mean(Cauloide_size, na.rm = T), leaves_size = mean(Leave_size, na.rm = T), .groups = "drop_last")

#write.table(cmed, "C.med_clean_temporal_phenology.txt", row.names = T,  sep="\t")

#1.2: Crinita/elegans
crin = read.csv("./raw_data/temporal_series_E.crinita.txt",header=T,dec=".",sep="\t", check.names = F); 
str(crin); colnames(crin)[c(7:9)] = c("Cauloid_size", "Fertility", "Leaf_size")

#Change names and complete missing values
crin = crin %>% select(-Comments) %>% 
                mutate(Population = case_when(Population %in% "PdS" ~ "Port de la Selva", #Correct current columns
                                              TRUE ~ "Cala Estreta"),
                       Species = case_when(Species %in% c("Cystoseira crinita", "Cystoseira_crinita") ~ "Ericaria crinita",
                                           Species %in% c("Cystoseira sp. ", "Cystoseira sp.", "Cystoseira sp", "Cystoseira_sp") ~ "Cystoseira sp",
                                           Species %in% c("Cystoseira elegans", "Cystoseira_elegans") ~ "Gongolaria elegans",
                                           Species %in% c("Cystoseira compressa", "Cystoseira_compressa") ~ "Cystoseira compressa",
                                           Species %in% c("Cystoseira_espinosa", "Cystoseira_spinosa") ~ "Cystoseira spinosa",
                                           Species %in% c("reclutes", "recluta") ~ "Reclutes",
                                           TRUE ~ Species),
                       Date = as.Date(Date, tryFormats = c("%m/%d/%Y")),
                       Cauloid_size = as.numeric(Cauloid_size),
                       Fertility = as.numeric(ifelse(is.na(Fertility), 0, Fertility)),
                       Leaf_size = as.numeric(Leaf_size) )%>%
                 mutate(Year = as.numeric(format(Date,'%Y')),
                        uniq_id = case_when(Population %in% "Port de la Selva" ~ paste("PS", substr(Month, 1, 3), substr(format(Date,'%Y'), 1, 2), sep = "_"), 
                                            TRUE ~ paste("CE", substr(Month, 1, 3), substr(format(Date,'%Y'), 1, 2), sep = "_"))) #Create new useful columns

#Rename subpopulations
crin %>% filter(Population == "Port de la Selva") %>% distinct(Subpopulation)
crin %>% filter(Population == "Cala Estreta") %>% distinct(Subpopulation)

#Compute some metrics
crin <- crin %>% group_by(uniq_id, Population, Species, Date, Quadrat) %>% 
  summarise(caul_size = mean(Cauloide_size, na.rm = T), leaves_size = mean(Leave_size, na.rm = T), .groups = "drop_last")

#write.table(cmed, "C.med_clean_temporal_phenology.txt", row.names = T,  sep="\t")

#Data exploration: calculate different indices representing phenology --------
data<-read.csv("C.med_clean_temporal_phenology.txt",header=T,dec=".",sep="\t", check.names = F)




