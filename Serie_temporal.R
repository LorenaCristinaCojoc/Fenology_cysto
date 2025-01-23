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
str(cmed);

#Data prep: create a new, clean big dataframe to work with
cmed <- cmed %>%  
  mutate(Leave_size = as.numeric(gsub(",", ".", Leave_size)), Quadrat = as.factor(as.character(Quadrat)),
         Cauloide_size = as.numeric(gsub(",", ".", Cauloide_size))) %>% group_by(Population, Species, Date, Quadrat) %>% 
  summarise(caul_size = mean(Cauloide_size, na.rm = T), leaves_size = mean(Leave_size, na.rm = T), .groups = "drop_last")

#write.table(cmed, "C.med_clean_temporal_phenology.txt", row.names = T,  sep="\t")

#Data exploration: calculate different indices representing phenology --------
data<-read.csv("C.med_clean_temporal_phenology.txt",header=T,dec=".",sep="\t", check.names = F)




