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

#create a new, clean big dataframe to work with
data <- cmed %>%  
  mutate(Leave_size = as.numeric(gsub(",", ".", Leave_size)), Quadrat = as.factor(as.character(Quadrat)),
         Cauloide_size = as.numeric(gsub(",", ".", Cauloide_size))) %>% group_by(Population, Species, Date, Quadrat) %>% 
  

#Select specific species and year
data = data %>% filter(Year == 2021)




