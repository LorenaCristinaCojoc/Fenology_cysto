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
library(data.table)

#1: Data preparation --------
#1.1: Temporal series Catalunya -----------
#Mediterranea
cmed<-read.csv("./raw_data/temporal_series_C.mediterranea.txt",header=T,dec=".",sep="\t", check.names = F); 
str(cmed);

#Change names and complete missing values
cmed = cmed %>% select(-Comments) %>% 
  mutate(Subpopulation = "CE",
         Quadrat = as.character(Quadrat),
         Date = as.Date(Date, tryFormats = c("%m/%d/%Y")),
         Cauloid_size = as.numeric(Cauloid_size),
         Fertility = as.numeric(ifelse(is.na(Fertility), 0, Fertility)),
         Leaf_size = as.numeric(Leaf_size))%>%
  mutate(Year = as.numeric(format(Date,'%Y')),
         uniq_id = paste("CE_med", substr(Month, 1, 3), substr(format(Date,'%Y'), 3, 4), sep = "_")) #Create new useful columns

str(cmed)
#write.table(cmed, "C.med_clean_temporal_phenology.txt", row.names = T,  sep="\t")

#Crinita/elegans
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
                                            TRUE ~ paste("CE", substr(Month, 1, 3), substr(format(Date,'%Y'), 3, 4), sep = "_"))) #Create new useful columns

str(crin)
#Rename subpopulations
c(crin %>% filter(Population == "Port de la Selva") %>% distinct(Subpopulation))
c(crin %>% filter(Population == "Cala Estreta") %>% distinct(Subpopulation))

#Merge both dataframes
phen_all = bind_rows(cmed, crin); 
#write.table(phen_all, "Temporal_series_phenology.txt", row.names = T,  sep="\t")

#1.2: Regional 1-2 year Mediterranean monitoring -----------
#2: Calculate different indices related with phenology --------------
phen_all<-read.csv("Temporal_series_phenology.txt",header=T,dec=".",sep="\t", check.names = F); 
phen_all$Species = as.factor(phen_all$Species); #phen_all$Fertility = as.character(phen_all$Fertility)

#Data exploration
table(phen_all$Species) #Total counts
length(unique(phen_all$Date)) #Unique sampling days
uniq_yr <- phen_all %>% filter(!Species %in% c("Reclutes", "Cystoseira compressa", "Cystoseira spinosa")) %>% 
                        group_by(Species, Year) %>% summarise(count = n(), .groups = "drop_last") #Total obs per year
  
#Fertility
colnames(phen_all)
fertility_class = phen_all %>% filter(!Species %in% c("Reclutes", "Cystoseira compressa", "Cystoseira spinosa"),
                                Fertility <= 4) %>% 
                         group_by(uniq_id, Population, Species, Date, Month, Year, Fertility) %>% 
                         summarise(count = n(), Cauloid_mean = mean(Cauloid_size, na.rm = T), Cauloid_sd = sd(Cauloid_size, na.rm = T),
                                   Leaf_mean = mean(Leaf_size, na.rm = T), Leaf_sd = sd(Leaf_size, na.rm = T), .groups = "drop_last")

fertility_pop <- reshape2::dcast(fertility_class, uniq_id + Population + Species + Date + Month + Year ~ Fertility, 
                       value.var = "count", fun.aggregate = sum)

#Obtain several metrics per sampling event
for(i in 1:nrow(fertility_pop)){
  fertility_pop$prop_fertile[i] = (sum(fertility_pop[i,c(8:11)])/sum(fertility_pop[i,c(7:11)]))*100 #proportion of fertile individuals
  }
  
#Metrics per population, species and year
fertility_metrics_year = data.frame()
years = sort(unique(fertility_class$Year)); popul = c("Cala Estreta", "Port de la Selva"); sp = unique(as.character(fertility_pop$Species))

#Loop to compute metrics x year
for(i in 1:length(years)){ #START loop YEAR
  for(j in 1:2){#START loop Population
    for(s in 1:length(sp)){#START loop species
      print(paste(years[i], popul[j], sp[s]))
      
      temp_class = fertility_class %>% filter(Year == years[i], Population == popul[j], Species == sp[s]) #All fertility classes per each year in a single population
      temp_pop = fertility_pop %>% filter(Year == years[i], Population == popul[j], Species == sp[s]); 
      
      temp_pop = temp_pop[order(temp_pop$Date),] #Really important to order data by DATE for calculations
      
      if(nrow(temp_class) > 0){
      
      n_months_fertile = length(which(temp_pop$prop_fertile > 0)) #Number of fertile months
      obs_fertile = (n_months_fertile/nrow(temp_pop))*100 #Times observed fertile in total times sampled
      peak = temp_pop[which.max(temp_pop$prop_fertile), "Month"] #Month where most individuals are fertile
      first_maturity = ifelse(n_months_fertile == 0, #If it's not mature 
                              "None",
                              with(temp_pop[,], Month[which.min(prop_fertile <= 0)])) #Start of the fertile period

      last_maturity = ifelse(nrow(temp_pop %>% filter(prop_fertile == 0)) == 0, #If there is no month 
                             "None",
                             temp_pop[tail(which(temp_pop[order(temp_pop$Date),12]!=0, arr.ind = T),1), "Month"]) #Last month of the fertile period
      
      fert_classes = temp_pop[, c(7:11)]; fert_classes[fert_classes == 0] <- NA
      fert_classes = t(as.data.frame(colMeans(fert_classes, na.rm = T))) #Mean individuals per fertile class WHEN fertile
      total_ind = sum(temp_pop[, c(7:11)])
      
      #Join everything in a dataframe
      temp_fertility = data.frame(Population = popul[j], Species = sp[s], Year = years[i],
                                  fert_classes, total_ind = total_ind, n_months_fertile = n_months_fertile, obs_fertile = obs_fertile, 
                                  peak = peak, first_maturity = first_maturity, last_maturity = last_maturity)
      temp_fertility[is.na(temp_fertility)] <- 0
      
      } else {temp_fertility = data.frame(Population = popul[j], Species = sp[s], Year = years[i]) }
      
      fertility_metrics_year = bind_rows(fertility_metrics_year, temp_fertility)
      
    } #END loop species
    
  } #END loop Population
  
}#END loop YEAR

#Remove NAs
fertility_metrics_year = fertility_metrics_year %>% filter(!is.na(last_maturity)) %>% mutate(last_maturity = ifelse(last_maturity == "0", "None", last_maturity))
rownames(fertility_metrics_year) = NULL
  
#3: Plots of fertility time-series ---------
fert_barpl = fertility_pop[order(fertility_pop$Date),] %>% filter(Species %in% "Ericaria crinita")

p <- ggplot(fert_barpl, aes(y=prop_fertile, x=Date)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_x_discrete(breaks=seq(2008,2021,1), minor_breaks = seq(2008,2021,1))+
  theme_minimal() + theme(legend.position = "right", text = element_text(size = 15), axis.text.y = element_text(size = 15),
                          axis.text.x = element_text(size = 15)); p

  
  
  
  
  
  
  



