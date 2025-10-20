########################################################
#### DATA ANALYSIS OF CYSTOSEIRA PHENOLOGY TIME SERIES # 
########################################################
remove(list=ls())

#Libraries and functions
listoflibrary<-c("purrr", "ggplot2", "ggpubr", "car", "viridisLite", "viridis", "data.table", "mgcv", "car",
                 "mgcv", "tidyverse", "Rmisc",  "doParallel", "foreach", "parallel", "lme4", "multcomp",
                 "emmeans", "mgcViz", "tseries", "psych")

for (pkg in listoflibrary){
  if(!eval(bquote(require(.(pkg))))) {eval(bquote(install.packages(.(pkg))))}
  eval(bquote(library(.(pkg))))
}

#Set the number of cores to do functions in parallel
# numcores <- detectCores()
# registerDoParallel(numcores)

# Load environmental and time-series data
env = read.csv("env_data_serie_temporal.txt",header=T,dec=".",sep="\t", check.names = F); #env = env %>% filter(!is.na(sst))
env = env %>% mutate(Month = month.name[month_extract]) %>% dplyr::select(-stat_year, -year_day, -year_month) 
env_reduced = env %>% filter(!is.na(Date))

#merge fertility with environmental data
fertility_metrics = readRDS("Fertility_metrics.RData")
fertility_pop = fertility_metrics$Population; 

fertility_pop = merge(fertility_pop, env, by = c("Population", "Date", "Year"), all = T); 
fertility = fertility_pop %>% mutate(Month = coalesce(Month.x, Month.y)) %>% dplyr::select(-Month.x, -Month.y)

#fertility = fertility %>% filter(!is.na(Date))

#See how many NAs and which months have missing data
fertility$year_month = paste(fertility$Year, fertility$Month); fertility$sp_pop = paste(fertility$Species, fertility$Population)
nas = fertility %>% dplyr::select(Year, Month, sp_pop, prop_peak) %>% arrange(Year, match(Month, month.name)) %>% 
                      filter(sp_pop %in% c("NA Cala Estreta", "NA Port de la Selva"))
nas = table(nas$Year, nas$Month, nas$sp_pop)
na_CalaE = as.data.frame.matrix(nas[,,1]) %>% dplyr::select(month.name); na_CalaE[na_CalaE == 1] = NA
na_PdS = as.data.frame.matrix(nas[,,2]) %>% dplyr::select(month.name); na_PdS[na_PdS == 1] = NA


#Data exploration
#Multiple correlations among environmental variables
variables <- fertility %>% dplyr::select(sst, bot_t, nh4, no3, po4, photoperiod); colnames(variables)
cor(variables)
pairs.panels(variables, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

#Variance inflation factor
library(car)
variables <- fertility %>% dplyr::select(prop_peak,colnames(variables))
vif <- lm(prop_peak ~ ., variables %>% dplyr::select(-bot_t), na.action = na.omit); #summary (vif)

vif_values <- vif(vif)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)
sort(vif_values)

#Cross-correlation analysis
time_s = fertility %>% filter(Species == "Ericaria crinita", Population == "Cala Estreta", !Year == 2019)
cross_c = stats::ccf(time_s$photoperiod, time_s$prop_peak, lag.max = 6, type = "covariance", na.action = na.pass)



