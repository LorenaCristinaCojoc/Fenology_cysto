########################################################
#### DATA ANALYSIS OF CYSTOSEIRA PHENOLOGY TIME SERIES # 
########################################################
remove(list=ls())

#Libraries and functions
listoflibrary<-c("purrr", "ggplot2", "ggpubr", "car", "viridisLite", "viridis", "data.table", "mgcv", "car",
                 "mgcv", "Rmisc",  "doParallel", "foreach", "parallel", "lme4", "multcomp", "forecast",
                 "emmeans", "mgcViz", "tseries", "psych", "tidyverse")

for (pkg in listoflibrary){
  if(!eval(bquote(require(.(pkg))))) {eval(bquote(install.packages(.(pkg))))}
  eval(bquote(library(.(pkg))))
}

#Set the number of cores to do functions in parallel
numcores <- detectCores()
registerDoParallel(numcores)

# 1: Time-series data exploration ----
# Load environmental and time-series data
env = read.csv("env_data_serie_temporal.txt",header=T,dec=".",sep="\t", check.names = F); #env = env %>% filter(!is.na(sst))
env = env %>% mutate(Month = month.name[month_extract]) %>% dplyr::select(-stat_year, -year_day, -year_month) 
env_reduced = env %>% filter(!is.na(Date))

#merge fertility with environmental data
fertility_metrics = readRDS("Fertility_metrics.RData")
fertility_pop = fertility_metrics$Population; 

fertility_pop = merge(fertility_pop, env, by = c("Population", "Date", "Year"), all = T); 
fertility = fertility_pop %>% mutate(Month = coalesce(Month.x, Month.y)) %>% dplyr::select(-Month.x, -Month.y) %>% filter(!is.na(Year), !is.na(Date))

#fertility = fertility %>% filter(!is.na(Year))

#See how many NAs and which months have missing data
fertility$year_month = paste(fertility$Year, fertility$Month); fertility$sp_pop = paste(fertility$Species, fertility$Population)
nas = fertility %>% dplyr::select(Year, Month, sp_pop, prop_peak) %>% arrange(Year, match(Month, month.name)) %>% 
                      filter(sp_pop %in% c("NA Cala Estreta", "NA Port de la Selva"))
nas = table(nas$Year, nas$Month, nas$sp_pop)
na_CalaE = as.data.frame.matrix(nas[,,1]) %>% dplyr::select(month.name); na_CalaE[na_CalaE == 1] = NA
na_PdS = as.data.frame.matrix(nas[,,2]) %>% dplyr::select(month.name); na_PdS[na_PdS == 1] = NA

#Initial data exploration
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
variables <- fertility %>% dplyr::select(prop_peak,colnames(variables))
vif <- lm(prop_peak ~ ., variables, na.action = na.omit); #summary (vif)

vif_values <- vif(vif)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)
sort(vif_values)

#Test for stationarity in the time series
time_s = fertility %>% filter(Species == "Ericaria crinita", Population == "Cala Estreta", !Year %in% c(2019))
adf.test(time_s$prop_peak)
kpss.test(time_s$prop_peak)

# 2: Cross-correlation analysis ---------------------
#Exploration with some packages
time_s = fertility %>% filter(Species == "Ericaria mediterranea", Population == "Cala Estreta", !Year %in% c(2019))
ts.plot(ts(time_s$prop_peak))
cross_c = ccf(time_s$photoperiod, time_s$prop_peak,  lag.max = 6, type = "correlation", 
                     na.action = na.pass, plot = F); cross_c

#another package
lag2.plot(time_s$po4,time_s$prop_peak, 8)

#Summarise all this cross-correlation information in one table for future use
popul = c("Cala Estreta", "Port de la Selva"); sp = unique(as.character(fertility$Species))
env_vars = c("sst", "bot_t", "photoperiod", "po4", "no3", "nh4")
varnames = c("Sea Surface Temperature (°C)", "Bottom temperature (°C)", "Photoperiod length (hours)", 
             "Phosphate (mmol/m3)", "Nitrate (mmol/m3)", "Ammonium (mmol/m3)")
cross = data.frame(); cross_plots = list()

#Cross-correlation analysis for ALL species and populations
cross_corr_ALL = foreach(j = 1:2, .packages = c("dplyr", "stats", "forecast", "ggpubr"), 
                               .combine = c, .errorhandling = "stop")%do%{ #Loop per population
  #cross = data.frame(); cross_plots = list()
  for(s in 1:length(sp)){ #Include all species in the loop
    print(paste(popul[j], sp[s]))
    time_s = fertility %>% filter(Species %in%  sp[s], 
                                  Population %in% popul[j], !Date %in% c("2019-09-26", "2019-02-20"), !is.na(Year)) #Delete single point in the middle of time series
    plotlist = list()
    for(e in 1:length(env_vars)){
      var = env_vars[e]
      cr = stats::ccf(time_s[,var],time_s$prop_peak,  lag.max = 6, type = "correlation", na.action = na.pass, plot = F);
      cr = data.frame(Population = popul[j], Species = sp[s], env_var = var, lag = cr$lag, corr = cr$acf) #%>% 
                  #mutate(CIupper = confint(lm(cr$corr ~ 1))[2])
      # cr = cr[c(which(cr[,"lag"] == 0),
      #           which.max(cr[,"corr"]),
      #           which.min(cr[,"corr"])),]
      cross = rbind(cross, cr)
      
      ccf_plot = ggCcf(time_s[,var], time_s$prop_peak, lag.max = 6, 
            type = c("correlation"), na.action = na.remove, linewidth = 1) + 
        theme_minimal() + labs(x = NULL, y = NULL, title = varnames[e])
      
      plotlist = append(plotlist, list(ccf_plot))
      
    }
    
    cr_plots = ggarrange(plotlist = plotlist, ncol = 3, nrow = 2)
      
    cr_plots = annotate_figure(cr_plots + theme(plot.margin = margin(0.75,0.5,0,0, "cm")), 
                    fig.lab = paste0(sp[s], " from ", popul[j]), fig.lab.size = 18,
                    left = text_grob("Correlation", rot = 90, vjust = 1, size = 15),
                    bottom = text_grob("Lag", vjust = 1, size = 15))
    cr_plots = list(cr_plots); names(cr_plots) = paste(popul[j], sp[s])
    cross_plots = append(cross_plots, cr_plots) 
  }
    # cross_correlation_population = ; names(cross_correlation_population) = popul[j]
    # cross_correlation_population
    list(list(data = cross, Plots = cross_plots))
                                                             
}

#Select the most relevant plots and store data for further analysis
cross = cross_corr_ALL[[2]]$data %>% filter(!corr == 0)
cross_plots = cross_corr_ALL[[2]]$Plots[c(1,3:5)]

#write.table(cross, "Cross_correlation_table.txt", row.names = T,  sep="\t")

cross_correlation_analysis = ggarrange(plotlist = cross_plots, ncol = 2, nrow = 2) + theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
ggsave(file="./Plots/cross_correlation_ALL.png", plot=cross_correlation_analysis, width=20, height=12, dpi = 600)

#Save plots individually
ggsave(file="./Plots/cross_correlation_PdS_ele.png", plot=cross_plots$`Port de la Selva Gongolaria elegans`, width=12, height=8, dpi = 600)
ggsave(file="./Plots/cross_correlation_PdS_crin.png", plot=cross_plots$`Port de la Selva Ericaria crinita`, width=12, height=8, dpi = 600)
ggsave(file="./Plots/cross_correlation_CE_medi.png", plot=cross_plots$`Cala Estreta Ericaria mediterranea`, width=12, height=8, dpi = 600)
ggsave(file="./Plots/cross_correlation_CE_crin.png", plot=cross_plots$`Cala Estreta Ericaria crinita`, width=12, height=8, dpi = 600)

# 2: GAMM with lagged environmental variables ---------------------
#Load data again and clean missing years
env = read.csv("env_data_serie_temporal.txt",header=T,dec=".",sep="\t", check.names = F); #env = env %>% filter(!is.na(sst))
env = env %>% mutate(Month = month.name[month_extract]) %>% dplyr::select(-stat_year, -year_day, -year_month) 
env_reduced = env %>% filter(!is.na(Date))

#merge fertility with environmental data
fertility_metrics = readRDS("Fertility_metrics.RData")
fertility_pop = fertility_metrics$Population; 

fertility_pop = merge(fertility_pop, env, by = c("Population", "Date", "Year"), all = T); 
fertility = fertility_pop %>% mutate(Month = coalesce(Month.x, Month.y)) %>% 
                  dplyr::select(-Month.x, -Month.y) %>% filter(!is.na(Year), !is.na(Date))

fertility = fertility %>% filter(!is.na(Year), !Date %in% c("2019-09-26", "2019-02-20"))

#Explore the most correlated lags (before peak) for the model
cross = read.csv("Cross_correlation_table.txt",header=T,dec=".",sep="\t", check.names = F) #env = env %>% filter(!is.na(sst))

cross = cross %>% group_by(Population, Species, env_var)










