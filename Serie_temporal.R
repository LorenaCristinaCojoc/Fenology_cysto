#############################################
########## REPRODUCTIVE PHENOLOGY TIME SERIES
#############################################
remove(list=ls())

listoflibrary<-c("purrr", "ggplot2", "ggpubr", "car", "viridisLite", "viridis", "readxl", "data.table","raster", #"seegSDM"
                 "geosphere", "mgcv", "Rmisc",  "doParallel", "foreach", "parallel", "tidyverse")

for (pkg in listoflibrary){
  if(!eval(bquote(require(.(pkg))))) {eval(bquote(install.packages(.(pkg))))}
  eval(bquote(library(.(pkg))))
}

#Set the number of cores to do functions in parallel
numcores <- detectCores()
registerDoParallel(numcores)

#Functions
nearestLand <- function (points, raster, max_distance) {
  # get nearest non_na cells (within a maximum distance) to a set of points
  # points can be anything extract accepts as the y argument
  # max_distance is in the map units if raster is projected
  # or metres otherwise
  
  # function to find nearest of a set of neighbours or return NA
  nearest <- function (lis, raster) {
    neighbours <- matrix(lis[[1]], ncol = 2)
    point <- lis[[2]]
    # neighbours is a two column matrix giving cell numbers and values
    land <- !is.na(neighbours[, 2])
    if (!any(land)) {
      # if there is no land, give up and return NA
      return (c(NA, NA))
    } else{
      # otherwise get the land cell coordinates
      coords <- xyFromCell(raster, neighbours[land, 1])
      
      if (nrow(coords) == 1) {
        # if there's only one, return it
        return (coords[1, ])
      }
      
      # otherwise calculate distances
      dists <- sqrt((coords[, 1] - point[1]) ^ 2 +
                      (coords[, 2] - point[2]) ^ 2)
      
      # and return the coordinates of the closest
      return (coords[which.min(dists), ])
    }
  }
  
  # extract cell values within max_distance of the points
  neighbour_list <- raster::extract(raster, points,
                                    buffer = max_distance,
                                    cellnumbers = TRUE)
  
  # add the original point in there too
  neighbour_list <- lapply(1:nrow(points),
                           function(i) {
                             list(neighbours = neighbour_list[[i]],
                                  point = as.numeric(points[i, ]))
                           })
  
  return (t(sapply(neighbour_list, nearest, raster)))
} #Needed function to deal with NA points

#1: Data preparation --------
#1.1: Temporal series Catalunya -----------
#Mediterranea
cmed<-read.csv("./raw_data/temporal_series_C.mediterranea.txt",header=T,dec=".",sep="\t", check.names = F); 
str(cmed);

#Change names and complete missing values
cmed = cmed %>% dplyr::select(-Comments) %>% 
  mutate(Population = case_when(Population %in% " PdS" ~ "Port de la Selva", #Correct current columns
                                TRUE ~ "Cala Estreta"),
         Subpopulation = "CE",
         Quadrat = as.character(Quadrat),
         Date = as.Date(Date, tryFormats = c("%d/%m/%Y")),
         Cauloid_size = as.numeric(Cauloid_size),
         Fertility = as.numeric(ifelse(is.na(Fertility), 0, Fertility)),
         Leaf_size = as.numeric(Leaf_size),
         Stage = "Adult")%>%
  mutate(Year = as.numeric(format(Date,'%Y')),
         uniq_id = paste("CE_med", substr(Month, 1, 3), substr(format(Date,'%Y'), 3, 4), sep = "_")) #Create new useful columns

str(cmed); cmed$Species = "Ericaria mediterranea"
#write.table(cmed, "C.med_clean_temporal_phenology.txt", row.names = T,  sep="\t")

#Crinita/elegans
crin = read.csv("./raw_data/temporal_series_E.crinita.txt",header=T,dec=".",sep="\t", check.names = F); 
str(crin); colnames(crin)[c(7:9)] = c("Cauloid_size", "Fertility", "Leaf_size")

#crin_corr = crin %>% filter(!Species %in% c("Cystoseira crinita", "Cystoseira_crinita"), Month %in% "May")

#Change names and complete missing values
crin = crin %>% dplyr::select(-Comments) %>% 
                mutate(Population = case_when(Population %in% "PdS" ~ "Port de la Selva", #Correct current columns
                                              TRUE ~ "Cala Estreta"),
                       Species = case_when(Species %in% c("Cystoseira crinita", "Cystoseira_crinita") ~ "Ericaria crinita",
                                           Species %in% c("Cystoseira sp. ", "Cystoseira sp.", "Cystoseira sp", "Cystoseira_sp",
                                                          "Cystoseira elegans", "Cystoseira_elegans", 
                                                          "Cystoseira_espinosa", "Cystoseira_spinosa") ~ "Gongolaria elegans",
                                           Species %in% c("Cystoseira compressa", "Cystoseira_compressa") ~ "Cystoseira compressa",
                                           Species %in% c("reclutes", "recluta") ~ "Reclutes",
                                           TRUE ~ Species),
                       Date = as.Date(Date, tryFormats = c("%d/%m/%Y")),
                       Cauloid_size = as.numeric(Cauloid_size),
                       Fertility = as.numeric(ifelse(is.na(Fertility), 0, Fertility)),
                       Leaf_size = as.numeric(Leaf_size))%>%
                 mutate(Year = as.numeric(format(Date,'%Y')),
                        uniq_id = case_when(Population %in% "Port de la Selva" ~ paste("PS", substr(Month, 1, 3), substr(format(Date,'%Y'), 1, 2), sep = "_"), 
                                            TRUE ~ paste("CE", substr(Month, 1, 3), substr(format(Date,'%Y'), 3, 4), sep = "_"))) #Create new useful columns

str(crin)

#Rename subpopulations --- NOT USED
c(crin %>% filter(Population == "Port de la Selva") %>% distinct(Subpopulation))
c(crin %>% filter(Population == "Cala Estreta") %>% distinct(Subpopulation))

#Merge both dataframes
phen_all = bind_rows(cmed, crin); 

#Add coordinates
phen_all = phen_all %>% mutate(lon = case_when(Population %in% "Port de la Selva" ~ 3.185744, 
                                               TRUE ~ 3.175610), 
                               lat = case_when(Population %in% "Port de la Selva" ~ 42.346378, 
                                               TRUE ~ 41.866430))
#write.table(phen_all, "Temporal_series_phenology.txt", row.names = T,  sep="\t")

#1.2: Regional 1-2 year Mediterranean monitoring -----------
#2: Calculate different indices related with phenology --------------
phen_all<-read.csv("Temporal_series_phenology.txt",header=T,dec=".",sep="\t", check.names = F); 
phen_all$Species = as.factor(phen_all$Species); #phen_all$Fertility = as.character(phen_all$Fertility)
prova = phen_all %>% filter(Species %in% c("Ericaria crinita"), Population %in% c("Cala Estreta"), Year == 2023)

#Data exploration
table(phen_all$Species) #Total counts
length(unique(phen_all$Date)) #Unique sampling days
uniq_yr <- phen_all %>% filter(!Species %in% c("Reclutes", "Cystoseira compressa", "Cystoseira spinosa")) %>% 
                        group_by(Species, Year) %>% dplyr::summarise(count = n(), .groups = "drop_last") #Total obs per year
  
#Fertility
colnames(phen_all)
fertility_class = phen_all %>% filter(!Species %in% c("Reclutes", "Cystoseira compressa", "Cystoseira spinosa"),
                                Fertility <= 4) %>% 
                         group_by(uniq_id, Population, Species, Date, Month, Year, Fertility) %>% 
                         dplyr::summarise(count = n(), 
                                   Cauloid_mean = mean(Cauloid_size, na.rm = T), 
                                   Cauloid_sd = sd(Cauloid_size, na.rm = T),
                                   Leaf_mean = mean(Leaf_size, na.rm = T), 
                                   Leaf_sd = sd(Leaf_size, na.rm = T), .groups = "drop_last")

fertility_pop <- reshape2::dcast(fertility_class, uniq_id + Population + Species + Date + Month + Year ~ Fertility, 
                       value.var = "count", fun.aggregate = sum)

#Obtain several metrics per sampling event
for(i in 1:nrow(fertility_pop)){
  fertility_pop$prop_fertile[i] = (sum(fertility_pop[i,c(8:11)])/sum(fertility_pop[i,c(7:11)]))*100 #proportion of fertile individuals
  
  fertility_pop$prop_F0[i] = (fertility_pop[i,7]/sum(fertility_pop[i,c(7:11)]))*100 #Proportion of each fertility class
  fertility_pop$prop_F1[i] = (fertility_pop[i,8]/sum(fertility_pop[i,c(7:11)]))*100
  fertility_pop$prop_F2[i] = (fertility_pop[i,9]/sum(fertility_pop[i,c(7:11)]))*100
  fertility_pop$prop_F3[i] = (fertility_pop[i,10]/sum(fertility_pop[i,c(7:11)]))*100
  fertility_pop$prop_F4[i] = (fertility_pop[i,11]/sum(fertility_pop[i,c(7:11)]))*100
  
  fertility_pop$prop_peak[i] = sum(fertility_pop$prop_F4[i], fertility_pop$prop_F3[i]) #proportion of F3 and F4 (peak)
  
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
      
      prop_peak = max(temp_pop$prop_peak) #Percentage where individuals in the population are the most fertile
      peak = temp_pop[which.max(temp_pop$prop_peak), "Month"] #Month where most individuals are fertile
      
      first_maturity = ifelse(n_months_fertile == 0, #If it's not mature 
                              "None",
                              with(temp_pop[,], Month[which(prop_fertile >= 30)])) #Start of the fertile period
      
      least_maturity = ifelse(n_months_fertile == 0, #If it's not mature 
                              "None",
                              with(temp_pop[,], Month[which.min(prop_fertile)])) #Start of the fertile period

      last_maturity = ifelse(nrow(temp_pop %>% filter(prop_fertile < 30)) == 0, #If there is no month 
                             "None",
                             temp_pop[tail(which(temp_pop[order(temp_pop$Date),12]!=0, arr.ind = T),1), "Month"]) #Last month of the fertile period
      
      fert_classes = temp_pop[, c(7:11)]; fert_classes[fert_classes == 0] <- NA
      fert_classes = t(as.data.frame(colMeans(fert_classes, na.rm = T))) #Mean individuals per fertile class WHEN fertile
      total_ind = sum(temp_pop[, c(7:11)])
      
      n_months_peak = nrow(temp_pop %>% filter(prop_peak >= quantile(temp_pop$prop_peak)[4])) #Number of months when fertility would be at its peak
      
      #Join everything in a dataframe
      temp_fertility = data.frame(Population = popul[j], Species = sp[s], Year = years[i], 
                                  fert_classes, total_ind = total_ind, n_months_fertile = n_months_fertile, obs_fertile = obs_fertile, 
                                  peak = peak, prop_peak = prop_peak, n_months_peak = n_months_peak,
                                  first_maturity = first_maturity, last_maturity = last_maturity, least_maturity = least_maturity)
      temp_fertility[is.na(temp_fertility)] <- 0
      
      } else {temp_fertility = data.frame(Population = popul[j], Species = sp[s], Year = years[i]) }
      
      fertility_metrics_year = bind_rows(fertility_metrics_year, temp_fertility)
      
    } #END loop species
    
  } #END loop Population
  
}#END loop YEAR

#Remove NAs
fertility_metrics_year = fertility_metrics_year %>% filter(!is.na(last_maturity)) %>% mutate(last_maturity = ifelse(last_maturity == "0", "None", last_maturity))
rownames(fertility_metrics_year) = NULL

#Clean by number of observations
#CIa <- CI(fertility_metrics_year$total_ind, ci = 0.995) # We use a 99% to see which values fall below the 0.5% rather than the 2.5% that would be the 95 conventional confidence interval
#fertility_metrics_year <- fertility_metrics_year[which(fertility_metrics_year$total_ind >= CIa[3]),]

#Fertility metrics data
fertility_metrics = list(Year = fertility_metrics_year, Population = fertility_pop, `Size class` = fertility_class)
saveRDS(fertility_metrics, "Fertility_metrics.RData")

#2.1: Extract environmental variables --------------
#Load the database with the coordinates ------
data <- read.csv("Temporal_series_phenology.txt",header=T,dec=".",sep="\t", check.names = FALSE)
data$Date = as.Date(data$Date)
data$month_extract = str_pad(sapply(data$Month, function(x) which(month.name == x)), , width = 2, pad = '0')

#season<-read.csv("./Data/Raw_data/species_site_AFDW_2005.txt",header=T,dec=".",sep="\t")
#sample.mat <- dcast(season,station+year+ month +lon+lat~taxon, mean, value.var='wet_weight') #with this function we can create a matrix with the rows on the left side
season <- data %>% dplyr::select(Population, Year, month_extract) %>% distinct(.)
data <- data %>% dplyr::select(Population, Date, Year, month_extract) %>% distinct(.)
season = tidyr::expand(season, Population, Year, month_extract) %>% mutate(lon = case_when(Population %in% "Port de la Selva" ~ 3.185744, 
                                                                                    TRUE ~ 3.175610), 
                                                                    lat = case_when(Population %in% "Port de la Selva" ~ 42.346378, 
                                                                                    TRUE ~ 41.866430)) %>% filter(!is.na(month_extract), !is.na(Year))


season$stat_year <- paste(season$Population, season$Year, sep = "_")
#season$month_extract = format(season$Date,'%m')
#season <- season[which(!duplicated(season$stat_year)),]; season <- season %>% dplyr::select(station, year, month)
#env <- data %>% dplyr::select(station, year, lon, lat)
#env <- merge(env, season, by = c("station", "year")); env$month <- ifelse(env$month > 9, env$month, sprintf("%02d", env$month))
env = season
env$year_month <- paste(env$Year,env$month_extract, sep = ".")
#env$year_day = gsub("-", ".", env$Date)
#env = env %>% select(Population, Year, Month, lon, lat, year_month, stat_year, month_extract) %>% distinct(.)

#Prepare the dataset and extract the coordinates for which we want information
env <- env %>% filter(!year_month %in% c("2025.11", "2025.12")) #select the years of interest
env = merge(env, data, by = c("Population", "Year", "month_extract"), all = T); 

env = env %>% filter(!is.na(lon)); env$year_day = gsub("-", ".", env$Date)
datasp <- env[,4:5]; coordinates(datasp)=~lon+lat #Select the coordinates to extract

# Bottom temperature ------------
#coords: N = 42.424697392587376 / S = 41.3640154786213 / W = 2.1950003946838477 / E = 3.5036339249017425
temperature <- env 
temp <- './Env_data/Temp_data_fenologia_2016_2023.nc'; temp <- brick(temp, var = "bottomT")
temp1 <- './Env_data/Temp_data_fenologia_2023_2025.nc'; temp1 <- brick(temp1, var = "bottomT") # Start at 01/06/2023

temp <- raster::stack(temp,temp1) #Join all the files in one

var_raw<- raster::extract(temp,datasp,method = 'bilinear', small = T, na.rm = T, df = T, exact = T, cellnumbers = F) #Extract the data for the selected coordinates

var = var_raw
colnames(var) <- substr(colnames(var), 2,11) #Extract only the year and month for the name, expand to more characters if the day is also needed
#temp_red = temperature %>% filter(!is.na(Date))

for (i in 1:nrow(temperature)){ #Obtain the median value for each month
  
  if(is.na(temperature$year_day[i])){bot_temp_day = NA} else {bot_temp_day = var[i,which(colnames(var) == temperature$year_day[i])]} #Environmental data for a specific day
  
  temperature$bot_temp_day[i] = bot_temp_day
}

colnames(var) <- substr(colnames(var), 1,7) #Extract only the year and month for the name, expand to more characters if the day is also needed
var[1:5,1:5]


for (i in 1:nrow(temperature)){ #Obtain the median value for each month
  
  month <-var[i,which(colnames(var) == temperature$year_month[i])]
  temperature$bot_t[i] <- apply(month, 1, median) #Median for a single month in a specific year
  
  temperature$max_t[i] = max(month)
  
  year <- var[i,which(substr(colnames(var), 1,4) == temperature$Year[i])]
  temperature$bot_t_var[i] <- apply(year, 1, sd) #Variability in a specific year
  
  month <- var[i,which(substr(colnames(var), 1,7) == temperature$year_month[i])]
  temperature$bot_t_var_month[i] <- apply(month, 1, sd) #Variability in a specific month for a certain year
  
}

#Deal with NAs; 
tempNA <- temperature[which(is.na(temperature$bot_t)),];

#find the coordinates of the nearest nonNA cell in the raster
nas <- tempNA[,4:5]
q = 300 #we set a maximum distance for the function to look at coordinates, this is in meters
repeat{
  print(q)
  #With this nearestLand function, we calculate the nearest point in a raster file which is not NA
  #To deal with possible NA's in our data due to mistmatch between in-situ points and the satellite coordinates.
  land <- as.data.frame(nearestLand(nas, temp@layers[[1]], q)); colnames(land) <- c("lon", "lat")
  dup <- c(which(is.na(land)))
  q = q + 100
  if(length(dup)==0){break}
} #We repeat it until there are not more NAs increasing the maximum distance by 1km each iteration

coordinates(land)= ~lon + lat
var_na<- raster::extract(temp,land,method = 'bilinear', small = T, na.rm = T, df = T, exact = T, cellnumbers = F);

var = var_na
colnames(var) <- substr(colnames(var), 2,11); 

for (i in 1:nrow(tempNA)){ #Obtain the median value for each month
  
  if(is.na(tempNA$year_day[i])){bot_temp_day = NA} else {bot_temp_day = var[i,which(colnames(var) == tempNA$year_day[i])]} #Environmental data for a specific day
  
  tempNA$bot_temp_day[i] = bot_temp_day
}

colnames(var) <- substr(colnames(var), 1,7) #Extract only the year and month for the name, expand to more characters if the day is also needed
var[1:5,1:5]

for (i in 1:nrow(tempNA)){
  
  month <-var[i,which(substr(colnames(var), 1,7) == tempNA$year_month[i])]
  tempNA$bot_t[i] <- apply(month, 1, median) #Median for a single month in a specific year
  
  tempNA$max_t[i] = max(month)
  
  year <- var[i,which(substr(colnames(var), 1,4) == tempNA$Year[i])]
  tempNA$bot_t_var[i] <- apply(year, 1, sd) #Variability in a specific year
  
  month <- var[i,which(substr(colnames(var), 1,7) == tempNA$year_month[i])]
  tempNA$bot_t_var_month[i] <- apply(month, 1, sd) #Variability in a specific month for a certain year
  
  
} 
colnames(tempNA)[10:14] <- c("var1","var2", "var3", "var4", "var5"); #Now we merge the original data with the nearest non-NA values
temperature <- temperature %>% full_join(tempNA) %>% #filter(!is.na(Date)) %>% 
  mutate(bot_temp_day = coalesce(bot_temp_day,var1),
         max_t =coalesce(bot_t,var2),
         bot_t = coalesce(bot_t,var3),
         bot_t_var = coalesce(bot_t_var, var4),
         bot_t_var_month = coalesce(bot_t_var_month, var5)) %>% dplyr::select(-var1, -var2, -var3, -var4, -var5); rm(temp, temp1)

# Sea Surface temperature ------------
#coords: N = 42.424697392587376 / S = 41.3640154786213 / W = 2.1950003946838477 / E = 3.5036339249017425
sst <- env
#setwd("C:/Users/avipo/OneDrive - Danmarks Tekniske Universitet/Skrivebord/PhD/Project 1/MarenzelleriaSweden/env_data/")
temp <- './Env_data/Temp_data_fenologia_2016_2023.nc'; temp <- brick(temp, var = "thetao", level = 1)
temp1 <- './Env_data/Temp_data_fenologia_2023_2025.nc'; temp1 <- brick(temp1, var = "thetao", level = 1) #Start at 01/06/2023, at 1m depth

temp <- raster::stack(temp,temp1) #Join all the files in one

var_raw<- raster::extract(temp,datasp,method = 'bilinear', small = T, na.rm = T, df = T, exact = T, cellnumbers = F) #Extract the data for the selected coordinates

var = var_raw
colnames(var) <- substr(colnames(var), 2,11) #Extract only the year and month for the name, expand to more characters if the day is also needed
#temp_red = temperature %>% filter(!is.na(Date))

for (i in 1:nrow(sst)){ #Obtain the median value for each month
  
  if(is.na(sst$year_day[i])){sst_day = NA} else {sst_day = var[i,which(colnames(var) == sst$year_day[i])]} #Environmental data for a specific day
  
  sst$sst_day[i] = sst_day
}

colnames(var) <- substr(colnames(var), 1,7) #Extract only the year and month for the name, expand to more characters if the day is also needed
var[1:5,1:5]


for (i in 1:nrow(sst)){ #Obtain the median value for each month
  
  month <-var[i,which(colnames(var) == sst$year_month[i])]
  sst$sst[i] <- apply(month, 1, median) #Median for a single month in a specific year
  
  sst$max_sst[i] = max(month)
  
  sst$min_sst[i] = min(month)
  
  year <- var[i,which(substr(colnames(var), 1,4) == sst$Year[i])]
  sst$sst_var[i] <- apply(year, 1, sd) #Variability in a specific year
  
  month <- var[i,which(substr(colnames(var), 1,7) == sst$year_month[i])]
  sst$sst_var_month[i] <- apply(month, 1, sd) #Variability in a specific month for a certain year
  
}

#Deal with NAs; 
tempNA <- sst[which(is.na(sst$sst)),];

#find the coordinates of the nearest nonNA cell in the raster
nas <- tempNA[,4:5]
q = 300 #we set a maximum distance for the function to look at coordinates, this is in meters
repeat{
  print(q)
  #With this nearestLand function, we calculate the nearest point in a raster file which is not NA
  #To deal with possible NA's in our data due to mistmatch between in-situ points and the satellite coordinates.
  land <- as.data.frame(nearestLand(nas, temp@layers[[1]], q)); colnames(land) <- c("lon", "lat")
  dup <- c(which(is.na(land)))
  q = q + 100
  if(length(dup)==0){break}
} #We repeat it until there are not more NAs increasing the maximum distance by 1km each iteration

coordinates(land)= ~lon + lat
var_na<- raster::extract(temp,land,method = 'bilinear', small = T, na.rm = T, df = T, exact = T, cellnumbers = F);

var = var_na
colnames(var) <- substr(colnames(var), 2,11); 

for (i in 1:nrow(tempNA)){ #Obtain the median value for each month
  
  if(is.na(tempNA$year_day[i])){sst_day = NA} else {sst_day = var[i,which(colnames(var) == tempNA$year_day[i])]} #Environmental data for a specific day
  
  tempNA$sst_day[i] = sst_day
}

colnames(var) <- substr(colnames(var), 1,7) #Extract only the year and month for the name, expand to more characters if the day is also needed
var[1:5,1:5]

for (i in 1:nrow(tempNA)){
  
  month <-var[i,which(substr(colnames(var), 1,7) == tempNA$year_month[i])]
  tempNA$sst[i] <- apply(month, 1, median) #Median for a single month in a specific year
  
  tempNA$max_sst[i] = max(month)
  
  tempNA$min_sst[i] = min(month)
  
  year <- var[i,which(substr(colnames(var), 1,4) == tempNA$Year[i])]
  tempNA$sst_var[i] <- apply(year, 1, sd) #Variability in a specific year
  
  month <- var[i,which(substr(colnames(var), 1,7) == tempNA$year_month[i])]
  tempNA$sst_var_month[i] <- apply(month, 1, sd) #Variability in a specific month for a certain year
  
  
} 
colnames(tempNA)[10:15] <- c("var1","var2", "var3", "var4", "var5", "var6"); #Now we merge the original data with the nearest non-NA values
sst <- sst %>% full_join(tempNA) %>% #filter(!is.na(Date)) %>% 
  mutate(sst_day = coalesce(sst_day,var1),
         max_sst =coalesce(max_sst,var2),
         min_sst =coalesce(min_sst,var3),
         sst = coalesce(sst,var4),
         sst_var = coalesce(sst_var, var5),
         sst_var_month = coalesce(sst_var_month, var6)) %>% dplyr::select(-var1, -var2, -var3, -var4, -var5, -var6); rm(temp, temp1)

# Nutrients concentration ----
#coords: N = 42.424697392587376 / S = 41.3640154786213 / W = 2.1950003946838477 / E = 3.5036339249017425
nutrients = c("nh4", "no3", "po4")
all_nutri = foreach(t = 1:length(nutrients), .packages = c("dplyr", "raster"), .errorhandling = "stop")%dopar%{
nutr <- env 
nutrient <- './Env_data/Nutrients_2016_2023.nc'; nutrient <- brick(nutrient, var = nutrients[t], level = 1)
nutrient1 <- './Env_data/Nutrients_2023_2025.nc'; nutrient1 <- brick(nutrient1, var = nutrients[t], level = 1)

nutrient <- raster::stack(nutrient,nutrient1) #Join all the files in one

var_raw<- raster::extract(nutrient,datasp,method = 'bilinear', small = T, na.rm = T, df = T, exact = T, cellnumbers = F) #Extract the data for the selected coordinates

var = var_raw
colnames(var) <- substr(colnames(var), 2,11) #Extract only the year and month for the name, expand to more characters if the day is also needed
#temp_red = temperature %>% filter(!is.na(Date))

for (i in 1:nrow(nutr)){ #Obtain the median value for each month
  
  if(is.na(nutr$year_day[i])){nutr_day = NA} else {nutr_day = var[i,which(colnames(var) == nutr$year_day[i])]} #Environmental data for a specific day
  
  nutr$nutr_day[i] = nutr_day
}

colnames(var) <- substr(colnames(var), 1,7) #Extract only the year and month for the name, expand to more characters if the day is also needed
var[1:5,1:5]


for (i in 1:nrow(nutr)){ #Obtain the median value for each month
  
  month <-var[i,which(colnames(var) == nutr$year_month[i])]
  nutr$nutr[i] <- apply(month, 1, median) #Median for a single month in a specific year
  
  nutr$max_nutr[i] = max(month)
  
  nutr$min_nutr[i] = min(month)
  
  year <- var[i,which(substr(colnames(var), 1,4) == nutr$Year[i])]
  nutr$nutr_var[i] <- apply(year, 1, sd) #Variability in a specific year
  
  month <- var[i,which(substr(colnames(var), 1,7) == nutr$year_month[i])]
  nutr$nutr_var_month[i] <- apply(month, 1, sd) #Variability in a specific month for a certain year
  
}

#Deal with NAs; 
tempNA <- nutr[which(is.na(nutr$nutr)),];

#find the coordinates of the nearest nonNA cell in the raster
nas <- tempNA[,4:5]
q = 300 #we set a maximum distance for the function to look at coordinates, this is in meters
repeat{
  print(q)
  #With this nearestLand function, we calculate the nearest point in a raster file which is not NA
  #To deal with possible NA's in our data due to mistmatch between in-situ points and the satellite coordinates.
  land <- as.data.frame(nearestLand(nas, nutrient@layers[[1]], q)); colnames(land) <- c("lon", "lat")
  dup <- c(which(is.na(land)))
  q = q + 100
  if(length(dup)==0){break}
} #We repeat it until there are not more NAs increasing the maximum distance by 1km each iteration

coordinates(land)= ~lon + lat
var_na<- raster::extract(nutrient,land,method = 'bilinear', small = T, na.rm = T, df = T, exact = T, cellnumbers = F);

var = var_na
colnames(var) <- substr(colnames(var), 2,11); 

for (i in 1:nrow(tempNA)){ #Obtain the median value for each month
  
  if(is.na(tempNA$year_day[i])){nutr_day = NA} else {nutr_day = var[i,which(colnames(var) == tempNA$year_day[i])]} #Environmental data for a specific day
  
  tempNA$nutr_day[i] = nutr_day
}

colnames(var) <- substr(colnames(var), 1,7) #Extract only the year and month for the name, expand to more characters if the day is also needed
var[1:5,1:5]

for (i in 1:nrow(tempNA)){
  
  month <-var[i,which(substr(colnames(var), 1,7) == tempNA$year_month[i])]
  tempNA$nutr[i] <- apply(month, 1, median) #Median for a single month in a specific year
  
  tempNA$max_nutr[i] = max(month)
  
  year <- var[i,which(substr(colnames(var), 1,4) == tempNA$Year[i])]
  tempNA$nutr_var[i] <- apply(year, 1, sd) #Variability in a specific year
  
  month <- var[i,which(substr(colnames(var), 1,7) == tempNA$year_month[i])]
  tempNA$nutr_var_month[i] <- apply(month, 1, sd) #Variability in a specific month for a certain year
  
  
} 
colnames(tempNA)[10:15] <- c("var1","var2", "var3", "var4", "var5", "var6"); #Now we merge the original data with the nearest non-NA values
nutr <- nutr %>% full_join(tempNA) %>% #filter(!is.na(Date)) %>% 
  mutate(nutr_day = coalesce(nutr_day,var1),
         nutr = coalesce(nutr,var2),
         max_nutr =coalesce(max_nutr,var3),
         min_nutr =coalesce(min_nutr,var4),
         nutr_var = coalesce(nutr_var, var5),
         nutr_var_month = coalesce(nutr_var_month, var6)) %>% dplyr::select(-var1, -var2, -var3, -var4, -var5, -var6); rm(nutrient, nutrient1)

colnames(nutr)[10:15] = gsub("nutr", nutrients[t], colnames(nutr)[10:15])

nutr

}

names(all_nutri) = nutrients
nh4 = all_nutri[[1]]; no3 = all_nutri[[2]]; po4 = all_nutri[[3]]

# Merge all environmental data --------
# <- env %>% select(c(1:10))
coln <- c(colnames(env))
env <- merge(env, temperature, by = coln)
env <- merge(env, sst, by = coln)
env = merge(env, nh4, by = coln)
env = merge(env, no3, by = coln)
env = merge(env, po4, by = coln)

# Daylenght -------------
for (i in 1:nrow(env)){
  env$photoperiod[i] <- daylength(env$lat[i], env$Date[i]) #Photoperiod for the specific sampling day
  
  #Increase/decrease in photoperiod from previous month 
  avg_mon_dl = tapply(daylength(env$lat[i], 1:365), rep(1:12, c(31,28,31,30,31,30,31,31,30,31,30,31)), mean)
  
  
  env$photoperiod_month[i] = avg_mon_dl[as.numeric(env$month_extract[i])]
  
  # env$photoperiod_month[i] = case_when(dayl$month_extract[i] != "01" ~ avg_mon_dl[as.numeric(dayl$month_extract[i])] - avg_mon_dl[as.numeric(dayl$month_extract[i]) - 1],
  #                                   TRUE ~ avg_mon_dl[as.numeric(dayl$month_extract[i])] - avg_mon_dl[12])

  # dayl$perc_diff_dayl[i] = case_when(dayl$month_extract[i] != "01" ~ (avg_mon_dl[as.numeric(dayl$month_extract[i])] - avg_mon_dl[as.numeric(dayl$month_extract[i]) - 1])/avg_mon_dl[as.numeric(dayl$month_extract[i]) - 1]*100,
  #                                 TRUE ~ (avg_mon_dl[as.numeric(dayl$month_extract[i])] - avg_mon_dl[12])/avg_mon_dl[as.numeric(dayl$month_extract[i])] * 100)
} 

#write.table(env,file="env_data_serie_temporal.txt",sep="\t", row.names = TRUE)

#3: Plots of fertility time-series ---------
env = read.csv("env_data_serie_temporal.txt",header=T,dec=".",sep="\t", check.names = F); #env = env %>% filter(!is.na(sst))
env = env %>% select(-stat_year, -year_day, -year_month) %>% mutate(Month = month.name[month_extract])
#remove wrong date
#env = env[!(env$Month == "September"  & env$Date == "2023-08-08"),]
#env = env[-93,]

env_reduced = env %>% filter(!is.na(Date)) 
#merge fertility with environmental data
fertility_metrics = readRDS("Fertility_metrics.RData")
fertility_pop = fertility_metrics$Population; 

fertility_class = merge(fertility_metrics$`Size class`, env, by = c("Population", "Date", "Year"), all = T)

#create colors vector
fert_colors = c("F0" = "gray75","F1" = "#fce9db", "F2" = "#ead2ac", "F3" = "#e6b89c", "F4" = "#c99789")

#Trials with some species, data cleaning, removing duplicates and adding complete temperature series
fertility_crin = fertility_pop %>% filter(Species %in% "Ericaria crinita", #Year %in% 2023,
                                          Population %in% c("Cala Estreta"))

fertility_crin = merge(fertility_crin, env %>% filter(Population %in% "Cala Estreta"#, Year %in% 2023,
                                                      ), by = c("Population", "Date", "Year"), all = T); 
fertility_crin = fertility_crin %>% mutate(Month = coalesce(Month.x, Month.y)) %>% 
                  select(-Month.x, -Month.y)

#remove all duplicates
# dupl_crin = fertility_crin[duplicated(fertility_crin[,c("Year", "Month")]) | duplicated(fertility_crin[,c("Year", "Month")], fromLast=TRUE),] %>% 
#   filter(!is.na(Species))
# 
# fertility_crin = fertility_crin[-which(duplicated(fertility_crin[,c("Year", "Month")]) | duplicated(fertility_crin[,c("Year", "Month")], fromLast=TRUE)), ]
# fertility_crin = rbind(fertility_crin, dupl_crin)
fertility_crin = fertility_crin[with(fertility_crin, order(month_extract, Year)),]

#yr2023 = fertility_crin %>% filter(Year == 2023)

#These duplicates need to be coalesced, as we cannot lose information
#dupl_crin = fertility_crin[duplicated(fertility_crin[,3:4]) | duplicated(fertility_crin[,3:4], fromLast=TRUE),] #Get all duplicated rows

fert_barpl = reshape2::melt(fertility_crin, id.vars = c(colnames(fertility_crin)[c(1:5,52,21:51)]), measure.vars = c("prop_F0", "prop_F1", "prop_F2", "prop_F3", "prop_F4"),
                  variable.name = "class", value.name = "proportion")
fert_barpl$class = as.factor(fert_barpl$class); levels(fert_barpl$class) = c("F0", "F1", "F2", "F3", "F4")
fert_barpl$class = factor(fert_barpl$class, levels = c("F0","F4", "F3", "F2", "F1"))
fert_barpl$short_month = as.factor(substr(fert_barpl$Month, start = 1, stop = 3))


#fert_barpl = fertility_pop[order(fertility_pop$Date),] %>% filter(Species %in% "Ericaria crinita", Population == "Port de la Selva")

#Find which years have all NAs
years_na = fert_barpl %>% group_by(Year) %>% filter(all(is.na(proportion)))%>% distinct(Year) %>% pull(Year) 

plot_fert_crin <- ggplot(fert_barpl, mapping = aes(y=proportion, x = fct_inorder(short_month), group = class, fill = class, color = class)) + 
  geom_bar(stat="identity", position = "stack") + 
  facet_grid(~ as.factor(Year)) + 
  scale_fill_manual(values = fert_colors) + 
  scale_color_manual(values = fert_colors) +
  geom_ribbon(data = fert_barpl %>% filter(!Year %in% years_na),
              aes(ymin = (sst-sst_var_month)*3, ymax = (sst+sst_var_month)*3),
              alpha = 0.1, color = "mistyrose") +
  geom_line(data = fert_barpl %>% filter(!Year %in% years_na),
            aes(x = fct_inorder(short_month), y = photoperiod_month*3),
            color = "darkblue", linewidth = 1) +
  geom_line(data = fert_barpl %>% filter(!Year %in% years_na), 
            aes(x = fct_inorder(short_month), y = sst*3), 
            color = "darkred", linewidth = 1, alpha = 0.2) +
  geom_ribbon(data = fert_barpl %>% filter(!Year %in% years_na),
              aes(ymin = (no3 - no3_var_month)*10, ymax = (no3+no3_var_month)*10),
              alpha = 0.1, color = "lightyellow2") +
  geom_ribbon(data = fert_barpl %>% filter(!Year %in% years_na),
              aes(ymin = (po4 - po4_var_month)*150, ymax = (po4+po4_var_month)*150),
              alpha = 0.1, color = "cornsilk") +
  geom_ribbon(data = fert_barpl %>% filter(!Year %in% years_na),
              aes(ymin = (nh4 - nh4_var_month)*50, ymax = (nh4+nh4_var_month)*50),
              alpha = 0.1, color = "bisque") +
  geom_line(data = fert_barpl %>% filter(!Year %in% years_na), 
            aes(x = fct_inorder(short_month), y = no3*10), 
            color = "darkgreen", linewidth = 1, alpha = 0.2) +
  geom_line(data = fert_barpl %>% filter(!Year %in% years_na), 
            aes(x = fct_inorder(short_month), y = po4*150), 
            color = "gold2", linewidth = 1, alpha = 0.2) +
  geom_line(data = fert_barpl %>% filter(!Year %in% years_na), 
            aes(x = fct_inorder(short_month), y = nh4*50), 
            color = "darkorange2", linewidth = 1, alpha = 0.2) +
  scale_y_continuous(
    # Features of the first axis
    name = "Proportion",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ ./3, name="")) + labs(fill = "Fertility class") + 
  guides(color = "none") +
  theme_minimal() + theme(legend.position = "right", text = element_text(size = 15), axis.title.y = element_blank(),
                          axis.text.y = element_text(size = 15), axis.title.x=element_blank(),
                          axis.text.x = element_text(size = 9, angle = 30)); plot_fert_crin

# Convert that into a loop -------------
fert_colors = c("F0" = "gray85","F1" = "#fce9db", "F2" = "#ead2ac", "F3" = "#e6b89c", "F4" = "#c99789")
popul = c("Cala Estreta", "Port de la Selva"); sp = unique(as.character(fertility_pop$Species))
temp_series_CAT = list()

#Loop to compute metrics x year
system.time(for(j in 1:2){#START loop Population
    for(s in 1:length(sp)){#START loop species
  print(paste(popul[j], sp[s]))
  
  #Trials with some species, data cleaning, removing duplicates and adding complete temperature series
  fertility_crin = fertility_pop %>% filter(Species %in%  sp[s], 
                                            Population %in% popul[j])
  
  fertility_crin = merge(fertility_crin, env %>% filter(Population %in% popul[j]), by = c("Population", "Date", "Year"), all = T); 
  fertility_crin = fertility_crin %>% mutate(Month = coalesce(Month.x, Month.y)) %>% 
    select(-Month.x, -Month.y)
  
  fertility_crin = fertility_crin[with(fertility_crin, order(month_extract, Year)),]
  
  #These duplicates need to be coalesced, as we cannot lose information
  
  fert_barpl = reshape2::melt(fertility_crin, id.vars = c(colnames(fertility_crin)[c(1:5,52,21:51)]), measure.vars = c("prop_F0", "prop_F1", "prop_F2", "prop_F3", "prop_F4"),
                    variable.name = "class", value.name = "proportion")
  fert_barpl$class = as.factor(fert_barpl$class); levels(fert_barpl$class) = c("F0", "F1", "F2", "F3", "F4")
  fert_barpl$class = factor(fert_barpl$class, levels = c("F0","F4", "F3", "F2", "F1"))
  fert_barpl$short_month = as.factor(substr(fert_barpl$Month, start = 1, stop = 3))
  
  #Find which years have all NAs
  years_na = fert_barpl %>% group_by(Year) %>% filter(all(is.na(proportion)))%>% distinct(Year) %>% pull(Year) 
  
  plot <- ggplot(fert_barpl, mapping = aes(y=proportion, x = fct_inorder(short_month), group = class, fill = class, color = class)) + 
    geom_bar(stat="identity", position = "stack") + 
    facet_grid(~ as.factor(Year)) +
    scale_fill_manual(values = fert_colors) + 
    scale_color_manual(values = fert_colors) + #Put all the interesting environmental variables in the time-series plot
    geom_ribbon(data = fert_barpl %>% filter(!Year %in% years_na),
                aes(ymin = (sst-sst_var_month)*3, ymax = (sst+sst_var_month)*3),
                alpha = 0.1, color = "mistyrose") +
    geom_line(data = fert_barpl %>% filter(!Year %in% years_na),
              aes(x = fct_inorder(short_month), y = photoperiod_month*3),
              color = "darkblue", linewidth = 1) +
    geom_line(data = fert_barpl %>% filter(!Year %in% years_na), 
              aes(x = fct_inorder(short_month), y = sst*3), 
              color = "darkred", linewidth = 1, alpha = 0.2) +
    geom_ribbon(data = fert_barpl %>% filter(!Year %in% years_na),
                aes(ymin = (no3 - no3_var_month)*10, ymax = (no3+no3_var_month)*10),
                alpha = 0.1, color = "lightyellow2") +
    geom_ribbon(data = fert_barpl %>% filter(!Year %in% years_na),
                aes(ymin = (po4 - po4_var_month)*150, ymax = (po4+po4_var_month)*150),
                alpha = 0.1, color = "cornsilk") +
    geom_ribbon(data = fert_barpl %>% filter(!Year %in% years_na),
                aes(ymin = (nh4 - nh4_var_month)*50, ymax = (nh4+nh4_var_month)*50),
                alpha = 0.1, color = "bisque") +
    geom_line(data = fert_barpl %>% filter(!Year %in% years_na), 
              aes(x = fct_inorder(short_month), y = no3*10), 
              color = "darkgreen", linewidth = 1, alpha = 0.2) +
    geom_line(data = fert_barpl %>% filter(!Year %in% years_na), 
              aes(x = fct_inorder(short_month), y = po4*150), 
              color = "gold2", linewidth = 1, alpha = 0.2) +
    geom_line(data = fert_barpl %>% filter(!Year %in% years_na), 
              aes(x = fct_inorder(short_month), y = nh4*50), 
              color = "darkorange2", linewidth = 1, alpha = 0.2) +
    labs(fill = "Fertility class", title = sp[s]) +  guides(color = "none") +
    scale_y_continuous(
      # Features of the first axis
      name = "Proportion",
      # Add a second axis and specify its features
      sec.axis = sec_axis(~ ./3, name="")) + 
    theme_minimal() +  theme_minimal() + theme(legend.position = "right", text = element_text(size = 25), axis.title.y = element_blank(),
                                               axis.text.y = element_text(size = 20), axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5),
                                               axis.text.x = element_text(size = 20, angle = 60),
                                               legend.text=element_text(size=20), legend.title = element_text(size = 22)); #plot_fert_crin
  
  p <- list(plot); names(p) <- paste(sp[s], popul[j], sep = "_")
  temp_series_CAT <- append(temp_series_CAT, p); 


    }
})


#Merge all plots in one
fertility_CE = ggarrange(temp_series_CAT[[1]] + rremove("x.text") + 
                           theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")), 
                         temp_series_CAT[[3]]+ 
                           theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")), nrow = 2, ncol = 1, common.legend = T, legend = "bottom"); #fertility_CE

fertility_CE = annotate_figure(fertility_CE, fig.lab ="Cala Estreta", fig.lab.pos = "top.left", fig.lab.size = 30)

fertility_PS = ggarrange(temp_series_CAT[[4]] + rremove("x.text") + theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")), 
                         temp_series_CAT[[5]] + rremove("x.text") + theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")), 
                         nrow = 2, ncol = 1, common.legend = T, legend = "none"); #fertility_PS

fertility_PS = annotate_figure(fertility_PS, fig.lab ="Port de la Selva", fig.lab.pos = "top.left", fig.lab.size = 30)


fertility_all = ggarrange(fertility_PS + theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")), 
                          fertility_CE + theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")), 
                          nrow = 2, ncol = 1, common.legend = T, legend = "bottom") + 
                          theme(plot.margin = margin(1,1,1,1, "cm"));


fertility_all = annotate_figure(fertility_all, left = text_grob("Proportion of individuals (%)", rot = 90, vjust = 1, size = 35),
                right = text_grob("Sea Surface Temperature (°C) & Photoperiod length (hours)", rot = -90, vjust = 1, size = 35))

#system.time(ggsave(file="./Plot_fertility.png", plot=fertility_all, width=40, height=25, dpi = 600))

