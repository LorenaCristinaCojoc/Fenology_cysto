################################3
########## REPRODUCTIVE PHENOLOGY
library(dplyr)
library(purrr)
library(ggplot2)
library(ggpubr)
library(car)
library(viridisLite)
library(viridis)
library(tidyverse)
library(readxl)

#### Data cleaning and unification ####
sp_data = list.files(pattern = "_phenology.txt", all.files = T)
dist_list = list()

for (i in 1:length(sp_data)){
  
  file = read.csv(sp_data[i], header=T,dec=".",sep="\t", check.names = F)
  dist_list = c(dist_list,file)
  
}

#data loading and cleaning
barb<-read.csv("barbata_phenology.txt",header=T,dec=".",sep="\t", check.names = F)
crin<-read.csv("crinita_phenology.txt",header=T,dec=".",sep="\t", check.names = F); crin = crin %>% select(-17)
str(barb); str(crin)

data_full = rbind(barb, crin); colnames(data_full)[c(8, 15)] = c("codigo", "M1(perc)")
colnames(data_full)[9:16]<- gsub("\\s*\\([^\\)]+\\)", "_perc", colnames(data_full)[9:16])

data <- data_full %>% mutate(M0_perc = as.integer(M0_perc), M1_perc = as.integer(M1_perc), M2_perc = as.integer(M2_perc),
                             Temperature = as.numeric(gsub(",", ".", Temperature))) %>% filter(!Species == "") %>% 
                      mutate(codigo = case_when(Species == "G. barbata" ~ paste0("barb", sep = "_", substr(Country, start = 1, stop = 2), sep = "_", 
                                                                                 substr(Month, start = 1, stop = 3), sep = "_", Year),
                                                Species == "E. crinita" ~ paste0("crin", sep = "_", substr(Country, start = 1, stop = 2), sep = "_", 
                                                                                 substr(Month, start = 1, stop = 3), sep = "_", Year)),
                             Locality = ifelse(Country == "Croatia", "Scuza", Locality))



#########BRACHYCARPA#######
### Read the data

Fertility_brachycarpa <- read_excel("Fertility_brachycarpa.xlsx")
Fertility_brachycarpa$Month <- factor(Fertility_brachycarpa$Month, levels=c("January", "February", "March", "April", "May", "June","July", "August", "September", "October", "November", "December"))

#Fertility<-read.csv2("Crinita fertility.csv", header=T)
## Crinita<-read.csv2("Crinita_Survival_Mod.csv", header=T) Data Jana
summary(Fertility)
str(Fertility)
?str()

### Bar plot
brachycarpa <-ggplot(Fertility_brachycarpa, aes(x=Month, y=Percentage, fill=Fertility)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Year, ncol=3) +
  geom_path(aes(y=Temperature/0.3, group=1), size=1, color="Dark red") +
  scale_y_continuous(name = c(expression(paste(bold("Fertile individuals (%)")))), limit=c(0,100), breaks=seq(0,100,20), sec.axis = sec_axis(~.*0.3, name="Temperature")) +
  ## scale_fill_viridis(discrete = T) +
  scale_fill_manual(values = c("#dddddd", "#fce9db", "#ead2ac", "#e6b89c", "#c99789")) +
  #scale_x_discrete(limits=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) +  ## If you want to change the order of the levels in the x axis and only choose some
  labs(title = "C. brachycarpa: Plages Ondes")+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y.left = element_text(vjust= 1, hjust=0.5, size=15),
        axis.title.y.right = element_text(vjust= 2, hjust=0.5, size=15),
        axis.title.x = element_text(vjust= -0.5, size=15),
        axis.text.y = element_text(size=15, face="bold"),
        axis.text.x = element_text(size=8, angle = 90, vjust = 0.4), 
        legend.title = element_text(size=15, face="bold"),
        legend.text = element_text(size=15), 
        legend.position = "bottom",
        plot.title = element_text(size=18, face="bold", vjust=2, hjust=0.5))

print(brachycarpa)

ggsave("Fertility_brachycarpa.pdf", brachycarpa, width=20, height=15, units= "cm")

#########COMPRESSA#######
### Read the data

Fertility_compressa <- read_excel("Fertility_compressa.xlsx")
Fertility_compressa$Month <- factor(Fertility_compressa$Month, levels=c("January", "February", "March", "April", "May", "June","July", "August", "September", "October", "November", "December"))

#Fertility<-read.csv2("Crinita fertility.csv", header=T)
## Crinita<-read.csv2("Crinita_Survival_Mod.csv", header=T) Data Jana
summary(Fertility_compressa)
str(Fertility_compressa)
?str()

### Bar plot
compressa <-ggplot(Fertility_compressa, aes(x=Month, y=Percentage, fill=Fertility)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Year, ncol=3) +
  geom_path(aes(y=Temperature/0.3, group=1), size=1, color="Dark red") +
  scale_y_continuous(name = c(expression(paste(bold("Fertile individuals (%)")))), limit=c(0,100), breaks=seq(0,100,20), sec.axis = sec_axis(~.*0.3, name="Temperature")) +
  ## scale_fill_viridis(discrete = T) +
  scale_fill_manual(values = c("#dddddd", "#fce9db", "#ead2ac", "#e6b89c", "#c99789")) +
  #scale_x_discrete(limits=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) +  ## If you want to change the order of the levels in the x axis and only choose some
  labs(title = "C. compressa: Eleochori (Greece)")+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y.left = element_text(vjust= 1, hjust=0.5, size=15),
        axis.title.y.right = element_text(vjust= 2, hjust=0.5, size=15),
        axis.title.x = element_text(vjust= -0.5, size=15),
        axis.text.y = element_text(size=15, face="bold"),
        axis.text.x = element_text(size=8, angle = 90, vjust = 0.4), 
        legend.title = element_text(size=15, face="bold"),
        legend.text = element_text(size=15), 
        legend.position = "bottom",
        plot.title = element_text(size=18, face="bold", vjust=2, hjust=0.5))

print(compressa)

ggsave("Fertility_compressa.pdf", compressa, width=20, height=15, units= "cm")


#########Mediterranea#######
### Read the data

Fertility_mediterranea <- read_excel("Fertility_mediterranea.xlsx")
Fertility_mediterranea$Month <- factor(Fertility_mediterranea$Month, levels=c("January", "February", "March", "April", "May", "June","July", "August", "September", "October", "November", "December"))

#Fertility<-read.csv2("Crinita fertility.csv", header=T)
## Crinita<-read.csv2("Crinita_Survival_Mod.csv", header=T) Data Jana
summary(Fertility_mediterranea)
str(Fertility_mediterranea)
?str()

### Bar plot: Cala Estreta
mediterranea_estreta <-ggplot(Fertility_mediterranea[Fertility_mediterranea$Location == "Cala Estreta",], aes(x=Month, y=Percentage, fill=Fertility)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Year, ncol=3) +
  geom_path(aes(y=Temperature/0.3, group=1), size=1, color="Dark red") +
  scale_y_continuous(name = c(expression(paste(bold("Fertile individuals (%)")))), limit=c(0,100), breaks=seq(0,100,20), sec.axis = sec_axis(~.*0.3, name="Temperature")) +
  ## scale_fill_viridis(discrete = T) +
  scale_fill_manual(values = c("#dddddd", "#fce9db", "#ead2ac", "#e6b89c", "#c99789")) +
  #scale_x_discrete(limits=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) +  ## If you want to change the order of the levels in the x axis and only choose some
  labs(title = "C. mediterranea: Cala Estreta")+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y.left = element_text(vjust= 1, hjust=0.5, size=15),
        axis.title.y.right = element_text(vjust= 2, hjust=0.5, size=15),
        axis.title.x = element_text(vjust= -0.5, size=15),
        axis.text.y = element_text(size=15, face="bold"),
        axis.text.x = element_text(size=8, angle = 90, vjust = 0.4), 
        legend.title = element_text(size=15, face="bold"),
        legend.text = element_text(size=15), 
        legend.position = "bottom",
        plot.title = element_text(size=18, face="bold", vjust=2, hjust=0.5))

print(mediterranea_estreta)

ggsave("Fertility_mediterranea_Estreta.pdf", mediterranea_estreta, width=20, height=15, units= "cm")


#########Elegans#######
### Read the data

Fertility_elegans <- read_excel("Fertility_elegans.xlsx")
Fertility_elegans$Month <- factor(Fertility_elegans$Month, levels=c("January", "February", "March", "April", "May", "June","July", "August", "September", "October", "November", "December"))

#Fertility<-read.csv2("Crinita fertility.csv", header=T)
## Crinita<-read.csv2("Crinita_Survival_Mod.csv", header=T) Data Jana
summary(Fertility_elegans)
str(Fertility_elegans)
?str()

### Bar plot
elegans <-ggplot(Fertility_elegans, aes(x=Month, y=Percentage, fill=Fertility)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Year, ncol=3) +
  geom_path(aes(y=Temperature/0.3, group=1), size=1, color="Dark red") +
  scale_y_continuous(name = c(expression(paste(bold("Fertile individuals (%)")))), limit=c(0,100), breaks=seq(0,100,20), sec.axis = sec_axis(~.*0.3, name="Temperature")) +
  ## scale_fill_viridis(discrete = T) +
  scale_fill_manual(values = c("#dddddd", "#fce9db", "#ead2ac", "#e6b89c", "#c99789")) +
  #scale_x_discrete(limits=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) +  ## If you want to change the order of the levels in the x axis and only choose some
  labs(title = "C. elegans: Port de la Selva")+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y.left = element_text(vjust= 1, hjust=0.5, size=15),
        axis.title.y.right = element_text(vjust= 2, hjust=0.5, size=15),
        axis.title.x = element_text(vjust= -0.5, size=15),
        axis.text.y = element_text(size=15, face="bold"),
        axis.text.x = element_text(size=8, angle = 90, vjust = 0.4), 
        legend.title = element_text(size=15, face="bold"),
        legend.text = element_text(size=15), 
        legend.position = "bottom",
        plot.title = element_text(size=18, face="bold", vjust=2, hjust=0.5))

print(elegans)

ggsave("Fertility_elegans.pdf", elegans, width=20, height=15, units= "cm")


#########Barbata#######
### Read the data

Fertility_barbata <- read_excel("Fertility_barbata.xlsx")
Fertility_barbata$Month <- factor(Fertility_barbata$Month, levels=c("January", "February", "March", "April", "May", "June","July", "August", "September", "October", "November", "December"))
Fertility_barbata$Percentage = as.numeric(Fertility_barbata$Percentage)

#Fertility<-read.csv2("Crinita fertility.csv", header=T)
## Crinita<-read.csv2("Crinita_Survival_Mod.csv", header=T) Data Jana
summary(Fertility_barbata)
str(Fertility_barbata)
?str()

### Bar plot: Chioggia
barbata_chioggia <-ggplot(Fertility_barbata[Fertility_barbata$Location == "Chioggia",], aes(x=Month, y=Percentage, fill=Fertility)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Year, ncol=3) +
  geom_path(aes(y=Temperature/0.3, group=1), size=1, color="Dark red") +
  scale_y_continuous(name = c(expression(paste(bold("Fertile individuals (%)")))), limit=c(0,100), breaks=seq(0,100,20), sec.axis = sec_axis(~.*0.3, name="Temperature")) +
  ## scale_fill_viridis(discrete = T) +
  scale_fill_manual(values = c("#dddddd", "#fce9db", "#ead2ac", "#e6b89c", "#c99789")) +
  #scale_x_discrete(limits=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) +  ## If you want to change the order of the levels in the x axis and only choose some
  labs(title = "G. barbata: Chioggia")+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y.left = element_text(vjust= 1, hjust=0.5, size=15),
        axis.title.y.right = element_text(vjust= 2, hjust=0.5, size=15),
        axis.title.x = element_text(vjust= -0.5, size=15),
        axis.text.y = element_text(size=15, face="bold"),
        axis.text.x = element_text(size=8, angle = 90, vjust = 0.4), 
        legend.title = element_text(size=15, face="bold"),
        legend.text = element_text(size=15), 
        legend.position = "bottom",
        plot.title = element_text(size=18, face="bold", vjust=2, hjust=0.5))

print(barbata_chioggia)

ggsave("Fertility_barbata_Chioggia.pdf", barbata_chioggia, width=20, height=15, units= "cm")

### Bar plot: Piscinetta del Passetto (Ancona)
barbata_piscinetta <-ggplot(Fertility_barbata[Fertility_barbata$Location == "Piscinetta del Passetto (Ancona)",], aes(x=Month, y=Percentage, fill=Fertility)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Year, ncol=3) +
  geom_path(aes(y=Temperature/0.3, group=1), size=1, color="Dark red") +
  scale_y_continuous(name = c(expression(paste(bold("Fertile individuals (%)")))), limit=c(0,100), breaks=seq(0,100,20), sec.axis = sec_axis(~.*0.3, name="Temperature")) +
  ## scale_fill_viridis(discrete = T) +
  scale_fill_manual(values = c("#dddddd", "#fce9db", "#ead2ac", "#e6b89c", "#c99789")) +
  #scale_x_discrete(limits=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) +  ## If you want to change the order of the levels in the x axis and only choose some
  labs(title = "G. barbata: Piscinetta del Passetto (Ancona)")+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y.left = element_text(vjust= 1, hjust=0.5, size=15),
        axis.title.y.right = element_text(vjust= 2, hjust=0.5, size=15),
        axis.title.x = element_text(vjust= -0.5, size=15),
        axis.text.y = element_text(size=15, face="bold"),
        axis.text.x = element_text(size=8, angle = 90, vjust = 0.4), 
        legend.title = element_text(size=15, face="bold"),
        legend.text = element_text(size=15), 
        legend.position = "bottom",
        plot.title = element_text(size=18, face="bold", vjust=2, hjust=0.5))

print(barbata_piscinetta)

ggsave("Fertility_barbata_Piscinetta.pdf", barbata_piscinetta, width=20, height=15, units= "cm")

### Bar plot: Scalinata del Passetto di Ancona
barbata_scalinata <-ggplot(Fertility_barbata[Fertility_barbata$Location == "Scalinata del Passetto di Ancona",], aes(x=Month, y=Percentage, fill=Fertility)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Year, ncol=3) +
  geom_path(aes(y=Temperature/0.3, group=1), size=1, color="Dark red") +
  scale_y_continuous(name = c(expression(paste(bold("Fertile individuals (%)")))), limit=c(0,100), breaks=seq(0,100,20), sec.axis = sec_axis(~.*0.3, name="Temperature")) +
  ## scale_fill_viridis(discrete = T) +
  scale_fill_manual(values = c("#dddddd", "#fce9db", "#ead2ac", "#e6b89c", "#c99789")) +
  #scale_x_discrete(limits=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) +  ## If you want to change the order of the levels in the x axis and only choose some
  labs(title = "G. barbata: Scalinata del Passetto (Ancona)")+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y.left = element_text(vjust= 1, hjust=0.5, size=15),
        axis.title.y.right = element_text(vjust= 2, hjust=0.5, size=15),
        axis.title.x = element_text(vjust= -0.5, size=15),
        axis.text.y = element_text(size=15, face="bold"),
        axis.text.x = element_text(size=8, angle = 90, vjust = 0.4), 
        legend.title = element_text(size=15, face="bold"),
        legend.text = element_text(size=15), 
        legend.position = "bottom",
        plot.title = element_text(size=18, face="bold", vjust=2, hjust=0.5))

print(barbata_scalinata)

ggsave("Fertility_barbata_Scalinata.pdf", barbata_scalinata, width=20, height=15, units= "cm")

### Bar plot: Scalaccia Nord
barbata_scalaccia <-ggplot(Fertility_barbata[Fertility_barbata$Location == "Scalaccia Nord",], aes(x=Month, y=Percentage, fill=Fertility)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Year, ncol=3) +
  geom_path(aes(y=Temperature/0.3, group=1), size=1, color="Dark red") +
  scale_y_continuous(name = c(expression(paste(bold("Fertile individuals (%)")))), limit=c(0,100), breaks=seq(0,100,20), sec.axis = sec_axis(~.*0.3, name="Temperature")) +
  ## scale_fill_viridis(discrete = T) +
  scale_fill_manual(values = c("#dddddd", "#fce9db", "#ead2ac", "#e6b89c", "#c99789")) +
  #scale_x_discrete(limits=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) +  ## If you want to change the order of the levels in the x axis and only choose some
  labs(title = "G. barbata: Scalaccia Nord")+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y.left = element_text(vjust= 1, hjust=0.5, size=15),
        axis.title.y.right = element_text(vjust= 2, hjust=0.5, size=15),
        axis.title.x = element_text(vjust= -0.5, size=15),
        axis.text.y = element_text(size=15, face="bold"),
        axis.text.x = element_text(size=8, angle = 90, vjust = 0.4), 
        legend.title = element_text(size=15, face="bold"),
        legend.text = element_text(size=15), 
        legend.position = "bottom",
        plot.title = element_text(size=18, face="bold", vjust=2, hjust=0.5))

print(barbata_scalaccia)

ggsave("Fertility_barbata_Scalaccia.pdf", barbata_scalaccia, width=20, height=15, units= "cm")

### Bar plot: Taranto
barbata_taranto <-ggplot(Fertility_barbata[Fertility_barbata$Location == "Taranto",], aes(x=Month, y=Percentage, fill=Fertility)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Year, ncol=3) +
  geom_path(aes(y=Temperature/0.3, group=1), size=1, color="Dark red") +
  scale_y_continuous(name = c(expression(paste(bold("Fertile individuals (%)")))), limit=c(0,100), breaks=seq(0,100,20), sec.axis = sec_axis(~.*0.3, name="Temperature")) +
  ## scale_fill_viridis(discrete = T) +
  scale_fill_manual(values = c("#dddddd", "#fce9db", "#ead2ac", "#e6b89c", "#c99789")) +
  #scale_x_discrete(limits=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) +  ## If you want to change the order of the levels in the x axis and only choose some
  labs(title = "G. barbata: Taranto")+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y.left = element_text(vjust= 1, hjust=0.5, size=15),
        axis.title.y.right = element_text(vjust= 2, hjust=0.5, size=15),
        axis.title.x = element_text(vjust= -0.5, size=15),
        axis.text.y = element_text(size=15, face="bold"),
        axis.text.x = element_text(size=8, angle = 90, vjust = 0.4), 
        legend.title = element_text(size=15, face="bold"),
        legend.text = element_text(size=15), 
        legend.position = "bottom",
        plot.title = element_text(size=18, face="bold", vjust=2, hjust=0.5))

print(barbata_taranto)

ggsave("Fertility_barbata_Taranto.pdf", barbata_taranto, width=20, height=15, units= "cm")

### Bar plot: Sta. Marguerite
barbata_marguerite <-ggplot(Fertility_barbata[Fertility_barbata$Location == "Sta. Marguerite",], aes(x=Month, y=Percentage, fill=Fertility)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Year, ncol=3) +
  geom_path(aes(y=Temperature/0.3, group=1), size=1, color="Dark red") +
  scale_y_continuous(name = c(expression(paste(bold("Fertile individuals (%)")))), limit=c(0,100), breaks=seq(0,100,20), sec.axis = sec_axis(~.*0.3, name="Temperature")) +
  ## scale_fill_viridis(discrete = T) +
  scale_fill_manual(values = c("#dddddd", "#fce9db", "#ead2ac", "#e6b89c", "#c99789")) +
  #scale_x_discrete(limits=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) +  ## If you want to change the order of the levels in the x axis and only choose some
  labs(title = "G. barbata: Sta. Marguerite")+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y.left = element_text(vjust= 1, hjust=0.5, size=15),
        axis.title.y.right = element_text(vjust= 2, hjust=0.5, size=15),
        axis.title.x = element_text(vjust= -0.5, size=15),
        axis.text.y = element_text(size=15, face="bold"),
        axis.text.x = element_text(size=8, angle = 90, vjust = 0.4), 
        legend.title = element_text(size=15, face="bold"),
        legend.text = element_text(size=15), 
        legend.position = "bottom",
        plot.title = element_text(size=18, face="bold", vjust=2, hjust=0.5))

print(barbata_marguerite)

ggsave("Fertility_barbata_Marguerite.pdf", barbata_marguerite, width=20, height=15, units= "cm")

### Bar plot: Eleochori 
barbata_elechori <-ggplot(Fertility_barbata[Fertility_barbata$Location == "Eleochori",], aes(x=Month, y=Percentage, fill=Fertility)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Year, ncol=3) +
  geom_path(aes(y=Temperature/0.3, group=1), size=1, color="Dark red") +
  scale_y_continuous(name = c(expression(paste(bold("Fertile individuals (%)")))), limit=c(0,100), breaks=seq(0,100,20), sec.axis = sec_axis(~.*0.3, name="Temperature")) +
  ## scale_fill_viridis(discrete = T) +
  scale_fill_manual(values = c("#dddddd", "#fce9db", "#ead2ac", "#e6b89c", "#c99789")) +
  #scale_x_discrete(limits=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) +  ## If you want to change the order of the levels in the x axis and only choose some
  labs(title = "G. barbata: Eleochori (Greece)")+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y.left = element_text(vjust= 1, hjust=0.5, size=15),
        axis.title.y.right = element_text(vjust= 2, hjust=0.5, size=15),
        axis.title.x = element_text(vjust= -0.5, size=15),
        axis.text.y = element_text(size=15, face="bold"),
        axis.text.x = element_text(size=8, angle = 90, vjust = 0.4), 
        legend.title = element_text(size=15, face="bold"),
        legend.text = element_text(size=15), 
        legend.position = "bottom",
        plot.title = element_text(size=18, face="bold", vjust=2, hjust=0.5));barbata_elechori

print(barbata_elechori)

ggsave("Fertility_barbata_Elechori.pdf", barbata_elechori, width=20, height=15, units= "cm")
