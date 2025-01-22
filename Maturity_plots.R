################################3
########## REPRODUCTIVE PHENOLOGY
library(readxl)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggpubr)
library(car)
library(viridisLite)
library(viridis)
library(tidyverse)


### Read the data

Fertility<-read.csv2("Crinita fertility.csv", header=T)
## Crinita<-read.csv2("Crinita_Survival_Mod.csv", header=T) Data Jana
summary(Fertility)
str(Fertility)
?str()


### Bar plot
ggplot(Fertility, aes(x=Month, y=Percentage, fill=Fertility)) + 
  geom_bar(position="stack", stat="identity") +
  geom_path(aes(y=Temperature/0.3, group=1), size=1, color="Dark red") +
  scale_y_continuous(name = c(expression(paste(bold("Fertile individuals (%)")))), limit=c(0,100), breaks=seq(0,100,20), sec.axis = sec_axis(~.*0.3, name="Temperature")) +
  ## scale_fill_viridis(discrete = T) +
  scale_fill_manual(values = c("#dddddd", "#fce9db", "#ead2ac", "#e6b89c", "#c99789")) +
  scale_x_discrete(limits=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) +  ## If you want to change the order of the levels in the x axis and only choose some
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =element_line(size = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 1.8, size=20),
        axis.title.x = element_text(vjust= -0.5, size=20),
        axis.text.y = element_text(size=18, face="bold"),
        axis.text.x = element_text(size=18, face="bold"), 
        legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=18), 
        legend.position = "bottom",
        plot.title = element_text(size=20, face="bold", hjust=0.5))



###################################
###### PIE CHARTS FOR MATURITY ####


Maturity <- read_excel("All maturity.xlsx")
summary(Maturity)
str(Maturity)
Maturity$Month <- factor(Maturity$Month, levels=c("January", "February", "March", "April", "May", "June","July", "August", "September", "October", "November", "December"))
Maturity$Maturity <- factor(Maturity$Maturity, levels=c("Immature", "Medium", "Mature"))

m_barbata <- Maturity[Maturity$Species == "Barbata",]
m_brachycarpa <- Maturity[Maturity$Species == "Brachycarpa",]
m_compressa <- Maturity[Maturity$Species == "Compressa",]
m_crinita <- Maturity[Maturity$Species == "Crinita",]
m_elegans <- Maturity[Maturity$Species == "Elegans",]
m_mediterranea <- Maturity[Maturity$Species == "Mediterranea",]
m_montagnei <- Maturity[Maturity$Species == "Montagnei",]

#BARBATA

#Piscinetta Ancona

barbata_Piscinetta <-ggplot(m_barbata[m_barbata$Location == "Piscinetta Ancona",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Piscinetta del Passetto di Ancona")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(barbata_Piscinetta)

ggsave("Maturity_barbata_Piscinetta.pdf", barbata_Piscinetta, width=15, height=20, units= "cm")

#Sante Marguerite

barbata_Marguerite <-ggplot(m_barbata[m_barbata$Location == "Sante Marguerite",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Sante Marguerite")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(barbata_Marguerite)

ggsave("Maturity_barbata_Marguerite.pdf", barbata_Marguerite, width=15, height=20, units= "cm")

#Scalaccia di Nord

barbata_Scalaccia <-ggplot(m_barbata[m_barbata$Location == "Scalaccia Nord",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Scalaccia di Nord")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(barbata_Scalaccia)

ggsave("Maturity_barbata_Scalaccia.pdf", barbata_Scalaccia, width=15, height=20, units= "cm")

#Scalinata Ancona

barbata_Scalinata <-ggplot(m_barbata[m_barbata$Location == "Scalinata Ancona",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Scalinata del Passetto di Ancona")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(barbata_Scalinata)

ggsave("Maturity_barbata_Scalinata.pdf", barbata_Scalinata, width=15, height=20, units= "cm")

#Scuza

barbata_Scuza <-ggplot(m_barbata[m_barbata$Location == "Scuza",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Scuza")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(barbata_Scuza)

ggsave("Maturity_barbata_Scuza.pdf", barbata_Scuza, width=15, height=20, units= "cm")

#Taranto

barbata_Taranto <-ggplot(m_barbata[m_barbata$Location == "Taranto",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Taranto")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(barbata_Taranto)

ggsave("Maturity_barbata_Taranto.pdf", barbata_Taranto, width=15, height=20, units= "cm")

#BRACHYCARPA

#Plage des Ondes

brachycarpa_Plage <-ggplot(m_brachycarpa[m_brachycarpa$Location == "Plage des Ondes",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Plage des Ondes")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(brachycarpa_Plage)

ggsave("Maturity_brachycarpa_Plage.pdf", brachycarpa_Plage, width=15, height=20, units= "cm")

#COMPRESSA

#Cala Estreta

compressa_Estreta <-ggplot(m_compressa[m_compressa$Location == "Cala Estreta",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Cala Estreta")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(compressa_Estreta)

ggsave("Maturity_compressa_Estreta.pdf", compressa_Estreta, width=15, height=20, units= "cm")

#Calella

compressa_Calella <-ggplot(m_compressa[m_compressa$Location == "Calella",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Calella de Palafrugell")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(compressa_Calella)

ggsave("Maturity_compressa_Calella.pdf", compressa_Calella, width=15, height=20, units= "cm")


#Roques planes

compressa_Roques <-ggplot(m_compressa[m_compressa$Location == "Roques Planes",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Roques Planes")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(compressa_Roques)

ggsave("Maturity_compressa_Roques.pdf", compressa_Calella, width=15, height=20, units= "cm")

#CRINITA

#Cala Estreta

crinita_Estreta <-ggplot(m_crinita[m_crinita$Location == "Cala Estreta",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Cala Estreta")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(crinita_Estreta)

ggsave("Maturity_crinita_Estreta.pdf", crinita_Estreta, width=15, height=20, units= "cm")

#Frigole

crinita_Frigole <-ggplot(m_crinita[m_crinita$Location == "Frigole",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Frigole")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(crinita_Frigole)

ggsave("Maturity_crinita_Frigole.pdf", crinita_Frigole, width=15, height=20, units= "cm")

#Port de la Selva

crinita_Port <-ggplot(m_crinita[m_crinita$Location == "Port de la Selva",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Port de la Selva")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(crinita_Port)

ggsave("Maturity_crinita_Port.pdf", crinita_Port, width=15, height=20, units= "cm")

#Rimel

crinita_Rimel <-ggplot(m_crinita[m_crinita$Location == "Rimel",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Rimel")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(crinita_Rimel)

ggsave("Maturity_crinita_Rimel.pdf", crinita_Rimel, width=15, height=20, units= "cm")

#Sante Marguerite

crinita_Marguerite <-ggplot(m_crinita[m_crinita$Location == "Sante Marguerite",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Sante Marguerite")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(crinita_Marguerite)

ggsave("Maturity_crinita_Marguerite.pdf", crinita_Marguerite, width=15, height=20, units= "cm")

#Scannella
crinita_Scannella <-ggplot(m_crinita[m_crinita$Location == "Scannella",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Scannella")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(crinita_Scannella)

ggsave("Maturity_crinita_Scannella.pdf", crinita_Scannella, width=15, height=20, units= "cm")

#Elegans 

#Port de la Selva

elegans_Port <-ggplot(m_elegans[m_elegans$Location == "Port de la Selva",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Port de la Selva")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(elegans_Port)

ggsave("Maturity_elegans_Port.pdf", crinita_Port, width=15, height=20, units= "cm")

#MEDITERRANEA 

#Cala Estreta

mediterranea_Estreta <-ggplot(m_mediterranea[m_mediterranea$Location == "Cala Estreta",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Cala Estreta")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(mediterranea_Estreta)

ggsave("Maturity_mediterranea_Estreta.pdf", mediterranea_Estreta, width=15, height=20, units= "cm")

#Cap des trois fourches

mediterranea_Fourches <-ggplot(m_mediterranea[m_mediterranea$Location == "Cap des trois fourches",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Cap des trois fourches")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(mediterranea_Fourches)

ggsave("Maturity_mediterranea_Fourches.pdf", mediterranea_Fourches, width=15, height=20, units= "cm")

#MONTAGNEI

#Anse des Fosses

montagnei_Anse <-ggplot(m_montagnei[m_montagnei$Location == "Anse des Fosses",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Anse des Fosses")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(montagnei_Anse)

ggsave("Maturity_montagnei_Anse.pdf", montagnei_Anse, width=15, height=20, units= "cm")

#Plage des Ondes

montagnei_Plage <-ggplot(m_montagnei[m_montagnei$Location == "Plage des Ondes",], aes(x = "", y = Percentage, fill=Maturity)) +
  geom_bar(stat = "identity") +
  facet_wrap(Year~Month) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette=4) +
  ##scale_fill_manual(values = c(my_palette_41)) +
  labs(title = "Plage des Ondes")+
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust=0.05, vjust=5))

print(montagnei_Plage)

ggsave("Maturity_montagnei_Plage.pdf", montagnei_Plage, width=15, height=20, units= "cm")


