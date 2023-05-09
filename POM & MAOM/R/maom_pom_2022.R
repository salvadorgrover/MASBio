# Script for the analysis of POM and MAOM samples from August 2022 
#Salvador Grover
#Jan. 26, 2023

library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)

sample_weight <- read_excel("Data/August 2022/maom_pom_weights_aug_22.xlsx") %>% 
  select(site:'POM + MAOM') %>% 
  rename(MAOM = maom_g,
         POM = pom_g) %>% 
  pivot_longer(cols = MAOM:POM, 
               names_to = "fraction",
               values_to = "fraction_weight_g")

plot_treat <- read.csv("Data/August 2022/study_treatments_2022.csv") 

pom.maom <- merge(sample_weight, plot_treat)

pom.maom$ratio <- (pom.maom$fraction_weight_g)/(pom.maom$`POM + MAOM`)





#c_n <- read.csv(file = "Data/June 2021/maom_pom_cn.csv", header = T) %>% 
#  filter(is.na(Notes)) %>% 
#  select(c(site:percent_c))
#pom.maom <- merge(c_n, sample_weight) %>% 
#  merge(plot_treat) 

pom.maom$treatment <- gsub('F', 'C', pom.maom$treatment)



pom.maom$site <- gsub("agronomy_farm", "Agronomy Farm", pom.maom$site)
pom.maom$site <- gsub("allstar_mine_1", "Allstar Mine 1", pom.maom$site)
pom.maom$site <- gsub("allstar_mine_2", "Allstar Mine 2", pom.maom$site)
pom.maom$site <- gsub("jackson_mill", "Jackson Mill", pom.maom$site)
pom.maom$site <- gsub("goshen", "Goshen Road", pom.maom$site)
pom.maom$site <- gsub("reedsville", "Reedsville", pom.maom$site)

offsite <- sample_weight %>% 
  subset(plot %in% c("offsite 1", "offsite 2", "offsite 3")) %>% 
  drop_na(fraction_weight_g)

offsite$site <- gsub("allstar_mine_1", "Allstar Mine 1", offsite$site)
offsite$site <- gsub("allstar_mine_2", "Allstar Mine 2", offsite$site)
offsite$site <- gsub("jackson_mill", "Jackson Mill", offsite$site)
offsite$site <- gsub("goshen", "Goshen Road", offsite$site)


#write.csv(pom.maom, file = "pom_maom_2022.csv", row.names = F)
#write.csv(offsite, file = "pom_maom_offsite_2022.csv", row.names = F)

###Weight plots__________________________________________________________________________

crops <- c("Switchgrass", "Willow")
names(crops) <- c("S","W")

ggplot(data = pom.maom, aes(x = treatment, y = fraction_weight_g, color = fraction)) +
  geom_boxplot() +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) + 
  facet_grid( ~ crop)


ggplot(data = pom.maom, aes(x = treatment, y = fraction_weight_g, color = fraction)) +
  geom_boxplot() +
  geom_boxplot(alpha = .2) +
  facet_grid(site ~ crop, labeller = labeller(crop = crops))



#offsite weights
ggplot(data = offsite, aes(x = site, y = fraction_weight_g, color = fraction)) +
  geom_boxplot() +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) 


##combining offsite and onsite
offsite <- offsite %>% 
  mutate(land_type = if_else(site %in% c("Agronomy Farm", "Reedsville", "Jackson Mill"), "Agriculture", "Mine")) %>% 
  select(-plot) 

offsite$plot_type <- c("offsite")

pom.maom_new <- pom.maom %>% 
  mutate(land_type = if_else(site %in% c("Agronomy Farm", "Reedsville", "Jackson Mill"), "Agriculture", "Mine")) %>% 
  select(-plot, -crop, -treatment, -ratio)

pom.maom_new$plot_type <- c("onsite")

pom.maom.off.on <- rbind(pom.maom_new, offsite)

pom.maom.off.on <- pom.maom.off.on %>% 
  subset(site %in% c("Allstar Mine 1", "Allstar Mine 2", "Goshen Road", "Jackson Mill"))

anova <- aov(fraction_weight_g ~ site * fraction * plot_type, data = pom.maom.off.on)
summary(anova)

pom.off.on <- pom.maom.off.on %>% 
  subset(fraction %in% c("POM"))

anova <- aov(fraction_weight_g ~ site * plot_type, data = pom.off.on)
summary(anova)
TukeyHSD(aov(fraction_weight_g ~ site * plot_type, data = pom.off.on), conf.level = .95)

maom.off.on <- pom.maom.off.on %>% 
  subset(fraction %in% c("MAOM"))

anova <- aov(fraction_weight_g ~ site * plot_type, data = maom.off.on)
summary(anova)

ggplot(data = pom.maom.off.on, aes(x = plot_type, y = fraction_weight_g, color = fraction)) +
  geom_boxplot() +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) +
  facet_grid(fraction ~ site) +
  labs(x = "Offsite or Onsite", y = "Weight (g)", title = "August 2022 Fraction Weights for Offsite and Onsite plots")

