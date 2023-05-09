# MAOM and POM weight data
library(dplyr)
library(tidyverse)
library(readxl)
library(plotrix)
library(ggsignif)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #color blind friendly palettes


sample_weight <- read_excel("Data/June 2021/pom_maom_weights_new.xlsx") %>% 
  select(1:6) %>% 
  #rename(MAOM = maom_g,
   #      POM = pom_g) %>% 
  pivot_longer(cols = MAOM:POM, 
               names_to = "fraction",
               values_to = "fraction_weight_g")
plot_treat <- read_excel("Data/June 2021/study_treatments_2021.xlsx") 
c_n <- read.csv(file = "Data/June 2021/maom_pom_cn.csv", header = T) %>% 
  filter(is.na(Notes)) %>% 
  dplyr::select(c(site:percent_c))
pom.maom <- merge(c_n, sample_weight) %>% 
  merge(plot_treat) 

pom.maom$treatment <- gsub('F', 'C', pom.maom$treatment)

pom.maom$ratio <- (pom.maom$fraction_weight_g)/(pom.maom$`POM + MAOM`)

anova.n.1 <- aov(percent_n ~ fraction, data = pom.maom)
summary(anova.n.1)

anova.n.3 <- aov(percent_n ~ fraction + crop + treatment, data = pom.maom)
summary(anova.n.3)

anova.n.4 <- aov(percent_n ~ fraction + crop + treatment + site, data = pom.maom)
summary(anova.n.4)

anova.c.1 <- aov(percent_c ~ fraction, data = pom.maom)
summary(anova.c.1)

anova.c.3 <- aov(percent_c ~ fraction + crop + treatment, data = pom.maom)
summary(anova.c.3)

anova.c.4 <- aov(percent_c ~ fraction + crop + treatment + site, data = pom.maom)
summary(anova.c.4)



anova_pom <- aov(pom_g ~ site + crop + treatment, data = sample_weight)
summary(anova_pom)

anova_maom <- aov(maom_g ~ site + crop + treatment, data = sample_weight)
summary(anova_maom)

write.csv(pom.maom, file = "pom_maom_2021.csv", row.names = F )

#Weights_____________________________________________________________________________________________________________________________________________________
#NEED TO CHANGE VARIABLES TO MAKE PLOTS WORK

ggplot(data = pom.maom, aes(x = treatment, y = fraction_weight_g, color = fraction)) +
  geom_boxplot() +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) + 
  facet_grid( ~ crop)

crops <- c("Switchgrass", "Willow")
names(crops) <- c("S","W")

ggplot(data = pom.maom, aes(x = treatment, y = fraction_weight_g, color = fraction)) +
  geom_boxplot() +
  geom_boxplot(alpha = .2) +
  facet_grid(site ~ crop, labeller = labeller(crop = crops))

## Site and Crop - MaOM 
maom_site_crop <- sample_weight %>% 
  select(site, crop, maom_g, subsample_weight) %>% 
  mutate(maom = maom_g/subsample_weight) %>%  ##MaOM presented as a ratio to total soil 
  group_by(site, crop) %>% 
  summarise(mean_maom = mean(maom),
            sd = sd(maom),
            count=n(),
            se=(sd/(sqrt(count))))

ggplot(data = maom_site_crop,
       aes(x = site, y = mean_maom, fill = crop)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin= mean_maom - se, ymax= mean_maom + se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  theme_classic() +
  labs(title="MaOM Sample Weight Site and Crop June 2021", x= "Site", y="Mean MaOM weight (ratio to total soil)") + 
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))

##Site and treatment - MaOM
sample_weight <- merge(sample_weight, plot_treat)
maom_site_treat <- sample_weight %>% 
  select(site, treatment, maom_g, subsample_weight) %>% 
  mutate(maom = maom_g/subsample_weight) %>%
  group_by(site, treatment) %>% 
  summarise(mean_maom = mean(maom),
            sd = sd(maom),
            count=n(),
            se=(sd/(sqrt(count))))

ggplot(data = maom_site_treat,
       aes(x = site, y = mean_maom, fill = treatment)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin= mean_maom - se, ymax= mean_maom + se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  theme_classic() +
  labs(title="MaOM Sample Weight Site and Treatment June 2021", x= "Site", y="Mean MaOM weight (ratio to total soil)") + 
  scale_fill_manual(values=cbPalette,
                    name = "Treatment",
                   labels=c("Biochar","Control","Fertilizer")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))


##Treatment and crop - MaOM
sample_weight <- merge(sample_weight, plot_treat)
maom_crop_treat <- sample_weight %>% 
  select(crop, treatment, maom_g, subsample_weight) %>% 
  mutate(maom = maom_g/subsample_weight) %>%
  group_by(crop, treatment) %>% 
  summarise(mean_maom = mean(maom),
            sd = sd(maom),
            count=n(),
            se=(sd/(sqrt(count))))

ggplot(data = maom_crop_treat,
       aes(x = treatment, y = mean_maom, fill = crop)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin= mean_maom - se, ymax= mean_maom + se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  theme_classic() +
  labs(title="MaOM Sample Weight Treatment and Crop June 2021", x= "Treatment", y="Mean MaOM weight (ratio to total soil)") + 
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Biochar", "Control", "Fertilizer"))



## Site and Crop - POM
sample_weight <- merge(sample_weight, plot_treat)
pom_site_crop <- sample_weight %>% 
  select(site, crop, pom_g, subsample_weight) %>% 
  mutate(pom = pom_g/subsample_weight) %>%
  group_by(site, crop) %>% 
  summarise(mean_pom = mean(pom),
            sd = sd(pom),
            count=n(),
            se=(sd/(sqrt(count))))

ggplot(data = pom_site_crop,
       aes(x = site, y = mean_pom, fill = crop)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin= mean_pom - se, ymax= mean_pom + se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  theme_classic() +
  labs(title="POM Sample Weight Site and Crop June 2021", x= "Site", y="Mean POM weight (ratio to total soil)") + 
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))

##Site and treatment - POM
sample_weight <- merge(sample_weight, plot_treat)
pom_site_treat <- sample_weight %>% 
  select(site, treatment, pom_g, subsample_weight) %>% 
  mutate(pom = pom_g/subsample_weight) %>%
  group_by(site, treatment) %>% 
  summarise(mean_pom = mean(pom),
            sd = sd(pom),
            count=n(),
            se=(sd/(sqrt(count))))

ggplot(data = pom_site_treat,
       aes(x = site, y = mean_pom, fill = treatment)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin= mean_pom - se, ymax= mean_pom + se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  theme_classic() +
  labs(title="POM Sample Weight Site and Treatment June 2021", x= "Site", y="Mean POM weight (ratio to total soil)") + 
  scale_fill_manual(values=cbPalette,
                    name = "Treatment",
                    labels=c("Biochar","Control","Fertilizer")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))


##Treatment and crop - pom
sample_weight <- merge(sample_weight, plot_treat)
pom_crop_treat <- sample_weight %>% 
  select(crop, treatment, pom_g, subsample_weight) %>% 
  mutate(pom = pom_g/subsample_weight) %>%
  group_by(crop, treatment) %>% 
  summarise(mean_pom = mean(pom),
            sd = sd(pom),
            count=n(),
            se=(sd/(sqrt(count))))

ggplot(data = pom_crop_treat,
       aes(x = treatment, y = mean_pom, fill = crop)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin= mean_pom - se, ymax= mean_pom + se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  theme_classic() +
  labs(title="POM Sample Weight Crop and Treatment June 2021", x= "Treatment", y="Mean POM weight (ratio to total soil)") + 
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Biochar", "Control", "Fertilizer"))

#C and N_____________________________________________________________________________________________________________________________

# Treatment and crop - C POM 
C.t.c <- pom.maom %>% 
  filter(fraction == "POM") %>% 
  group_by(crop, treatment) %>% 
  summarise(mean_c = mean(percent_c),
            sd = sd(percent_c),
            count=n(),
            se=(sd/(sqrt(count))))

ggplot(data = C.t.c,
       aes(x = treatment, y = mean_c, fill = crop)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin= mean_c - se, ymax= mean_c + se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  theme_classic() +
  labs(title="Percent C by Treatment and Crop June 2021", x= "Treatment", y="Percent C") + 
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Biochar", "Control", "Fertilizer")) 

# Treatment and crop - N POM 
N.t.c <- pom.maom %>% 
  filter(fraction == "POM") %>% 
  group_by(crop, treatment) %>% 
  summarise(mean_c = mean(percent_c),
            sd = sd(percent_c),
            count=n(),
            se=(sd/(sqrt(count))))

ggplot(data = C.t.c,
       aes(x = treatment, y = mean_c, fill = crop)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin= mean_c - se, ymax= mean_c + se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  theme_classic() +
  labs(title="Percent C by Treatment and Crop June 2021", x= "Treatment", y="Percent C") + 
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Biochar", "Control", "Fertilizer")) 



