library(tidyr)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(RColorBrewer)

#Set working directory to POXC root folder
poxc <- read.csv(file = "Data/June 2021/poxc_complete_2021.csv", header = T)

plot_treat <- read.csv(file = "Data/June 2021/study_treatments_2021.csv", header = T) %>%   
  na.omit() %>% 
  select(plot:treatment)

poxc <- poxc %>% 
  select(site:poxc)

poxc_treat <- merge(poxc, plot_treat)

##Replace F treatment with C since fert has not been applied yet
poxc_treat$treatment <- gsub('F', 'C', poxc_treat$treatment)

##group by site, crop, , plot and treatment and get the outliers of those groups (these are for the technical reps)
poxc.stat <- group_by(poxc_treat, site, treatment, crop, plot) %>%   
  mutate(mean = mean(poxc),
         two.sd = 2*sd(poxc),
         z.score = abs(((poxc-mean(poxc))/sd(poxc))))

poxc_clean <- filter(poxc.stat, z.score < 2)

poxc_clean <- group_by(poxc_clean, site, plot, crop, treatment) %>% 
  summarise(mean(poxc)) %>% 
  rename(poxc = 'mean(poxc)')                     ### value of poxc will be the mean of the three technical reps 

#Redo outliers, this time with the average of the technical reps, and grouping by site, treatment, and crop
poxc.stat <- poxc_clean %>% 
  na.omit() %>% 
  group_by(site, treatment, crop) %>%   
  mutate(mean = mean(poxc),
         two.sd = 2*sd(poxc),
         z.score = abs(((poxc-mean(poxc))/sd(poxc))))

poxc_clean <- filter(poxc.stat, z.score < 2)

anova <- aov(poxc ~ site + crop + treatment, data = poxc_clean)
summary(anova)

date <- c("06/1/2021")
poxc_clean$date <- as.Date(date, "%m/%d/%Y")
#write.csv(poxc_clean, "poxc_clean_june_2021.csv", row.names = F) #comment out if needed to rewrite file

#Extra Work
#Plots ___________________________________________________________________________________________________________________________
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #color blind friendly palettes

poxc_treat_crop <- poxc_clean %>% 
  group_by(crop,treatment) %>% 
  summarise(mean_poxc = mean(poxc),
            sd = sd(poxc),
            count=n(),
            se=(sd/(sqrt(count)))) 


p1 <- ggplot(data = poxc_treat_crop, aes(x=treatment, y= mean_poxc, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Bioavailable C by Treatment and Crop June 2021 (sd)",x= "Treatment", y="Bioavailable C (mg/kg of soil)") +
  geom_errorbar(aes(ymin=mean_poxc-sd, ymax=mean_poxc+sd), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  ylim(0,1500) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Biochar", "Control", "Fertilizer"))

p2 <- ggplot(data = poxc_treat_crop, aes(x=treatment, y= mean_poxc, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Bioavailable C by Treatment and Crop June 2021",x= "Treatment", y="Bioavailable C (mg/kg of soil)") +
  geom_errorbar(aes(ymin=mean_poxc-se, ymax=mean_poxc+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  ylim(0,1500) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Biochar", "Control", "Fertilizer")) 
  

grid.arrange(p1, p2, nrow=2, ncol = 1) 

 

#_____________________________________________________________________________

poxc_treat_site <- poxc_clean %>%  
  group_by(site,treatment) %>% 
  summarise(mean_poxc = mean(poxc),
            sd = sd(poxc),
            count=n(),
            se=(sd/(sqrt(count))))

p3 <- ggplot(data = poxc_treat_site, aes(x=site, y= mean_poxc, fill = treatment)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Bioavailable C by Treatment and Site June 2021 (sd)", x= "Site", y="Bioavailable C (mg/kg of soil)") +
  geom_errorbar(aes(ymin=mean_poxc-sd, ymax=mean_poxc+sd), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  ylim(0,1500) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Treatment",
                    labels=c("Biochar","Control","Fertilizer")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Goshen Road", "Jackson's Mill","Reedsville"))

p4 <- ggplot(data = poxc_treat_site, aes(x=site, y= mean_poxc, fill = treatment)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Bioavailable C by Treatment and Site June 2021", x= "Site", y="Bioavailable C (mg/kg of soil)") +
  geom_errorbar(aes(ymin=mean_poxc-se, ymax=mean_poxc+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  ylim(0,1500) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Treatment",
                    labels=c("Biochar","Control","Fertilizer")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Goshen Road", "Jackson's Mill","Reedsville"))

grid.arrange(p3, p4, nrow=2, ncol = 1)



##________________________________________________________________________________________


poxc_site_crop <- poxc_clean %>% 
  group_by(site,crop) %>% 
  summarise(mean_poxc = mean(poxc),
            sd = sd(poxc),
            count=n(),
            se=(sd/(sqrt(count))))

p5 <- ggplot(data = poxc_site_crop, aes(x=site, y= mean_poxc, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Bioavailable C by Site and Crop June 2021 (sd)", x= "Site", y="Bioavailable C (mg/kg of soil)") +
  geom_errorbar(aes(ymin=mean_poxc-sd, ymax=mean_poxc+sd), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  ylim(0,1500) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Goshen ROad", "Jackson's Mill","Reedsville"))

p6 <- ggplot(data = poxc_site_crop, aes(x=site, y= mean_poxc, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Bioavailable C by Site and Crop June 2021", x= "Site", y="Bioavailable C (mg/kg of soil)") +
  geom_errorbar(aes(ymin=mean_poxc-se, ymax=mean_poxc+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  ylim(0,1500) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Goshen Road", "Jackson's Mill","Reedsville"))

grid.arrange(p5, p6, nrow=2, ncol = 1)

#_____________________________________________________________________
ggplot(data = poxc_clean, aes(x=crop, y= poxc)) + 
  geom_boxplot() 

ggplot(data = poxc_clean, aes(x=site, y= poxc)) + 
  geom_boxplot() 

ggplot(data = poxc_clean, aes(x=treatment, y= poxc)) + 
  geom_boxplot()

