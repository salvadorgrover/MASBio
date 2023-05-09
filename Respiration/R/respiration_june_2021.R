library(dplyr)
library(tidyverse)
library(readxl)


respiration_raw <- read_excel("Data/June 2021/finalrespdata_modified.xlsx") %>%  ##load finalrespdata_modified, data from IRGA
  drop_na(co2_int)
plot_treat <- read_excel("Data/June 2021/study_treatments_2021.xlsx")  
soil_resp <- read_excel("Data/June 2021/soil_respiration_data_modified.xlsx")  
  


respiration <- respiration_raw  %>%  
  group_by(plot_no) %>% 
  summarise(max(co2_int)) 

respiration <- merge(respiration, plot_treat) %>% 
  mutate(mol_co2 = ((`max(co2_int)`/1000000)*0.473176)/(293.15*.08205)) %>% 
  mutate(c_g_hr = (mol_co2*12)/72) 

respiration <- soil_resp %>% 
  select(site, plot, 'W1- mass of dry soil') %>% 
  merge(respiration) %>% 
  rename(mass_soil = 'W1- mass of dry soil') %>% 
  mutate(resp = ((c_g_hr/mass_soil)*1000000))     ##resp values are g of CO2 per hour per g dry soil



zscore <- respiration %>% 
  group_by(site, treatment, crop) %>%   
  mutate(mean = mean(resp),
         two.sd = 2*sd(resp),
         zscore = abs(((resp-mean(resp))/sd(resp))))


resp.clean <- filter(zscore, zscore < 2)

anova <- aov(resp ~ site + crop + treatment, data = resp.clean)
summary(anova)

resp.clean$site <- gsub("lp_mine", "goshen_road", resp.clean$site)

write.csv(resp.clean, "resp_clean_june_2021.csv", row.names = F)
##Plots_________________________________________________________________________________________________________________________________________________
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #color blind friendly palettes

respiration_treat_crop <- resp.clean %>% 
  group_by(crop,treatment) %>% 
  summarise(mean_respiration = mean(resp),
            sd = sd(resp),
            count=n(),
            se=(sd/(sqrt(count)))) 


ggplot(data = respiration_treat_crop, aes(x=treatment, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Treatment and Crop June 2021 (sd)",x= "Treatment", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-sd, ymax=mean_respiration+sd), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Treatment",
                    labels=c("Biochar","Control","Fertilizer")) +
  scale_x_discrete(labels = c("Biochar", "Control", "Fertilizer"))


ggplot(data = respiration_treat_crop, aes(x=treatment, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Treatment and Crop June 2021 - 72 hours", x= "Treatment", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-se, ymax=mean_respiration+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Treatment",
                    labels=c("Biochar","Control","Fertilizer")) +
  scale_x_discrete(labels = c("Biochar", "Control", "Fertilizer"))


##_____________________________________________________________________________________________________________________________________________

respiration_treat_site <- resp.clean %>%  
  group_by(site,treatment) %>% 
  summarise(mean_respiration = mean(resp),
            sd = sd(resp),
            count=n(),
            se=(sd/(sqrt(count))))

 
ggplot(data = respiration_treat_site, aes(x=site, y= mean_respiration, fill = treatment)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Site and Treatment June 2021 (sd)", x= "Site", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-sd, ymax=mean_respiration+sd), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Treatment",
                    labels=c("Biochar","Control","Fertilizer")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))

ggplot(data = respiration_treat_site, aes(x=site, y= mean_respiration, fill = treatment)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Site and Treatment June 2021 - 72 hours", x= "Site", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-se, ymax=mean_respiration+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Treatment",
                    labels=c("Biochar","Control","Fertilizer")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))



#________________________________________________________________________________________________________________________________
  

respiration_site_crop <- resp.clean %>% 
  group_by(site,crop) %>% 
  summarise(mean_respiration = mean(resp),
            sd = sd(resp),
            count=n(),
            se=(sd/(sqrt(count))))

ggplot(data = respiration_site_crop, aes(x=site, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Site and Crop June 2021 (sd)", x= "Site", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-sd, ymax=mean_respiration+sd), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))

ggplot(data = respiration_site_crop, aes(x=site, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Site and Crop June 2021 - 72 hours", x= "Site", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-se, ymax=mean_respiration+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))



###_____________________________________________________________________________________________________
ggplot(data = resp.clean, aes(x=crop, y= resp)) + 
  geom_boxplot()
ggplot(data = resp.clean, aes(x=treatment, y= resp)) + 
  geom_boxplot()
ggplot(data =resp.clean, aes(x=site, y= resp)) + 
  geom_boxplot()
