library(dplyr)
library(tidyverse)
library(readxl)
library(gridExtra)

resp_24 <- read.csv("Data/August 2022/aug_22_resp_24hr.csv", header = T)  %>% 
  drop_na(co2_int | plot_no)##load data from IRGA
resp_48 <- read.csv("Data/August 2022/aug_22_resp_48hr.csv", header = T) %>% 
  drop_na(co2_int | plot_no)
resp_72 <- read.csv("Data/August 2022/aug_22_resp_72hr.csv", header =T) %>% 
  drop_na(co2_int | plot_no)

plot_treat <- read_excel("Data/August 2022/study_treatments_2022.xlsx")  ##load study_treatments_2022
soil_resp <- read_excel("Data/August 2022/fhc_moisture_aug_22.xlsx")   ##load fhc_and_soil_resp_2022

##respiration_raw$date <- as.Date(respiration_raw$date, "%d/%m/%y") use if need to group by date as well



resp.24.max <- resp_24  %>%  
  group_by(plot_no) %>% 
  summarise(max(co2_int))

resp.48.max <- resp_48  %>%  
  group_by(plot_no) %>% 
  summarise(max(co2_int))

resp.72.max <- resp_72  %>%  
  group_by(plot_no) %>% 
  summarise(max(co2_int))

m.resp.24 <- merge(resp.24.max, plot_treat) %>% 
  mutate(mol_co2 = ((`max(co2_int)`/1000000)*0.473176)/(293.15*.08205)) %>%  ##change .473 to volume of jar if different
  mutate(c_g_hr = (mol_co2*12)/24) 

m.resp.48 <- merge(resp.48.max, plot_treat) %>% 
  mutate(mol_co2 = ((`max(co2_int)`/1000000)*0.473176)/(293.15*.08205)) %>% 
  mutate(c_g_hr = (mol_co2*12)/48)

m.resp.72 <- merge(resp.72.max, plot_treat) %>% 
  mutate(mol_co2 = ((`max(co2_int)`/1000000)*0.473176)/(293.15*.08205)) %>% 
  mutate(c_g_hr = (mol_co2*12)/72)

m.resp.24 <- soil_resp %>% 
  select(site, plot, w1) %>%  ### w1 is soil mass
  merge(m.resp.24) %>% 
  rename(mass_soil = w1) %>% 
  mutate(resp = ((c_g_hr/mass_soil)*1000000))       ##resp values are ug of CO2 per hour per g dry soil

m.resp.48 <- soil_resp %>% 
  select(site, plot, w1) %>%  
  merge(m.resp.48) %>% 
  rename(mass_soil = w1) %>% 
  mutate(resp = ((c_g_hr/mass_soil)*1000000)) 

m.resp.72 <- soil_resp %>% 
  select(site, plot, w1) %>%  
  merge(m.resp.72) %>% 
  rename(mass_soil = w1) %>% 
  mutate(resp = ((c_g_hr/mass_soil)*1000000)) 


m.resp.24$hour <- c('24')
m.resp.48$hour <- c('48')
m.resp.72$hour <- c('72')

m.resp.24 %>% 
  group_by(site) %>% 
  summarise(mean = mean(resp), n = n())

resp.all <- rbind(m.resp.24, m.resp.48) %>% 
  rbind(m.resp.72)
#drop offsite for analysis
resp.all <- resp.all %>% 
  na.omit()

ggplot(resp.all, aes(x = hour, y = resp, color = plot, group = plot)) +
  geom_point() +
  geom_line() + 
  facet_grid(site ~ .)


zscore.24 <- group_by(m.resp.24, site, treatment, crop) %>%    ##group by site, crop, and treatment and get the outliers of those groups, if any
  mutate(mean = mean(resp),
         two.sd = 2*sd(resp),
         zscore = abs(((resp-mean(resp))/sd(resp))))

zscore.48 <- group_by(m.resp.48, site, treatment, crop) %>%   
  mutate(mean = mean(resp),
         two.sd = 2*sd(resp),
         zscore = abs(((resp-mean(resp))/sd(resp))))

zscore.72 <- group_by(m.resp.72, site, treatment, crop) %>%    
  mutate(mean = mean(resp),
         two.sd = 2*sd(resp),
         zscore = abs(((resp-mean(resp))/sd(resp))))

resp.24.clean <- filter(zscore.24, zscore < 2)
resp.48.clean <- filter(zscore.48, zscore < 2)
resp.72.clean <- filter(zscore.72, zscore < 2)



resp.24.treat <- resp.24.clean %>%  
  group_by(resp,site,plot,crop,treatment) %>% 
  summarise(mean(resp)) %>% 
  rename(mean_respiration = 'mean(resp)')

resp.48.treat <- resp.48.clean %>%  
  group_by(resp,site,plot,crop,treatment) %>% 
  summarise(mean(resp)) %>% 
  rename(mean_respiration = 'mean(resp)') 

resp.72.treat <- resp.72.clean %>%  
  group_by(resp,site,plot,crop,treatment) %>% 
  summarise(mean(resp)) %>% 
  rename(mean_respiration = 'mean(resp)') 

anova.24 <- aov(resp ~ site + crop + treatment, data = resp.24.clean)
summary(anova.24)

anova.48 <- aov(resp ~ site + crop + treatment, data = resp.48.clean)
summary(anova.48)

anova.72 <- aov(resp ~ site + crop + treatment, data = resp.72.clean)
summary(anova.72)

resp.24.clean$site <- gsub("goshen", "goshen_road", resp.24.clean$site)
resp.48.clean$site <- gsub("goshen", "goshen_road", resp.48.clean$site)
resp.72.clean$site <- gsub("goshen", "goshen_road", resp.72.clean$site)
#write.csv(resp.24.clean, "respiration_clean_24h_august_2022.csv", row.names = F)
#write.csv(resp.48.clean, "respiration_clean_48h_august_2022.csv", row.names = F)
#write.csv(resp.72.clean, "respiration_clean_72h_august_2022.csv", row.names = F)

##offsite 
resp.all.offsite <- rbind(m.resp.24, m.resp.48) %>% 
  rbind(m.resp.72)
offsite <- resp.all.offsite %>% 
  filter(is.na(crop))

write.csv(offsite, "offsite_august_2022_respiration.csv", row.names = F)

##Plots_________________________________________________________________________________________________________________________________________________
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #color blind friendly palettes

resp.24.treat.crop <- resp.24.treat %>% 
  group_by(crop,treatment) %>% 
  summarise(mean_respiration = mean(resp),
            sd = sd(resp),
            count=n(),
            se=(sd/(sqrt(count)))) 

resp.48.treat.crop <- resp.48.treat %>% 
  group_by(crop,treatment) %>% 
  summarise(mean_respiration = mean(resp),
            sd = sd(resp),
            count=n(),
            se=(sd/(sqrt(count)))) 

resp.72.treat.crop <- resp.72.treat %>% 
  group_by(crop,treatment) %>% 
  summarise(mean_respiration = mean(resp),
            sd = sd(resp),
            count=n(),
            se=(sd/(sqrt(count)))) 

p1 <- ggplot(data = resp.24.treat.crop, aes(x=treatment, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Treatment and Crop May 2022(sd) - 24h",x= "Treatment", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-sd, ymax=mean_respiration+sd), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Biochar", "Control", "Fertilizer"))

p2 <- ggplot(data = resp.24.treat.crop, aes(x=treatment, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Treatment and Crop May 2022 - 24h", x= "Treatment", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-se, ymax=mean_respiration+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Biochar", "Control", "Fertilizer"))

p3 <- ggplot(data = resp.48.treat.crop, aes(x=treatment, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Treatment and Crop May 2022 (sd) - 48h", x= "Treatment", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-sd, ymax=mean_respiration+sd), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Biochar", "Control", "Fertilizer"))

p4 <- ggplot(data = resp.48.treat.crop, aes(x=treatment, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Treatment and Crop May 2022 - 48h", x= "Treatment", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-se, ymax=mean_respiration+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Biochar", "Control", "Fertilizer"))

p5 <- ggplot(data = resp.72.treat.crop, aes(x=treatment, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Treatment and Crop May 2022 (sd) - 72h", x= "Treatment", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-sd, ymax=mean_respiration+sd), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Biochar", "Control", "Fertilizer"))

p6 <- ggplot(data = resp.72.treat.crop, aes(x=treatment, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Treatment and Crop May 2022 - 72h", x= "Treatment", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-se, ymax=mean_respiration+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Biochar", "Control", "Fertilizer"))

grid.arrange(p1,p3,p5, nrow = 2, ncol = 2)
grid.arrange(p2,p4,p6, nrow = 2, ncol = 2)


##_____________________________________________________________________________________________________________________________________________

resp.24.treat.site <- resp.24.treat %>%  
  group_by(site,treatment) %>% 
  summarise(mean_respiration = mean(resp),
            sd = sd(resp),
            count=n(),
            se=(sd/(sqrt(count))))

resp.48.treat.site <- resp.48.treat %>%  
  group_by(site,treatment) %>% 
  summarise(mean_respiration = mean(resp),
            sd = sd(resp),
            count=n(),
            se=(sd/(sqrt(count))))

resp.72.treat.site <- resp.72.treat %>%  
  group_by(site,treatment) %>% 
  summarise(mean_respiration = mean(resp),
            sd = sd(resp),
            count=n(),
            se=(sd/(sqrt(count))))

q1 <- ggplot(data = resp.24.treat.site, aes(x=site, y= mean_respiration, fill = treatment)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Site and Treatment May 2022 (sd) - 24h", x= "Site", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-sd, ymax=mean_respiration+sd), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Treatment",
                    labels=c("Biochar","Control","Fertilizer")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))

q2 <- ggplot(data = resp.24.treat.site, aes(x=site, y= mean_respiration, fill = treatment)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Site and Treatment May 2022 - 24h", x= "Site", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-se, ymax=mean_respiration+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Treatment",
                    labels=c("Biochar","Control","Fertilizer")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))

q3 <- ggplot(data = resp.48.treat.site, aes(x=site, y= mean_respiration, fill = treatment)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Site and Treatment May 2022 (sd) - 48h", x= "Site", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-sd, ymax=mean_respiration+sd), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Treatment",
                    labels=c("Biochar","Control","Fertilizer")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))

q4 <- ggplot(data = resp.48.treat.site, aes(x=site, y= mean_respiration, fill = treatment)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Site and Treatment May 2022 - 48h", x= "Site", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-se, ymax=mean_respiration+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Treatment",
                    labels=c("Biochar","Control","Fertilizer")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))

q5 <- ggplot(data = resp.72.treat.site, aes(x=site, y= mean_respiration, fill = treatment)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Site and Treatment May 2022 (sd) - 72h", x= "Site", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-sd, ymax=mean_respiration+sd), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Treatment",
                    labels=c("Biochar","Control","Fertilizer")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))

q6 <- ggplot(data = resp.72.treat.site, aes(x=site, y= mean_respiration, fill = treatment)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Site and Treatment May 2022 - 72h", x= "Site", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-se, ymax=mean_respiration+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Treatment",
                    labels=c("Biochar","Control","Fertilizer")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))

grid.arrange(q1,q3,q5, nrow = 2, ncol = 2)
grid.arrange(q2,q4,q6, nrow = 2, ncol = 2)

#________________________________________________________________________________________________________________________________


resp.24.site.crop <- resp.24.treat %>% 
  group_by(site,crop) %>% 
  summarise(mean_respiration = mean(resp),
            sd = sd(resp),
            count=n(),
            se=(sd/(sqrt(count))))

resp.48.site.crop <- resp.48.treat %>% 
  group_by(site,crop) %>% 
  summarise(mean_respiration = mean(resp),
            sd = sd(resp),
            count=n(),
            se=(sd/(sqrt(count))))

resp.72.site.crop <- resp.72.treat %>% 
  group_by(site,crop) %>% 
  summarise(mean_respiration = mean(resp),
            sd = sd(resp),
            count=n(),
            se=(sd/(sqrt(count))))

t1 <- ggplot(data = resp.24.site.crop, aes(x=site, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Site and Crop May 2022 (sd) - 24h", x= "Site", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-sd, ymax=mean_respiration+sd), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))

t2 <- ggplot(data = resp.24.site.crop, aes(x=site, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Site and Crop May 2022 - 24h", x= "Site", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-se, ymax=mean_respiration+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))

t3 <- ggplot(data = resp.48.site.crop, aes(x=site, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Site and Crop May 2022 (sd) - 48h", x= "Site", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-sd, ymax=mean_respiration+sd), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))

t4 <- ggplot(data = resp.48.site.crop, aes(x=site, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Site and Crop May 2022 - 48h", x= "Site", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-se, ymax=mean_respiration+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))

t5 <- ggplot(data = resp.72.site.crop, aes(x=site, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Site and Crop May 2022 (sd) - 72h", x= "Site", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-sd, ymax=mean_respiration+sd), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))

t6 <- ggplot(data = resp.72.site.crop, aes(x=site, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Site and Crop May 2022 - 72h", x= "Site", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-se, ymax=mean_respiration+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Jackson's Mill","LP Mine","Reedsville"))

grid.arrange(t1,t3,t5, nrow = 2, ncol = 2)
grid.arrange(t2,t4,t6, nrow = 2, ncol = 2)
####___________________________________________________________________________________________________________________________________________________________
ggplot(data = resp.24.clean, aes(x=crop, y= resp)) + 
  geom_boxplot()
ggplot(data = resp.24.clean, aes(x=treatment, y= resp)) + 
  geom_boxplot()
ggplot(data =resp.24.clean, aes(x=site, y= resp)) + 
  geom_boxplot()

ggplot(data = resp.48.clean, aes(x=crop, y= resp)) + 
  geom_boxplot()
ggplot(data = resp.48.clean, aes(x=treatment, y= resp)) + 
  geom_boxplot()
ggplot(data =resp.48.clean, aes(x=site, y= resp)) + 
  geom_boxplot()

ggplot(data = resp.48.clean, aes(x=crop, y= resp)) + 
  geom_boxplot()
ggplot(data = resp.48.clean, aes(x=treatment, y= resp)) + 
  geom_boxplot()
ggplot(data =resp.48.clean, aes(x=site, y= resp)) + 
  geom_boxplot()


