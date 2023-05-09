library(ggplot2)
library(dplyr)
library(forcats)
library(agricolae)
library(rstatix)
library(ggpubr)
library(MASS)
library(ggpattern)
library(ggpubr)
library(multcompView)

setwd(dir = "C:/Users/salgr/Box/Data/Respiration")
#load data
resp_june <- read.csv("Data/June 2021/resp_clean_june_2021.csv")
resp_may <- read.csv("Data/May 2022/respiration_clean_72h_may_2022.csv")
resp_aug <- read.csv("Data/August 2022/respiration_clean_72h_august_2022.csv")

#add the dates
resp_june$date <- c("June 2021")
resp_may$date <- c("May 2022")
resp_aug$date <- c("August 2022")

resp_june <- resp_june %>% 
  dplyr::select(site, plot, crop, treatment, resp, date)

resp_may <- resp_may %>% 
  dplyr::select(site, plot, crop, treatment, resp, date)

resp_aug <- resp_aug %>% 
  dplyr::select(site, plot, crop, treatment, resp, date)

resp_all <- resp_june %>% 
  rbind(resp_may) %>% 
  rbind(resp_aug)

resp_all$site <- gsub("goshen_road_road_road", "goshen_road", resp_all$site)
resp_all$site <- gsub("agronomy_farm", "Agronomy Farm", resp_all$site)
resp_all$site <- gsub("allstar_mine_1", "Allstar Mine 1", resp_all$site)
resp_all$site <- gsub("allstar_mine_2", "Allstar Mine 2", resp_all$site)
resp_all$site <- gsub("jackson_mill", "Jackson Mill", resp_all$site)
resp_all$site <- gsub("goshen_road", "Goshen Road", resp_all$site)
resp_all$site <- gsub("reedsville", "Reedsville", resp_all$site)

resp_all$treatment <- gsub("F", "C", resp_all$treatment)

resp_all$crop <- gsub('S', 'Switchgrass', resp_all$crop)
resp_all$crop <- gsub('W', 'Willow', resp_all$crop)
resp_all$treatment <- gsub('B', 'Biochar', resp_all$treatment)
resp_all$treatment <- gsub('C', 'Control', resp_all$treatment)

resp_all <- resp_all %>% 
  mutate(land_type = if_else(site %in% c("Agronomy Farm", "Reedsville", "Jackson Mill"), "Agriculture", "Mine")) %>% 
  mutate(site = fct_relevel(site, c("Agronomy Farm", "Jackson Mill", "Reedsville", "Allstar Mine 1", "Allstar Mine 2", "Goshen Road"))) %>% 
  mutate(date = fct_relevel(date, c("June 2021", "May 2022", "August 2022")))

#Remove Goshen June - It rained prior to sample collection and sample readings are skewed heavily. 
resp_all <- resp_all %>% 
  subset(site != "Goshen Road" | date != "June 2021")

ggplot(data = resp_all, aes(x = resp)) +
  geom_histogram()
qqnorm(resp_all$resp, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(resp_all$resp, col = "red", lwd = 2)

qqnorm(log(resp_all$resp), plot.it = TRUE, pch = 4, cex = 0.7)
qqline(log(resp_all$resp), col = "red", lwd = 2)

resp_all$log_resp <- log(resp_all$resp)

model <- lm(log_resp ~ land_type * date * crop * treatment, data = resp_all)
ggqqplot(residuals(model))
shapiro.test(residuals(model))

normality <- resp_all %>% 
  group_by(land_type, date, crop, treatment) %>% 
  shapiro_test(log_resp) %>% 
  subset(p <= .05)

resp_all %>% 
  group_by(site, date, crop, treatment) %>% 
  identify_outliers(resp)

resp_all %>% 
  ungroup() %>% 
  levene_test(log_resp ~ land_type * date * crop * treatment)

resp_all$resp_inverse <- 1/resp_all$resp

model <- lm(resp_inverse ~ site * date * crop * treatment, data = resp_all)
ggqqplot(residuals(model))
shapiro.test(residuals(model))

resp_all$resp_sqrt <- sqrt(resp_all$resp)
model <- lm(resp_sqrt ~ site * date * crop * treatment, data = resp_all)
ggqqplot(residuals(model))
shapiro.test(residuals(model))

zscore <- resp_all %>% 
  group_by(site, date, treatment, crop) %>%   
  mutate(mean = mean(resp),
         two.sd = 2*sd(resp),
         zscore = abs(((resp-mean(resp))/sd(resp))))

resp_all <- filter(zscore, zscore < 2) %>% 
  na.omit()

model <- lm(resp ~ site * date * crop * treatment, data = resp_all)
ggqqplot(residuals(model))
shapiro.test(residuals(model))

resp_all %>% 
  group_by(site, date, crop, treatment) %>% 
  identify_outliers(resp)

ggplot(data = resp_all, aes(x = resp)) +
  geom_histogram()

ggplot(data = resp_all, aes(x = log_resp)) +
  geom_histogram()

ggplot(data = resp_all, aes(x = resp_inverse)) +
  geom_histogram()


qqnorm(resp_all$resp, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(resp_all$resp, col = "red", lwd = 2)

qqnorm(resp_all$log_resp, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(resp_all$log_resp, col = "red", lwd = 2)

normality <- resp_all %>% 
  group_by(land_type, date, crop, treatment) %>% 
  shapiro_test(log_resp) %>% 
  subset(p <= .05)



crops <- c("Switchgrass", "Willow")
names(crops) <- c("S","W")

ggplot(data = resp_all, aes(x = date, y = log_resp, fill = land_type)) +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) +
  facet_grid(site ~ crop, labeller = labeller(crop = crops)) +
  theme_bw() +
  labs(x = "Date", y = "Respiration µg CO2 g soil−1 h−1 (log transformed)", color = "Site", fill = "Land Type", 
       title = "Microbial Respiration by Crop, Site, and Date") +
  scale_fill_brewer(palette = "Dark2")



model <- lm(resp ~ land_type * date * crop * treatment, data = resp_all)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
skewness(residuals(model))
kurtosis(residuals(model))
bc <- boxcox(model)
(lambda <- bc$x[which.max(bc$y)])
new_model <- lm(((resp^lambda-1)/lambda) ~ land_type * date * crop * treatment, data = resp_all)
ggqqplot(residuals(new_model))
shapiro.test(residuals(new_model))
resp_all$resp_bc <- (resp_all$resp^lambda-1)/lambda

resp_all %>% 
  ungroup() %>% 
  levene_test(resp_bc ~ land_type * date * crop * treatment)




resp_all %>% 
  ungroup() %>% 
  levene_test(log_resp ~ site * date * crop * treatment) ##passes test

resp_all %>% 
  ungroup() %>% 
  levene_test(log_resp ~ land_type * date * crop * treatment) #does not pass test

bartlett.test(log_resp ~ interaction(land_type, crop, date, treatment), data = resp_all)
fligner.test(log_resp ~ interaction(land_type, crop, date, treatment), data = resp_all)

anova <- aov(log_resp ~ site * crop * treatment * date, data = resp_all)
summary(anova)

TukeyHSD(aov(log_resp ~ crop + site*date, data = resp_all), conf.level = .95)
tuk <- HSD.test(anova, trt = c("site", "date"))
tuk

anova <- aov(log_resp ~ crop * treatment *date * land_type, data = resp_all)
summary(anova)
TukeyHSD(aov(log_resp ~ crop + land_type + date + date*land_type, data = resp_all), conf.level = .95)

##Anova by land type
anova <- aov(resp_bc ~ land_type * date * treatment * crop, data = resp_all)
summary(anova)
tuk <- TukeyHSD(aov(resp_bc ~ crop + land_type * date, data = resp_all), conf.level = 0.95)
tuk
tuk <- HSD.test(anova, trt = c("land_type", "date"))
tuk
bar.group(tuk$groups, ylim = c(0,1500), density = 10, border = "blue", las = 2)


###Viz
resp.site.crop <- resp_all %>% 
  group_by(site,crop, date) %>% 
  summarise(mean_respiration = mean(resp),
            sd = sd(resp),
            count=n(),
            se=(sd/(sqrt(count))))
ggplot(data = resp.site.crop, aes(x = date, y = mean_respiration, group = site)) +
  geom_line() +
  geom_point() +
  facet_grid(.~crop)

ggplot(data = resp.site.crop, aes(x=site, y= mean_respiration, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Microbial Respiration per Treatment and Crop May 2022(sd) - 24h",x= "Treatment", y="µg CO2 g soil−1 h−1") +
  geom_errorbar(aes(ymin=mean_respiration-sd, ymax=mean_respiration+sd), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  #ylim(0,25) +
  theme_classic() 

#Boxplot
ggplot(data = resp_all, aes(x = date, y = resp, fill = land_type)) +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) +
  #facet_grid(crop ~ ., labeller = labeller(crop = crops)) +
  theme_bw() +
  labs(x = "Date", y = "µg CO2 g soil−1 h−1", color = "Site", fill = "Land Type", title = "Microbial Respiration") +
  scale_fill_brewer(palette = "Dark2")

ggplot(data = resp_all, aes(x = crop, y = resp, fill = crop)) +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) +
  #facet_grid(crop ~ ., labeller = labeller(crop = crops)) +
  theme_bw() +
  labs(x = "Date", y = "µg CO2 g soil−1 h−1", color = "Site", fill = "Crop", title = "Microbial Respiration") +
  scale_fill_brewer(palette = "Set1")

ggplot(data = resp_all, aes(x = treatment, y = resp, fill = treatment)) +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) +
  #facet_grid(crop ~ ., labeller = labeller(crop = crops)) +
  theme_bw() +
  labs(x = "Date", y = "µg CO2 g soil−1 h−1", color = "Site", fill = "Treatment", title = "Microbial Respiration") +
  scale_fill_brewer(palette = "BrBG")

#Bar
#Land Type and Date
anova <- aov(resp_bc ~ land_type * date * treatment * crop, data = resp_all)
tukey <- TukeyHSD(anova)
tukey
cld <- multcompLetters4(anova, tukey)
dt <- resp_all %>% 
  group_by(land_type, date) %>% 
  summarise(mean = mean(resp),
            sd = sd(resp),
            n = n(),
            se = sd/sqrt(n)) %>% 
  arrange(desc(mean))

cld <- as.data.frame.list(cld$`land_type:date`)
dt$cld <- cld$Letters


ggplot(data = dt, aes(x = date, y = mean, fill = land_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9)) +
  theme_classic() +
  labs(x = "", y = "?g CO2 g soil-1 h-1", color = "Site", fill = "Land Type") +
  geom_text(aes(label = cld, y = mean + se), position=position_dodge(width=0.9), vjust = -0.5, size = 6) +
  scale_fill_grey(name = "") +
  scale_y_continuous(expand=c(0,0), limits = c(0,8)) +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 22))

#Treatment
anova <- aov(resp_bc ~ land_type * date * treatment * crop, data = resp_all)
tukey <- TukeyHSD(anova)
tukey
cld <- multcompLetters4(anova, tukey)
dt <- resp_all %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(resp),
            sd = sd(resp),
            n = n(),
            se = sd/sqrt(n)) %>% 
  arrange(desc(mean))

cld <- as.data.frame.list(cld$treatment)
dt$cld <- cld$Letters


ggplot(data = dt, aes(x = treatment, y = mean, fill = treatment)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9)) +
  theme_classic() +
  labs(x = "", y = "µg CO2 g soil−1 h−1", color = "Site", fill = "Land Type") +
  geom_text(aes(label = cld, y = mean + se), position=position_dodge(width=0.9), vjust = -0.5, size = 8) +
  scale_fill_grey(name = "") +
  scale_y_continuous(expand=c(0,0), limits = c(0,2)) +
  theme(text = element_text(size = 20),
        legend.position="none",
        axis.text.x = element_text(size = 25))

#Crop
anova <- aov(resp_bc ~ land_type * date * treatment * crop, data = resp_all)
tukey <- TukeyHSD(anova)
tukey
cld <- multcompLetters4(anova, tukey)
dt <- resp_all %>% 
  group_by(crop) %>% 
  summarise(mean = mean(resp),
            sd = sd(resp),
            n = n(),
            se = sd/sqrt(n)) %>% 
  arrange(desc(mean))

cld <- as.data.frame.list(cld$crop)
dt$cld <- cld$Letters


sup <- ggplot(data = dt, aes(x = crop, y = mean, fill = crop)) +
  geom_bar_pattern(stat = "identity",
                   width=0.4,
                   aes(pattern = crop),
                   fill            = 'white', 
                   colour          = 'black',
                   pattern_spacing = 0.025) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9)) +
  theme_classic() +
  labs(x = "", y = mg~CO[2]-C~kg~soil^-1~h^-1, color = "Site", fill = "Land Type") +
  geom_text(aes(label = cld, y = mean + se), position=position_dodge(width=0.9), vjust = -0.5, size = 4) +
  scale_fill_grey(name = "") +
  scale_pattern_discrete(name = "") +
  scale_y_continuous(expand=c(0,0), limits = c(0,1.55)) +
  theme(text = element_text(size = 9),
        legend.position="none",
        axis.text.x = element_text(size = 10, color = "black"))

ggsave("resp_crop.jpeg", plot = sup, width = 80, height = 45, units = "mm", dpi = 300)

###__________________________________________________________
#removing Goshen from June since it rained after all the other data was collected and resp values are much higher

resp_clean <- resp_all %>% 
  filter(site != "Goshen Road")

qqnorm(resp_clean$resp, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(resp_clean$resp, col = "red", lwd = 2)
ggplot(data = resp_clean, aes(x = resp)) +
  geom_histogram()

qqnorm(log(resp_clean$resp), plot.it = TRUE, pch = 4, cex = 0.7)
qqline(log(resp_clean$resp), col = "red", lwd = 2)
ggplot(data = resp_clean, aes(x = log_resp)) +
  geom_histogram()


anova <- aov(log_resp ~ site * crop * treatment * date, data = resp_clean)
summary(anova)

TukeyHSD(aov(log_resp ~ site + crop + date + crop*date, data = resp_clean), conf.level = .95)

anova <- aov(log_resp ~ land_type * crop * treatment * date, data = resp_clean)
summary(anova)
TukeyHSD(aov(log_resp ~ land_type + crop + date + land_type*date, data = resp_clean), conf.level = .95)

test <- resp_clean %>% 
  filter(land_type == "Agriculture")
anova <- aov(log_resp ~ crop * treatment * date, data = test)
summary(anova)
TukeyHSD(aov(log_resp ~ crop + date, data = test), conf.level = .95)
tuk <- HSD.test(anova, trt = 'date')
tuk


resp_land <- resp_clean %>% 
  group_by(date, crop, land_type) %>% 
  summarise(mean(resp)) %>% 
  rename(mean_respiration = 'mean(resp)')

ggplot(data = resp_land, aes(x = date, y = mean_respiration, group = crop, color = crop)) +
  geom_line() +
  geom_point() +
  facet_grid(land_type ~ .) +
  scale_color_brewer(palette = "Set2") +
  theme_bw() + 
  labs(y = "Mean Microbial Respiration (µg CO2 g soil−1 h−1)", x = "Date")

resp_land_log <- resp_clean %>% 
  group_by(date, crop, land_type) %>% 
  summarise(mean(log_resp)) %>% 
  rename(mean_respiration = 'mean(log_resp)')

ggplot(data = resp_land_log, aes(x = date, y = mean_respiration, group = crop, color = crop)) +
  geom_line() +
  geom_point() +
  facet_grid(land_type ~ .) +
  scale_color_brewer(palette = "Set2", name = "Crop", labels = c("Switchgrass","Willow")) +
  theme_bw(base_size = 20) + 
  labs(y = "Mean Microbial Respiration (µg CO2 g soil−1 h−1)\nlog tranformed", x = "Date", title = "Increase in Microbial Activity") +
  theme(axis.title=element_text(size=14),plot.title = element_text(size=16, face = "bold"))

##SSSA Viz
resp_land_log <- resp_clean %>% 
  group_by(date, crop, land_type) %>% 
  summarise(mean_log_resp = mean(log_resp),
          sd = sd(log_resp),
          count=n(),
          se=(sd/(sqrt(count)))) 

p<- ggplot(data = resp_land_log, aes(x = date, y = mean_log_resp, group = crop, color = crop)) +
  geom_line(size = 1.5) +
  geom_point() +
  facet_grid(land_type ~ .) +
  scale_color_brewer(palette = "Set2", name = "Crop", labels = c("Switchgrass","Willow")) +
  theme_bw(base_size = 20) + 
  labs(y = "", x = "") +
  theme(axis.title=element_text(size=14),
        plot.title = element_text(size=16, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(labels =c("June 2021" = "0",
                             "May 2022" = "349",
                             "August 2022" = "454"), expand = c(0.02,0.03)) +
  geom_pointrange(aes(ymin=mean_log_resp-se, 
                      ymax=mean_log_resp+se),
                  position= "dodge",
                  alpha =.8)
p
ggsave(
  plot = p,
  filename = "resp_sssa.png",
  bg = "transparent"
)

### Offsite samples analysis
offsite_may <- read.csv("Data/May 2022/offsite_may_2022_respiration.csv") %>% 
  filter(hour == "72")
offsite_may$date <- c("May 2022")

offsite_aug <- read.csv("Data/August 2022/offsite_august_2022_respiration.csv") %>% 
  filter(hour == "72")
offsite_aug$date <- c("August 2022")

offsite_all <- rbind(offsite_may,offsite_aug)


offsite_all$site <- gsub("agronomy_farm", "Agronomy Farm", offsite_all$site)
offsite_all$site <- gsub("allstar_mine_1", "Allstar Mine 1", offsite_all$site)
offsite_all$site <- gsub("allstar_mine_2", "Allstar Mine 2", offsite_all$site)
offsite_all$site <- gsub("jackson_mill", "Jackson Mill", offsite_all$site)
offsite_all$site <- gsub("goshen", "Goshen Road", offsite_all$site)
offsite_all$site <- gsub("reedsville", "Reedsville", offsite_all$site)

offsite_all <- offsite_all %>% 
  mutate(land_type = if_else(site %in% c("Agronomy Farm", "Reedsville", "Jackson Mill"), "Agriculture", "Mine")) %>% 
  mutate(site = fct_relevel(site, c("Agronomy Farm", "Jackson Mill", "Reedsville", "Allstar Mine 1", "Allstar Mine 2", "Goshen Road"))) %>% 
  mutate(date = fct_relevel(date, c("May 2022", "August 2022")))

qqnorm(offsite_all$resp, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(offsite_all$resp, col = "red", lwd = 2)
ggplot(data = offsite_all, aes(x = resp)) +
  geom_histogram()


offsite_all$plot_type <- c("Offsite")
offsite_all$crop <- c("Pasture")
offsite_all <- offsite_all %>% 
  dplyr::select(-treatment, -mass_soil, -plot_no, -"max.co2_int.", -"mol_co2", -c_g_hr, -hour, -plot) 

resp_all$plot_type <- c("Onsite")
resp_all <- resp_all %>% 
  ungroup() %>% 
  dplyr::select(site, crop, resp, date, land_type, plot_type)  

resp_off_on <- rbind(resp_all, offsite_all) %>% 
  subset(date %in% c("May 2022")) #%>% 
  #subset(site %in% c("Allstar Mine 1", "Allstar Mine 2", "Goshen Road", "Jackson Mill"))

resp_off_on %>% 
  group_by(crop, land_type) %>% 
  na.omit() %>% 
  summarise(mean = mean(resp))

#Seperate by Mine and Ag for analysis
mine_off_on <- resp_off_on %>% 
  subset(land_type == "Mine")

model <- lm(resp ~ crop, data = mine_off_on)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
bc <- boxcox(model)
(lambda <- bc$x[which.max(bc$y)])
new_model <- lm(((resp^lambda-1)/lambda) ~ crop, data = mine_off_on)
ggqqplot(residuals(new_model))
shapiro.test(residuals(new_model))
mine_off_on$resp_bc <- (mine_off_on$resp^lambda-1)/lambda

mine_off_on %>% 
  ungroup() %>% 
  levene_test(resp_bc ~ crop)

anova <- aov(resp_bc ~ crop, data = mine_off_on)
anova
tukey <- TukeyHSD(anova)
tukey
tuk <- HSD.test(anova, trt = "crop", group = T)
tuk

#Ag 
ag_off_on <- resp_off_on %>% 
  subset(land_type == "Agriculture")

model <- lm(resp ~ crop, data = ag_off_on)
ggqqplot(residuals(model))
shapiro.test(residuals(model))

ag_off_on %>% 
  ungroup() %>% 
  levene_test(resp ~ crop)

anova <- aov(resp ~ crop, data = ag_off_on)
anova
tukey <- TukeyHSD(anova)
tukey
tuk <- HSD.test(anova, trt = "crop", group = T)
tuk
  
###__________________________________________________________________________________________________________________
model <- lm(resp ~ land_type * crop * plot_type, data = resp_off_on)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
bc <- boxcox(model)
(lambda <- bc$x[which.max(bc$y)])
new_model <- lm(((resp^lambda-1)/lambda) ~ land_type * date * plot_type, data = resp_off_on)
ggqqplot(residuals(new_model))
shapiro.test(residuals(new_model))
resp_off_on$resp_bc <- (resp_off_on$resp^lambda-1)/lambda

resp_off_on %>% 
ungroup() %>% 
  levene_test(resp_bc ~ land_type * date * plot_type)

anova <- aov(resp_bc ~ land_type * date * plot_type, data = resp_off_on)
summary(anova)
tuk <- HSD.test(anova, trt = c("date", "plot_type"))
tuk

#Boxplot
ggplot(data = resp_off_on, aes(x = date, y = resp, fill = plot_type)) +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) +
  #facet_grid(land_type ~ .,) +
  theme_bw() +
  labs(x = "Date", y = "µg CO2 g soil−1 h−1", color = "Site", fill = "Land Type", title = "Offsite Respiration") +
  scale_fill_brewer(palette = "Set1")

ggplot(data = resp_off_on, aes(x = plot_type, y = resp, fill = plot_type)) +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) +
  #facet_grid(land_type ~ .,) +
  theme_bw() +
  labs(x = "Date", y = "µg CO2 g soil−1 h−1", color = "Site", fill = "Land Type", title = "Offsite Respiration") +
  scale_fill_brewer(palette = "Set1")

#Bar
#Treatment
anova <- aov(resp_bc ~ land_type * date * plot_type, data = resp_off_on)
tukey <- TukeyHSD(anova)
tukey
cld <- multcompLetters4(anova, tukey)
dt <- resp_off_on %>% 
  group_by(plot_type) %>% 
  summarise(mean = mean(resp),
            sd = sd(resp),
            n = n(),
            se = sd/sqrt(n)) %>% 
  arrange(desc(mean))

cld <- as.data.frame.list(cld$plot_type)
dt$cld <- cld$Letters


ggplot(data = dt, aes(x = plot_type, y = mean, fill = plot_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9)) +
  theme_classic() +
  labs(x = "", y = "µg CO2 g soil−1 h−1", color = "Site", fill = "Land Type") +
  geom_text(aes(label = cld, y = mean + se), position=position_dodge(width=0.9), vjust = -0.5, size = 8) +
  scale_fill_grey(name = "") +
  scale_y_continuous(expand=c(0,0), limits = c(0,2.3)) +
  theme(text = element_text(size = 20),
        legend.position="none",
        axis.text.x = element_text(size = 25))

#Treatment
anova <- aov(resp_bc ~ land_type * date * plot_type, data = resp_off_on)
tukey <- TukeyHSD(anova)
tukey
cld <- multcompLetters4(anova, tukey)
dt <- resp_off_on %>% 
  group_by(date, plot_type) %>% 
  summarise(mean = mean(resp),
            sd = sd(resp),
            n = n(),
            se = sd/sqrt(n)) %>% 
  arrange(desc(mean))

cld <- as.data.frame.list(cld$`date:plot_type`)
dt$cld <- cld$Letters


ggplot(data = dt, aes(x = date, y = mean, fill = plot_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9)) +
  theme_classic() +
  labs(x = "", y = "µg CO2 g soil−1 h−1", color = "Site", fill = "Land Type") +
  geom_text(aes(label = cld, y = mean + se), position=position_dodge(width=0.9), vjust = -0.5, size = 8) +
  scale_fill_grey(name = "") +
  scale_y_continuous(expand=c(0,0), limits = c(0,3.2)) +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 25))

#__________________________________________________________________________________________________________________________
### Panel for POXC and Resp
#Run POXC script to load data
anova <- aov(poxc ~ land_type * date * treatment * crop, data = poxc.all)
tukey <- TukeyHSD(anova)
tukey
cld <- multcompLetters4(anova, tukey)
dt <- poxc.all %>% 
  group_by(land_type, date) %>% 
  summarise(mean = mean(poxc),
            sd = sd(poxc),
            n = n(),
            se = sd/sqrt(n)) %>% 
  arrange(desc(mean))

cld <- as.data.frame.list(cld$`land_type:date`)
dt$cld <- cld$Letters


p1 <- ggplot(data = dt, aes(x = date, y = mean, fill = land_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9)) +
  theme_classic() +
  labs(x = "", y = "POXC (mg/kg of soil)", color = "Site", fill = "Land Type") +
  geom_text(aes(label = cld, y = mean + se), position=position_dodge(width=0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(name = "",
                    values = c("grey", "black")) +
  scale_y_continuous(expand=c(0,0), limits = c(0,1200)) +
  theme(text = element_text(size = 10),
        legend.position = "none",
        legend.key.size = unit(.4, 'cm'),
        axis.text.x=element_blank())

anova <- aov(resp_bc ~ land_type * date * treatment * crop, data = resp_all)
tukey <- TukeyHSD(anova)
tukey
cld <- multcompLetters4(anova, tukey)
dt <- resp_all %>% 
  group_by(land_type, date) %>% 
  summarise(mean = mean(resp),
            sd = sd(resp),
            n = n(),
            se = sd/sqrt(n)) %>% 
  arrange(desc(mean))

cld <- as.data.frame.list(cld$`land_type:date`)
dt$cld <- cld$Letters


p2 <- ggplot(data = dt, aes(x = date, y = mean, fill = land_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9)) +
  theme_classic() +
  labs(x = "", y = mg~CO[2]-C~kg~soil^-1~h^-1, color = "Site", fill = "Land Type") +
  geom_text(aes(label = cld, y = mean + se), position=position_dodge(width=0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(name = "",
                    values = c("grey", "black")) +
  scale_y_continuous(expand=c(0,0), limits = c(0,2.2)) +
  scale_x_discrete(labels = c("June \n2021", "May \n2022", "August \n2022")) +
  theme(text = element_text(size = 10),
        legend.position = "top",
        axis.text.x = element_text(size = 10, color = "black"),
        legend.text = element_text(size=8),
        legend.key.size = unit(.1, 'cm'))

fig1 <- ggarrange(p1, p2,
          nrow = 2, align = 'v',
          labels = c("a)", "b)"),
          common.legend=TRUE,
          label.x = 0.06,
          label.y = 1.12,
          font.label=list(color="black",size=10),
          heights = c(1,1.3))
        

#dev.new(width=80, height = 80, unit="mm")
ggsave("poxc_resp.jpeg", plot = fig1, width = 80, height = 100, units = "mm", dpi = 300)

