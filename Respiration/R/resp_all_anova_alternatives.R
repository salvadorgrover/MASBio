##ANOVA by removing "F" treatment
library(ggplot2)
library(dplyr)
library(forcats)
library(agricolae)
library(rstatix)
library(ggpubr)
library(MASS)


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
#resp_all <- resp_all %>% 
#  subset(treatment != "F")

resp_all <- resp_all %>% 
  mutate(land_type = if_else(site %in% c("Agronomy Farm", "Reedsville", "Jackson Mill"), "Agriculture", "Mine")) %>% 
  mutate(site = fct_relevel(site, c("Agronomy Farm", "Jackson Mill", "Reedsville", "Allstar Mine 1", "Allstar Mine 2", "Goshen Road"))) %>% 
  mutate(date = fct_relevel(date, c("June 2021", "May 2022", "August 2022")))

#Remove Goshen June - It rained prior to sample collection and sample readings are skewed heavily. 
resp_all <- resp_all %>% 
  subset(site != "Goshen Road" | date != "June 2021")

zscore <- resp_all %>% 
  group_by(land_type, date, treatment, crop) %>%   
  mutate(mean = mean(resp),
         two.sd = 2*sd(resp),
         zscore = abs(((resp-mean(resp))/sd(resp))))

resp_all <- filter(zscore, zscore < 2)

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


resp_all %>% 
  ungroup() %>% 
  levene_test(log_resp ~ land_type * date * crop * treatment)



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


resp_all %>% 
  group_by(land_type, crop, date, treatment) %>% 
  count()


resp_all %>% 
  ungroup() %>% 
  levene_test(log_resp ~ land_type * date * crop * treatment) 

anova <- aov(log_resp ~ site * crop * treatment * date, data = resp_all)
summary(anova)

TukeyHSD(aov(log_resp ~ crop + site*date, data = resp_all), conf.level = .95)
tuk <- HSD.test(anova, trt = c("site", "date"))
tuk

anova <- aov(log_resp ~ crop * treatment *date * land_type, data = resp_all)
summary(anova)
TukeyHSD(aov(log_resp ~ crop + land_type + date + date*land_type, data = resp_all), conf.level = .95)

##Anova by land type
anova <- aov(log_resp ~ land_type * date * treatment * crop, data = resp_all)
summary(anova)
tuk <- TukeyHSD(aov(resp_bc ~ crop + land_type * date, data = resp_all), conf.level = 0.95)
tuk
tuk <- HSD.test(anova, trt = c("land_type", "date"))
tuk
bar.group(tuk$groups, ylim = c(0,1500), density = 10, border = "blue", las = 2)