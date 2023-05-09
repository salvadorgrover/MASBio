## Salvador Grover
# October 10, 2022
# MASBio enzyme data analysis
library(readxl)
library(dplyr)
library(ggplot2)
library(agricolae)

bg1 <- read_excel("Data/June 2021/bg_activity_june_2021.xlsx")
bg2 <- read_excel("Data/May 2022/bg_activity_may_22.xlsx")
bg3<- read_excel("Data/August 2022/bg_activity_aug_22.xlsx")

per1 <- read_excel("Data/June 2021/per_activity_june_2021.xlsx")
per2 <- read_excel("Data/May 2022/per_activity_may_2022.xlsx")
per3 <- read_excel("Data/August 2022/per_activity_august_2022.xlsx")

ppo1 <- read_excel("Data/June 2021/ppo_activity_june_2021.xlsx")
ppo2 <- read_excel("Data/May 2022/ppo_activity_may_2022.xlsx")
ppo3 <- read_excel("Data/August 2022/ppo_activity_august_2022.xlsx")

treatments <- read_excel("Data/June 2021/study_treatments_2021.xlsx")
treatments$plot <- as.character(treatments$plot)

bg.all <- bg1 %>% 
  rbind(bg2) %>% 
  rbind(bg3) %>% 
  left_join(treatments)

per.all <- per1 %>% 
  rbind(per2) %>% 
  rbind(per3) %>% 
  left_join(treatments)
per.all[per.all == 0]=NA
per.all <- na.omit(per.all)

ppo.all <- ppo1 %>% 
  rbind(ppo2) %>% 
  rbind(ppo3) %>% 
  left_join(treatments)
ppo.all[ppo.all == 0]=NA
ppo.all <- na.omit(ppo.all)

##BG_________________________________________________________________________________________

ggplot(data = bg.all, aes(x = as.factor(date), y = activity)) +
  geom_boxplot() +
  geom_jitter(aes(color = site), alpha = .3) +
  facet_grid(site ~ crop)

bg.all %>% 
  na.omit() %>% 
  group_by(site, date, crop, treatment) %>%  
  summarise(min = min(activity),
            q1 = quantile(activity, 0.25),
            meadian = median(activity),
            q3 = quantile(activity, 0.75),
            max = max(activity)) 


bg.stat <- bg.all %>% 
  na.omit() %>% 
  group_by(site, date, crop, treatment) %>% 
  mutate(mean = mean(activity),
         two.sd = 2*sd(activity),
         z.score = abs(((activity-mean(activity))/sd(activity))))

#bg.stat <- bg.all %>% 
#  na.omit() %>% 
#  group_by(site, date, crop, treatment) %>% 
#  mutate(mean = mean(activity),
#         two.sd = 2*sd(activity),
#         z.score = abs(((activity-mean(activity))/sd(activity))))

bg.clean <- filter(bg.stat, z.score < 2)

bg.clean %>% 
  group_by(site, date, crop, treatment) %>%
  summarise(min = min(activity),
            q1 = quantile(activity, 0.25),
            meadian = median(activity),
            q3 = quantile(activity, 0.75),
            max = max(activity)) 

ggplot(data = bg.clean, aes(x = as.factor(date), y = activity)) +
  geom_boxplot() +
  geom_jitter(aes(color = site), alpha = .3) +
  facet_grid(site ~ crop)

qqnorm(bg.clean$activity, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(bg.clean$activity, col = "red", lwd = 2)

ggplot(data = bg.clean, aes(x = activity, fill = crop)) + 
  geom_density(alpha = .3) + 
  facet_grid(site ~ as.factor(date))

anova <- aov(activity ~ site * crop * treatment * date, data = bg.clean)
summary(anova)


##per_________________________________________________________________________________________
ggplot(data = per.all, aes(x = as.factor(date), y = activity)) +
  geom_boxplot() +
  geom_jitter(aes(color = site), alpha = .3) +
  facet_grid(site ~ crop)

per.all %>% 
  na.omit() %>% 
  group_by(site, date, crop, treatment) %>%
  summarise(min = min(activity),
            q1 = quantile(activity, 0.25),
            meadian = median(activity),
            q3 = quantile(activity, 0.75),
            max = max(activity)) 


per.stat <- per.all %>% 
  na.omit() %>% 
  group_by(site, date, crop, treatment) %>% 
  mutate(mean = mean(activity),
         two.sd = 2*sd(activity),
         z.score = abs(((activity-mean(activity))/sd(activity))))

per.clean <- filter(per.stat, z.score < 2)

per.clean %>% 
  group_by(site, date, crop, treatment) %>%
  summarise(min = min(activity),
            q1 = quantile(activity, 0.25),
            meadian = median(activity),
            q3 = quantile(activity, 0.75),
            max = max(activity)) 

ggplot(data = per.clean, aes(x = as.factor(date), y = activity)) +
  geom_boxplot() +
  geom_jitter(aes(color = site), alpha = .3) +
  facet_grid(site ~ crop)

qqnorm(per.clean$activity, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(per.clean$activity, col = "red", lwd = 2)

ggplot(data = per.clean, aes(x = activity, fill = site)) + 
  geom_density() + 
  facet_grid(site ~ as.factor(date))

anova <- aov(activity ~ site * crop * treatment * date, data = per.clean)
summary(anova)

##PPO_____________________________________________________________________________________________________
ggplot(data = ppo.all, aes(x = as.factor(date), y = activity)) +
  geom_boxplot() +
  geom_jitter(aes(color = site), alpha = .3) +
  facet_grid(site ~ crop)

ppo.all %>% 
  na.omit() %>% 
  group_by(site, date, crop, treatment) %>%
  summarise(min = min(activity),
            q1 = quantile(activity, 0.25),
            meadian = median(activity),
            q3 = quantile(activity, 0.75),
            max = max(activity)) 

ppo.stat <- ppo.all %>% 
  na.omit() %>% 
  group_by(site, date, crop, treatment) %>% 
  mutate(mean = mean(activity),
         two.sd = 2*sd(activity),
         z.score = abs(((activity-mean(activity))/sd(activity))))

ppo.clean <- filter(ppo.stat, z.score < 2)

ppo.clean %>%  
  group_by(site, date, crop, treatment) %>%
  summarise(min = min(activity),
            q1 = quantile(activity, 0.25),
            meadian = median(activity),
            q3 = quantile(activity, 0.75),
            max = max(activity)) 

ggplot(data = ppo.clean, aes(x = as.factor(date), y = activity)) +
  geom_boxplot() +
  geom_jitter(aes(color = site), alpha = .3) +
  facet_grid(site ~ crop)

qqnorm(ppo.clean$activity, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(ppo.clean$activity, col = "red", lwd = 2)

ggplot(data = ppo.clean, aes(x = activity, fill = crop)) + 
  geom_density(alpha = 0.3) + 
  ylim(0,5) +
  facet_grid(site ~ as.factor(date))  

anova <- aov(activity ~ site * crop * treatment * date, data = ppo.clean)
summary(anova)

###Ratio of bg to oxidative enzymes as a measurement of carbon use
bg.join <- bg.clean %>% 
  rename(bg_activity = activity) %>% 
  select(site:treatment)
per.join <- per.clean %>% 
  rename(per_activity = activity) %>% 
  select(site:treatment)

enz.ratio <- inner_join(bg.join, per.join)

join.counts <- enz.ratio %>% 
  count(site, crop, date, treatment) ##some groups with only 2 reps after data cleaning and removing 0 values

enz.ratio$ratio <- enz.ratio$bg_activity/enz.ratio$per_activity

ggplot(data = enz.ratio, aes(x = as.factor(date), y = ratio)) +
  geom_boxplot() +
  geom_jitter(aes(color = site), alpha = .3) +
  facet_grid(site ~ crop) +
  ylim(0,3000)

qqnorm(enz.ratio$ratio, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(enz.ratio$ratio, col = "red", lwd = 2)

qqnorm(log(enz.ratio$ratio), plot.it = TRUE, pch = 4, cex = 0.7)
qqline(log(enz.ratio$ratio), col = "red", lwd = 2)

enz.ratio$log_ratio <- log(enz.ratio$ratio)

ggplot(data = enz.ratio, aes(x = log_ratio)) +
  geom_histogram()

ggplot(data = enz.ratio, aes(x = as.factor(date), y = log_ratio)) +
  geom_boxplot() +
  geom_jitter(aes(color = site), alpha = .3) +
  facet_grid(site ~ crop) 

anova <- aov(log_ratio ~ site * crop * treatment * date, data = enz.ratio)
summary(anova)
tuk.site <- HSD.test(anova, trt = 'site')
tuk.date <- HSD.test(anova, trt = 'date')
tuk.site
tuk.date
TukeyHSD(aov(log_ratio ~ site, data = enz.ratio), conf.level = .95)
TukeyHSD(aov(log_ratio ~ as.factor(date), data = enz.ratio), conf.level = .95)
bar.group(tuk.site$groups, ylim = c(0,7), density = 10, border = "blue")
title(main="Tukey HSD of bg:per by site",xlab = "Site", ylab ="Mean values (log)")
bar.group(tuk.date$groups, ylim = c(0,7), density = 10, border = "blue")
title(main="Tukey HSD of bg:per by date",xlab = "Date", ylab ="Mean values (log)")

TukeyHSD(aov(log_ratio ~ site + as.factor(date), data = enz.ratio), conf.level = .95)


