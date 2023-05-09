## Salvador Grover
# October 10, 2022
# MASBio enzyme data analysis
library(readxl)
library(dplyr)
library(ggplot2)
library(agricolae)
library(forcats)
library(RColorBrewer)
library(ggformula)
library(tidyr)
library(ggpubr)
library(rstatix)

bg1 <- read_excel("Data/June 2021/bg_activity_june_2021.xlsx")
bg1$date <- "June 2021"
bg2 <- read_excel("Data/May 2022/bg_activity_may_22.xlsx")
bg2$date <- "May 2022"
bg3<- read_excel("Data/August 2022/bg_activity_aug_22.xlsx")
bg3$date <- "August 2022"

per1 <- read_excel("Data/June 2021/per_activity_june_2021.xlsx")
per1$date <- "June 2021"
per2 <- read_excel("Data/May 2022/per_activity_may_2022.xlsx")
per2$date <- "May 2022"
per3 <- read_excel("Data/August 2022/per_activity_august_2022.xlsx")
per3$date <- "August 2022"

ppo1 <- read_excel("Data/June 2021/ppo_activity_june_2021.xlsx")
ppo1$date <- "June 2021"
ppo2 <- read_excel("Data/May 2022/ppo_activity_may_2022.xlsx")
ppo2$date <- "May 2022"
ppo3 <- read_excel("Data/August 2022/ppo_activity_august_2022.xlsx")
ppo3$date <- "August 2022"

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

ppo.all <- ppo1 %>% 
  rbind(ppo2) %>% 
  rbind(ppo3) %>% 
  left_join(treatments)

##Replace F treatment with C since fertilizer has not been applied to any plots at this point
#ppo.all$treatment <- gsub('F', 'C', ppo.all$treatment)
#per.all$treatment <- gsub('F', 'C', per.all$treatment)
#bg.all$treatment <- gsub('F', 'C', bg.all$treatment)

ppo.all <- ppo.all %>% 
  subset(treatment != "F")
per.all <- per.all %>% 
  subset(treatment != "F")
bg.all <- bg.all %>% 
  subset(treatment != "F")

per.all$site <- gsub("goshen_road", "Goshen Road", per.all$site)
per.all$site <- gsub("agronomy_farm", "Agronomy Farm", per.all$site)
per.all$site <- gsub("allstar_mine_1", "Allstar Mine 1", per.all$site)
per.all$site <- gsub("allstar_mine_2", "Allstar Mine 2", per.all$site)
per.all$site <- gsub("jackson_mill", "Jackson Mill", per.all$site)
per.all$site <- gsub("reedsville", "Reedsville", per.all$site)

ppo.all$site <- gsub("goshen_road", "Goshen Road", ppo.all$site)
ppo.all$site <- gsub("agronomy_farm", "Agronomy Farm", ppo.all$site)
ppo.all$site <- gsub("allstar_mine_1", "Allstar Mine 1", ppo.all$site)
ppo.all$site <- gsub("allstar_mine_2", "Allstar Mine 2", ppo.all$site)
ppo.all$site <- gsub("jackson_mill", "Jackson Mill", ppo.all$site)
ppo.all$site <- gsub("reedsville", "Reedsville", ppo.all$site)


bg.all$site <- gsub("goshen_road", "Goshen Road", bg.all$site)
bg.all$site <- gsub("agronomy_farm", "Agronomy Farm", bg.all$site)
bg.all$site <- gsub("allstar_mine_1", "Allstar Mine 1", bg.all$site)
bg.all$site <- gsub("allstar_mine_2", "Allstar Mine 2", bg.all$site)
bg.all$site <- gsub("jackson_mill", "Jackson Mill", bg.all$site)
bg.all$site <- gsub("reedsville", "Reedsville", bg.all$site)

per.all <- per.all %>% 
  mutate(land_type = if_else(site %in% c("Agronomy Farm", "Reedsville", "Jackson Mill"), "Agriculture", "Mine")) %>% 
  mutate(site = fct_relevel(site, c("Agronomy Farm", "Jackson Mill", "Reedsville", "Allstar Mine 1", "Allstar Mine 2", "Goshen Road"))) %>% 
  mutate(date = fct_relevel(date, c("June 2021", "May 2022", "August 2022")))

ppo.all <- ppo.all %>% 
  mutate(land_type = if_else(site %in% c("Agronomy Farm", "Reedsville", "Jackson Mill"), "Agriculture", "Mine")) %>% 
  mutate(site = fct_relevel(site, c("Agronomy Farm", "Jackson Mill", "Reedsville", "Allstar Mine 1", "Allstar Mine 2", "Goshen Road"))) %>% 
  mutate(date = fct_relevel(date, c("June 2021", "May 2022", "August 2022")))

bg.all <- bg.all %>% 
  mutate(land_type = if_else(site %in% c("Agronomy Farm", "Reedsville", "Jackson Mill"), "Agriculture", "Mine")) %>% 
  mutate(site = fct_relevel(site, c("Agronomy Farm", "Jackson Mill", "Reedsville", "Allstar Mine 1", "Allstar Mine 2", "Goshen Road"))) %>% 
  mutate(date = fct_relevel(date, c("June 2021", "May 2022", "August 2022")))

crops <- c("Switchgrass", "Willow")
names(crops) <- c("S","W")

##BG_________________________________________________________________________________________

ggplot(data = bg.all, aes(x = as.factor(date), y = activity)) +
  geom_boxplot() +
  geom_jitter(aes(color = site), alpha = .3) +
  facet_grid(site ~ crop)

bg.all %>% 
  na.omit() %>% 
  group_by(land_type, date, crop, treatment) %>%  
  summarise(min = min(activity),
            q1 = quantile(activity, 0.25),
            meadian = median(activity),
            q3 = quantile(activity, 0.75),
            max = max(activity)) 


bg.stat <- bg.all %>% 
  na.omit() %>% 
  group_by(land_type, date, crop, treatment) %>% 
  mutate(mean = mean(activity),
         two.sd = 2*sd(activity),
         z.score = abs(((activity-mean(activity))/sd(activity))))

#bg.stat <- bg.all %>% 
#  na.omit() %>% 
#  group_by(site, date, crop, treatment) %>% 
#  mutate(mean = mean(activity),
#         two.sd = 2*sd(activity),
#         z.score = abs(((activity-mean(activity))/sd(activity))))

bg.clean <- filter(bg.stat, z.score < 2) %>% 
  mutate(land_type = if_else(site %in% c("Agronomy Farm", "Reedsville", "Jackson Mill"), "Agriculture", "Mine")) %>% 
  mutate(site = fct_relevel(site, c("Agronomy Farm", "Jackson Mill", "Reedsville", "Allstar Mine 1", "Allstar Mine 2", "Goshen Road"))) %>% 
  mutate(date = fct_relevel(date, c("June 2021", "May 2022", "August 2022")))

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

model <- lm(activity ~ land_type * date * crop * treatment, data = bg.clean)
ggqqplot(residuals(model))
shapiro.test(residuals(model))

plot(density(bg.clean$sqrt_activity))
skewness(bg.clean$activity)
kurtosis(bg.clean$activity)

bg.clean$log_activity <- log(bg.clean$activity)
bg.clean$cube_activity <- bg.clean$activity^(1/3)
bg.clean$sqrt_activity <- sqrt(bg.clean$activity)

model <- lm(sqrt_activity ~ land_type * date * crop * treatment, data = bg.clean)
ggqqplot(residuals(model))
shapiro.test(residuals(model))

model <- lm(activity ~ land_type * date * crop * treatment, data = bg.clean)
bc <- boxcox(model)
(lambda <- bc$x[which.max(bc$y)])
new_model <- lm(((activity^lambda-1)/lambda) ~ land_type * date * crop * treatment, data = bg.clean)
ggqqplot(residuals(new_model))
shapiro.test(residuals(new_model))
resp_all$resp_bc <- (resp_all$resp^lambda-1)/lambda



bg.stat <- bg.clean %>% 
  na.omit() %>% 
  group_by(site, date, crop, treatment) %>% 
  mutate(mean = mean(activity),
         two.sd = 2*sd(activity),
         z.score = abs(((activity-mean(activity))/sd(activity))))




shapiro <- bg.clean %>% 
  group_by(land_type, date, crop, treatment) %>% 
  shapiro_test(activity) %>% 
  subset(p <= .05)

ggplot(data = bg.clean, aes(x = activity, fill = crop)) + 
  geom_density(alpha = .3) + 
  facet_grid(site ~ as.factor(date))

ggplot(data = bg.clean, aes(x = date, y = activity, fill = land_type)) +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) +
  facet_grid(site ~ crop, labeller = labeller(crop = crops)) +
  theme_bw() +
  labs(x = "Date", y = "BG", color = "Site", fill = "Land Type", 
       title = "Beta-Glucosidase by Crop, Site, and Date") +
  scale_fill_brewer(palette = "Dark2")

##Need to fix
ggplot(data = bg.clean, aes(x = date, y = activity, group = land_type, color = crop)) +
  geom_line() +
  geom_point() +
  facet_grid(land_type ~ .) +
  scale_color_brewer(palette = "Set2") +
  theme_bw() +
  labs(y = "Mean BG", x = "Date")

#homogneity of variance assumption test
bg.clean %>% 
  ungroup() %>% 
  levene_test(activity ~ land_type * date * crop * treatment)

bg.clean %>% 
  group_by(land_type, date, crop, treatment) %>% 
  count() %>% 
  print(n = 24)

model <- lm(activity ~ land_type * date * crop * treatment, data = bg.clean)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
bg.clean %>% 
  ungroup() %>% 
  levene_test(activity ~ land_type * date * crop * treatment)

anova <- aov(activity ~ site * crop * treatment * date, data = bg.clean)
summary(anova)

anova <- aov(activity ~ land_type * crop * treatment * date, data = bg.clean)
summary(anova)

tuk <- HSD.test(anova, trt = c("land_type", "crop", "date"))
tuk
bar.group(tuk$groups, ylim = c(0,230), density = 10, border = "blue", las = 2)

##_________________________________________________________________________________________________________________
per.all$ppo_per <- per.all$activity + ppo.all$activity
ppo.per <- per.all %>% 
  dplyr::select(-activity)
ppo.per <- rename(ppo.per, activity = ppo_per)

ppo.per.stat <- ppo.per %>% 
  na.omit() %>% 
  group_by(land_type, date, crop, treatment) %>% 
  mutate(mean = mean(activity),
         two.sd = 2*sd(activity),
         z.score = abs(((activity-mean(activity))/sd(activity))))

ppo.per.clean <- filter(ppo.per.stat, z.score < 2) %>% 
  mutate(land_type = if_else(site %in% c("Agronomy Farm", "Reedsville", "Jackson Mill"), "Agriculture", "Mine")) %>% 
  mutate(site = fct_relevel(site, c("Agronomy Farm", "Jackson Mill", "Reedsville", "Allstar Mine 1", "Allstar Mine 2", "Goshen Road"))) %>% 
  mutate(date = fct_relevel(date, c("June 2021", "May 2022", "August 2022")))


ggplot(data = ppo.per.clean, aes(x = as.factor(date), y = activity)) +
  geom_boxplot() +
  geom_jitter(aes(color = site), alpha = .3) +
  facet_grid(site ~ crop)

qqnorm(ppo.per.clean$activity, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(ppo.per.clean$activity, col = "red", lwd = 2)
model <- lm(activity ~ land_type * date * crop * treatment, data = ppo.per.clean)
ggqqplot(residuals(model))
shapiro.test(residuals(model))



ggplot(data = ppo.per.clean, aes(x = activity, fill = site)) + 
  geom_density() + 
  facet_grid(site ~ as.factor(date))

ggplot(data = ppo.per.clean, aes(x = date, y = activity, fill = land_type)) +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) +
  facet_grid(site ~ crop, labeller = labeller(crop = crops)) +
  theme_bw() +
  labs(x = "Date", y = "BG", color = "Site", fill = "Land Type", 
       title = "PER by Crop, Site, and Date") +
  scale_fill_brewer(palette = "Dark2")

ppo.per.clean[ppo.per.clean == 0]=NA
ppo.per.clean <- na.omit(ppo.per.clean)



ppo.per.clean$log_act <- log(ppo.per.clean$activity)



qqnorm(ppo.per.clean$log_act, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(ppo.per.clean$log_act, col = "red", lwd = 2)

model <- lm(log_act ~ site * date * crop * treatment, data = ppo.per.clean)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
shapiro.test(ppo.per.clean$log_act)

shapiro <- ppo.per.clean %>% 
  group_by(land_type, date, crop, treatment) %>% 
  shapiro_test(log_act) %>% 
  subset(p <= .05)

ppo.per.clean %>% 
  ungroup() %>% 
  levene_test(log_act ~ site * date * crop * treatment) 

anova <- aov(log_act ~ site * crop * treatment * date, data = ppo.per.clean)
summary(anova)

anova <- aov(log_act ~ land_type * crop * treatment * date, data = ppo.per.clean)
summary(anova)

tuk <- HSD.test(anova, trt = c("land_type","date"))
tuk
bar.group(tuk$groups, ylim = c(0,230), density = 10, border = "blue", las = 2)




##offsites 
bg.offsite <- bg2 %>% 
  rbind(bg3) %>% 
  subset(plot %in% c("offsite 1", "offsite 2", "offsite 3"))
bg.offsite$enzyme <- c("BG")

per.offsite <- per2 %>% 
  rbind(per3) %>% 
  subset(plot %in% c("offsite 1", "offsite 2", "offsite 3"))

ppo.offsite <- ppo2 %>% 
  rbind(ppo3) %>% 
  subset(plot %in% c("offsite 1", "offsite 2", "offsite 3"))

ppo.offsite <- rename(ppo.offsite, ppo = activity)
per.offsite <- rename(per.offsite, per = activity)


ppo_per_offsite <- full_join(ppo.offsite, per.offsite)
ppo_per_offsite[is.na(ppo_per_offsite)] <- 0
ppo_per_offsite$ppo_per <- ppo_per_offsite$per + ppo_per_offsite$ppo

ppo_per_offsite["ppo_per"][ppo_per_offsite["ppo_per"] == 0] <- NA
ppo_per_offsite <- na.omit(ppo_per_offsite)
ppo_per_offsite$enzyme <- c("Oxidative")
ppo_per_offsite <- rename(ppo_per_offsite, activity = ppo_per) 
ppo_per_offsite <- ppo_per_offsite %>% 
  select(site, plot, activity, date, enzyme)

offsite.all <- rbind(bg.offsite, ppo_per_offsite) 




 

offsite.all$site <- gsub("goshen_road", "Goshen Road", offsite.all$site)
offsite.all$site <- gsub("agronomy_farm", "Agronomy Farm", offsite.all$site)
offsite.all$site <- gsub("allstar_mine_1", "Allstar Mine 1", offsite.all$site)
offsite.all$site <- gsub("allstar_mine_2", "Allstar Mine 2", offsite.all$site)
offsite.all$site <- gsub("jackson_mill", "Jackson Mill", offsite.all$site)
offsite.all$site <- gsub("reedsville", "Reedsville", offsite.all$site)

offsite.all <- offsite.all %>% 
  mutate(land_type = if_else(site %in% c("Agronomy Farm", "Reedsville", "Jackson Mill"), "Agriculture", "Mine")) %>% 
  mutate(site = fct_relevel(site, c("Agronomy Farm", "Jackson Mill", "Reedsville", "Allstar Mine 1", "Allstar Mine 2", "Goshen Road"))) %>% 
  mutate(date = fct_relevel(date, c("May 2022", "August 2022")))
offsite.all$plot_type <- c("offsite")

bg.clean$enzyme <- c("BG")
per.clean$enzyme <- c("Oxidative")

onsite.all <- rbind(bg.clean, per.clean) %>% 
  rbind(ppo.clean) %>% 
  ungroup() %>% 
  select(-crop, -treatment, -mean, -two.sd, -z.score)
onsite.all$plot_type <- c("onsite")
onsite.all <- onsite.all %>% 
  select(site, plot, activity, date, enzyme, land_type, plot_type)

enzyme.on.off <- rbind(onsite.all, offsite.all) %>% 
  subset(date %in% c("May 2022", "August 2022")) %>% 
  na.omit()
  
  
  
  
enzyme.on.off <- enzyme.on.off %>% 
  pivot_wider(names_from = enzyme, values_from = activity)

qqnorm(enzyme.on.off$BG, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(enzyme.on.off$BG, col = "red", lwd = 2)

qqnorm(enzyme.on.off$Oxidative, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(enzyme.on.off$Oxidative, col = "red", lwd = 2)


qqnorm(log(enzyme.on.off$Oxidative), plot.it = TRUE, pch = 4, cex = 0.7)
qqline(log(enzyme.on.off$Oxidative), col = "red", lwd = 2)

enzyme.on.off$log_ox <- log(enzyme.on.off$Oxidative)

ggplot(data = enzyme.on.off, aes(x = date, y = BG, color = plot_type)) +
  geom_boxplot() +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) 

ggplot(data = enzyme.on.off, aes(x = date, y = Oxidative, color = plot_type)) +
  geom_boxplot() +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) 


anova <- aov(BG ~ land_type * date * plot_type, data = enzyme.on.off)
summary(anova)

anova <- aov(log_ox ~ land_type * date * plot_type, data = enzyme.on.off)
summary(anova)


ggplot(data = enzyme.on.off, aes(x = date, y = BG, color = plot_type)) +
  geom_boxplot() +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) +  
  facet_grid( ~ site) +
  labs(x = "Date", y = "Activity", title = "BG Offsite and Onsite")

ggplot(data = enzyme.on.off, aes(x = date, y = PPO, color = plot_type)) +
  geom_boxplot() +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) +
  facet_grid( ~ site) +
  labs(x = "Date", y = "Activity", title = "PPO Offsite and Onsite")

ggplot(data = enzyme.on.off, aes(x = date, y = PER, color = plot_type)) +
  geom_boxplot() +
  geom_boxplot(alpha = .2) +
  geom_jitter(alpha = .4) +
  facet_grid( ~ site) +
  labs(x = "Date", y = "Activity", title = "PER Offsite and Onsite")

###_________________________________________________________________________________________________


###SSSA and other analysis
anova <- aov(log_ratio ~ crop * treatment *date * land_type, data = enz.ratio)
summary(anova)
TukeyHSD(aov(log_ratio ~ land_type + date, data = enz.ratio), conf.level = .95)


enz_land <- enz.ratio %>% 
  group_by(date, crop, land_type) %>% 
  summarise(mean(ratio)) %>% 
  rename(mean_ratio = 'mean(ratio)')

ggplot(data = enz_land, aes(x = date, y = mean_ratio, group = crop, color = crop)) +
  geom_line() +
  geom_point() +
  facet_grid(land_type ~ .) +
  scale_color_brewer(palette = "Set2") +
  theme_bw() +
  labs(y = "Mean BG:PER", x = "Date")

enz_land_log <- enz.ratio %>% 
  group_by(date, crop, land_type) %>% 
  summarise(mean_log_ratio = mean(log_ratio),
            sd = sd(log_ratio),
            count=n(),
            se=(sd/(sqrt(count))))

test <- enz.ratio %>% 
  filter(land_type == "Agriculture")
anova <- aov(log_ratio ~ crop * treatment *date, data = test)
summary(anova)
TukeyHSD(aov(log_ratio ~ date, data = test), conf.level = .95)
tuk <- HSD.test(anova, trt = 'date')
tuk



#  Mean BG:PER (log transformed), Decrease in Cellulose Acquiring Enzymes, Increase in Lignin Oxidative Enzymes
p<- ggplot(data = enz_land_log, aes(x = date, y = mean_log_ratio, group = crop, color = crop)) +
  geom_line(size = 1.5) +
  geom_point() +
  facet_grid(land_type ~ .) +
  scale_color_brewer(palette = "Set2") +
  theme_bw(base_size = 20) +
  labs(y = "", x = "") +
  theme(legend.position = "none", 
        axis.title=element_text(size=14),
        plot.title = element_text(size=16, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.y = element_blank()) +
  scale_x_discrete(labels =c("June 2021" = "0",
                             "May 2022" = "349",
                             "August 2022" = "454"), expand = c(.02,.02)) +
  geom_pointrange(aes(ymin=mean_log_ratio-se, 
                      ymax=mean_log_ratio+se),
                  position= "dodge",
                  alpha =.8) 
p
ggsave(
  plot = p,
  filename = "enzyme_sssa.png",
  bg = "transparent"
)