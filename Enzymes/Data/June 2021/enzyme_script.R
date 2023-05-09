pkgTest <- function(x)  ## function to install necessary packages if not already installed 
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

## names of required packages 
lib <- c("tidyr", "dplyr", "tidyverse", "gridExtra","readxl", "ggplot2", "ggsignif", "RColorBrewer")

## Install packages
for (i in 1:length(lib))
{
  pkgTest(lib[i])
}

bg <- read_excel("enzyme_activity_bg_june_2021.xlsx") 
ppo <- read_excel("ppo_calc_june_2021.xlsx") 
per <- read_excel("per_calc_june_2021.xlsx") 
plot_treat <- read_excel("study_treatments_2021.xlsx")

bg$date <- "June 2021"
ppo$date <- "June 2021"
per$date <- "June 2021"


enzyme_treat <- merge(bg, plot_treat)

##group by site, crop, and treatment and get the outliers of those groups, if any
enzyme.stat <- enzyme_treat %>% 
  group_by(site, treatment, crop) %>%   
  mutate(mean = mean(activity),
         two.sd = 2*sd(activity),
         z.score = abs(((activity-mean(activity))/sd(activity))))

enzyme.clean <- filter(enzyme.stat, z.score < 2)

anova <- aov(activity ~ site + crop + treatment, data = enzyme.clean)
summary(anova)

oneway.anova <- aov(activity ~ crop, data = enzyme.clean)
summary(oneway.anova)

qqnorm(enzyme.clean$activity, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(enzyme.clean$activity, col = "red", lwd = 2)

qqnorm(log(enzyme.clean$activity), plot.it = TRUE, pch = 4, cex = 0.7)
qqline(log(enzyme.clean$activity), col = "red", lwd = 2)

quantile(enzyme.clean$activity)
quantile(enzyme.clean$activity, probs = seq(0.9, 1, 0.01))

#_______________________________________________________________________________
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #color blind friendly palettes

boxplot(activity ~ site, data =  enzyme.clean)

ggplot(data = enzyme.clean, aes(x= crop, y = activity)) +
  geom_boxplot() +
  theme_classic() +
  scale_x_discrete(labels = c("Switchgrass", "Willow")) +
  labs(title="Beta-Glucosidase Activity by Crop", x= "Crop", y="Beta-Glucosidase Activity (µmol/h/g)") +
  geom_signif(comparisons = list(c("S", "W")),
              test = "t.test")

bg.treat.crop <- enzyme.clean %>% 
  group_by(crop,treatment) %>% 
  summarise(mean_activity = mean(activity),
            sd = sd(activity),
            count=n(),
            se=(sd/(sqrt(count)))) 

ggplot(data = bg.treat.crop, aes(x=treatment, y= mean_activity, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Beta-Glucosidase Activity by Treatment and Crop June 2021",x= "Treatment", y="Beta-Glucosidase Activity (µmol/h/g)") +
  geom_errorbar(aes(ymin=mean_activity-se, ymax=mean_activity+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Biochar", "Control", "Fertilizer"))

#_____________________________________________________________________________
bg.treat.site <- enzyme.clean %>%  
  group_by(site,treatment) %>% 
  summarise(mean_activity = mean(activity),
            sd = sd(activity),
            count=n(),
            se=(sd/(sqrt(count))))
  
ggplot(data = bg.treat.site, aes(x=site, y= mean_activity, fill = treatment)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Beta-Glucosidase Activity by Treatment and Site June 2021", x= "Site", y="Beta-Glucosidase Activity (µmol/h/g)") +
  geom_errorbar(aes(ymin=mean_activity-se, ymax=mean_activity+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Treatment",
                    labels=c("Biochar","Control","Fertilizer")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Goshen Road", "Jackson's Mill","Reedsville"))

#____________________________________________________________________________________
bg.site.crop <- enzyme.clean %>% 
  group_by(site,crop) %>% 
  summarise(mean_activity = mean(activity),
            sd = sd(activity),
            count=n(),
            se=(sd/(sqrt(count))))

ggplot(data = bg.site.crop, aes(x=site, y= mean_activity, fill = crop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title="Beta-Glucosidase Activity by Site and Crop June 2021", x= "Site", y="Beta-Glucosidase Activity (µmol/h/g)") +
  geom_errorbar(aes(ymin=mean_activity-se, ymax=mean_activity+se), width=0.25,
                size=1,position=position_dodge(.9), alpha=.3) +
  theme_classic() +
  scale_fill_manual(values=cbPalette,
                    name = "Crop",
                    labels=c("Switchgrass","Willow")) +
  scale_x_discrete(labels = c("Agronomy Farm", "Allstar #1", "Allstar #2","Goshen Road", "Jackson's Mill","Reedsville"))




