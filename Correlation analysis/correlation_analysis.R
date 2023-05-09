#Correlation analysis of MASBio data
#Salvador Grover
#March 11, 2023
library(dplyr)
library(corrplot)
library(ithir)
#If Ithir package not installed:
#library(devtools)
#install_bitbucket("brendo1001/ithir/pkg")

setwd("C:/Users/salgr/Box/Data/Correlation analysis")
#Load all 8 data sets
poxc <- read.csv(file = "poxc_all.csv")
resp <- read.csv(file = "resp_all.csv")
bg <- read.csv(file = "bg_all.csv")
per <- read.csv(file = "per_all.csv") %>% 
  rename(per_sqrt = sqrt_activity)
pom.weight <- read.csv(file = "pom_weight.csv") %>% 
  dplyr::select(-"POM...MAOM", -"subsample_weight", -"fraction") %>% 
  rename(pom_weight_bc = weight_bc)
maom.weight <- read.csv(file = "maom_weight.csv") %>% 
  dplyr::select(-"POM...MAOM", -"subsample_weight", -"fraction") %>% 
  rename(maom_cbrt_weight = cbrt_weight)
pom.cn <- read.csv(file = "pom_cn.csv") %>% 
  dplyr::select(-"POM...MAOM", -"subsample_weight", -"fraction") %>% 
  rename(pom_cn = CN)
maom.cn <- read.csv(file = "maom_cn.csv") %>% 
  dplyr::select(-"POM...MAOM", -"subsample_weight", -"fraction") %>% 
  rename(maom_cn = CN)

soil.dat <- poxc %>% 
  full_join(resp) %>% 
  full_join(bg) %>% 
  full_join(per) %>% 
  full_join(maom.weight) %>% 
  full_join(maom.cn) %>% 
  full_join(pom.weight) %>% 
  full_join(pom.cn) %>% 
  subset(date != "June 2021") %>% 
  select(poxc, resp_bc, bg, per_sqrt, maom_cbrt_weight, maom_cn_bc, pom_weight_bc, pom_log_cn) %>% 
  rename(POXC = poxc,
         "Microbial Respiration" = resp_bc,
         BG = bg,
         PER = per_sqrt,
         "MAOM Weight" = maom_cbrt_weight,
         "MAOM C:N" = maom_cn_bc,
         "POM Weight" = pom_weight_bc,
         "POM C:N" = pom_log_cn)


corr.soil <- cor(soil.dat[,],
                 use = "complete.obs",
                 method = "pearson")
corr.soil

pairs(soil.dat[,])

cor.mtest <- function(soil.dat, ...) {
  soil.dat <- as.matrix(soil.dat)
  n <- ncol(soil.dat)
  p.soil.dat<- matrix(NA, n, n)
  diag(p.soil.dat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(soil.dat[, i], soil.dat[, j], ...)
      p.soil.dat[i, j] <- p.soil.dat[j, i] <- tmp$p.value
    }
  }
  colnames(p.soil.dat) <- rownames(p.soil.dat) <- colnames(soil.dat)
  p.soil.dat
}

p.soil.dat <- cor.mtest(soil.dat)

corrplot(corr.soil, type = "upper", order = "original",
         method="circle",
         tl.col = "black", tl.srt = 45,
         p.mat = p.soil.dat, sig.level = 0.05,
         addCoef.col = "black")

cor.fig <- corrplot(corr.soil, type = "upper", order = "original",
         method="circle",
         tl.col = "black", tl.srt = 45,
         p.mat = p.soil.dat, sig.level = 0.05, insig = "blank",
         addCoef.col = "black")

#ggsave("cor_plot.jpeg", plot = cor.fig, width = 180, height = 100, units = "mm", dpi = 300, type = "cairo")
#pdf("Hclust_corplot_mtcars.pdf", width=5, height=5)

#Linear Regression
all.dat <- poxc %>% 
  full_join(resp) %>% 
  full_join(bg) %>% 
  full_join(per) %>% 
  full_join(maom.weight) %>% 
  full_join(maom.cn) %>% 
  full_join(pom.weight) %>% 
  full_join(pom.cn) %>% 
  subset(date != "June 2021") %>% 
  select(poxc, resp_bc, bg, per_sqrt, maom_cbrt_weight, maom_cn_bc, pom_weight_bc, pom_log_cn) %>% 
  na.omit()

mod.1 <- lm(resp_bc ~ poxc + per_sqrt + bg  + maom_cn_bc + pom_weight_bc + maom_cbrt_weight +pom_log_cn, data = all.dat, y = T, x = T)
summary(mod.1)
anova(mod.1)

mod.1 <- lm(resp_bc ~ poxc  + bg  + maom_cn_bc + pom_weight_bc  +pom_log_cn, data = all.dat, y = T, x = T)
summary(mod.1)
anova(mod.1)
#mod.1 <- lm(resp_bc ~ poxc + bg + maom_cbrt_weight + maom_cn_bc + pom_weight_bc, data = soil.dat, y = T, x = T)
#summary(mod.1)
#anova(mod.1)

residuals(mod.1)
predict(mod.1)

plot(mod.1$y, mod.1$fitted.values, xlim= c(0,2), ylim=c(0,2))
abline(a = 0, b = 1, col="red")

soil.dat.clean <- all.dat %>% 
  na.omit()

plot(predict(mod.1), soil.dat.clean$resp_bc, xlim= c(0,2), ylim=c(0,2))
abline(a = 0, b = 1, col="red")

goof(mod.1$y, mod.1$fitted.values)



##_extra work spcae_________________________________________________________________________________________________________________________
cor.table <- as.data.frame(corr.soil)
mean(cor.table$poxc)
mean(cor.table$resp_bc)
mean(cor.table$bg)
mean(cor.table$per_sqrt)
mean(cor.table$maom_cbrt_weight)
mean(cor.table$maom_cn_bc)
mean(cor.table$pom_weight_bc)
mean(cor.table$pom_log_cn)

cor.test(soil.dat$maom_cbrt_weight, soil.dat$poxc)
cor.test(soil.dat$maom_cbrt_weight, soil.dat$bg)
cor.test(soil.dat$maom_cbrt_weight, soil.dat$per_sqrt)
cor.test(soil.dat$maom_cbrt_weight, soil.dat$resp_bc)
cor.test(soil.dat$maom_cbrt_weight, soil.dat$maom_cn_bc)
cor.test(soil.dat$maom_cbrt_weight, soil.dat$pom_weight_bc)
cor.test(soil.dat$maom_cbrt_weight, soil.dat$pom_log_cn)

mod.2 <- lm(maom_cbrt_weight ~ poxc + per_sqrt + bg + resp_bc + maom_cn_bc  + pom_log_cn, data = all.dat, y = T, x = T)
summary(mod.2)
anova(mod.2)

#mod.2 <- lm(maom_cbrt_weight ~ poxc + bg + resp_bc + maom_cn_bc + pom_weight_bc, data = all.dat, y = T, x = T)
#summary(mod.2)
#anova(mod.2)

mod.2 <- lm(maom_cbrt_weight ~ poxc + resp_bc + maom_cn_bc, data = all.dat, y = T, x = T)
summary(mod.2)
anova(mod.2)

residuals(mod.2)
predict(mod.2)

plot(mod.2$y, mod.2$fitted.values, xlim= c(0,2), ylim=c(0,2))
abline(a = 0, b = 1, col="red")


plot(predict(mod.2), soil.dat.clean$maom_cbrt_weight, xlim= c(0,2), ylim=c(0,2))
abline(a = 0, b = 1, col="red")

goof(mod.2$y, mod.2$fitted.values)

mod.3 <- lm(pom_weight_bc ~ poxc + per_sqrt + bg + resp_bc + maom_cn_bc + maom_cbrt_weight + pom_log_cn, data = soil.dat, y = T, x = T)
summary(mod.3)
anova(mod.3)

residuals(mod.3)
predict(mod.3)

plot(mod.3$y, mod.3$fitted.values, xlim= c(0,2), ylim=c(0,2))
abline(a = 0, b = 1, col="red")


plot(predict(mod.3), soil.dat.clean$pom_weight_bc, xlim= c(0,2), ylim=c(0,2))
abline(a = 0, b = 1, col="red")
