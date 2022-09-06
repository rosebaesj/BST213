
# # # # # # # # # # # 
# BST HW
# Sunjeong Bae
# # # # # # # # # # # 

library("readxl")
library("ggplot2")
library("ggstatsplot")
library("dplyr")
library("rstatix")
lbw <- read_excel("lbw.xls")

# Assignment:  Using the data set on birth weights, use the continuous measure of birth weight, BWT, as the outcome (not the dichotomized one) and:
  
# 1. Find all the univariate predictors of birth weight.  Do not use regression. Use “simple” tests, like t-tests, Wilcoxon tests, ANOVA, correlations, etc.



summary(lbw)



### AGE ###
ggplot(lbw, aes(x=bwt))+
  geom_histogram(color="black", fill="grey")+
  theme_classic()+
  scale_y_continuous(expand = c(0,0.1))


ggplot(lbw, aes(x=age))+
  geom_histogram(color="black", fill="grey")+
  theme_classic()+
  scale_y_continuous(expand = c(0,0.1))


ggplot(lbw) +
  geom_point(aes(x = age, y = bwt)) +
  theme_classic()

cor.test(lbw$age, lbw$bwt, method= "spearman")

ggscatterstats(data=lbw,x=age, y=bwt, bf.message = F,
               smooth.line.args = list (size = 0, alpha = 0, method = "lm", formular = 0))




lbw_1 <- lbw[-189,]
cor.test(lbw_1$age, lbw_1$bwt, method= "spearman")

cor.test(lbw_1$age, lbw_1$bwt, method= "pearson")




###l

ggplot(lbw) +
  geom_point(aes(x = lwt, y = bwt)) +
  theme_classic()

ggplot(lbw, aes(x=lwt))+
  geom_histogram(color="black", fill="grey")+
  theme_classic()+
  scale_y_continuous(expand = c(0,0.1))

cor.test(lbw$lwt, lbw$bwt, method= "spearman")


ggscatterstats(data=lbw,x=lwt, y=bwt, bf.message = F,
               smooth.line.args = list (size = 0, alpha = 0, method = "lm", formular = 0))






lbw$race <- as.factor(lbw$race)

boxplot(data=lbw, bwt~race, jitter = T)

ggplot( data = lbw, aes(x=race, y=bwt )) +
  geom_boxplot( outlier.alpha = 0) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_minimal()+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  )

ggplot(data = lbw, aes(x=bwt, color=race, fill=race)) +
  geom_histogram(alpha=0.6, binwidth = 100) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Assigned Probability (%)") +
  facet_wrap(~race)

lbw_2 <- lbw

lbw_2 <- lbw %>%
  filter( id!=10 & id!=4)




lbw_aov <- aov(data = lbw, bwt~race)
summary(lbw_aov)
kruskal.test(data = lbw, bwt~race)


lbw_2_aov <- aov(data = lbw_2, bwt~race)
summary(lbw_2_aov)
kruskal.test(data = lbw_2, bwt~race)



### smoke 

lbw$smoke <- as.factor(lbw$smoke)
boxplot(data=lbw, bwt~smoke)

ggplot( data = lbw, aes(x=smoke, y=bwt )) +
  geom_boxplot( outlier.alpha = 0) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_minimal()+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  )

ggplot(data = lbw, aes(x=bwt, color=smoke, fill=smoke)) +
  geom_histogram(alpha=0.6, binwidth = 100) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  facet_wrap(~smoke)


t.test(data=lbw, bwt~smoke)

wilcox.test(lbw[lbw$smoke=='0',]$bwt, lbw[lbw$smoke=='1',]$bwt)



### ptl
lbw$ptl <- as.factor(lbw$ptl)




boxplot(data=lbw, bwt~ptl)
summary(lbw[c("bwt", "ptl")])

ggplot( data = lbw, aes(x=ptl, y=bwt )) +
  geom_boxplot( outlier.alpha = 0) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_minimal()+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  )

ggplot(data = lbw, aes(x=bwt, color=ptl, fill=ptl)) +
  geom_histogram(alpha=0.6, binwidth = 100) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  facet_wrap(~ptl)

kruskal.test(data = lbw, bwt~ptl)

lbw$ptl2 <- lbw$ptl
levels(lbw$ptl2)[levels(lbw$ptl2)==2] <- 1
levels(lbw$ptl2)[levels(lbw$ptl2)==3] <- 1

summary(lbw[c("bwt", "ptl2")])

ggplot( data = lbw, aes(x=ptl2, y=bwt )) +
  geom_boxplot( outlier.alpha = 0) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_minimal()+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  )

wilcox.test(lbw[lbw$ptl2=='0',]$bwt, lbw[lbw$ptl2=='1',]$bwt)



boxplot(data=lbw, bwt~ht)
t.test(data=lbw, bwt~ht)

boxplot(data=lbw, bwt~ui)
t.test(data=lbw, bwt~ui)



### ftv
lbw$ftv <- as.factor(lbw$ftv)
summary(lbw[c("bwt", "ftv")])

boxplot(data=lbw, bwt~ftv)
lbw$ftv <- as.factor(lbw$ftv)
aov_ftv <- aov(data = lbw, bwt~ftv)
summary(aov_ftv)
kruskal.test(data = lbw, bwt~ftv)

lbw$ftv2 <- lbw$ftv
levels(lbw$ftv2)[levels(lbw$ftv2)==4] <- 3
levels(lbw$ftv2)[levels(lbw$ftv2)==6] <- 3

summary(lbw[c("bwt", "ftv2")])

ggplot( data = lbw, aes(x=ftv2, y=bwt )) +
  geom_boxplot( outlier.alpha = 0) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_minimal()+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  )

kruskal.test(data = lbw, bwt~ftv2)





#### 2 #####
ggplot(lbw,aes(x = age, y = bwt)) +
  geom_point() +
  geom_smooth (method = "lm", formula = lbw$bwt~lbw$age)+
  theme_classic()

cor.test(lbw$age, lbw$bwt, method= "spearman")
cor.test(lbw$age, lbw$bwt, method= "pearson")








