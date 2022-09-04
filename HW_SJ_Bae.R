
# # # # # # # # # # # 
# BST HW
# Sunjeong Bae
# # # # # # # # # # # 

library("readxl")
library("ggplot2")
lbw <- read_excel("lbw.xls")

# Assignment:  Using the data set on birth weights, use the continuous measure of birth weight, BWT, as the outcome (not the dichotomized one) and:
  
# 1. Find all the univariate predictors of birth weight.  Do not use regression. Use “simple” tests, like t-tests, Wilcoxon tests, ANOVA, correlations, etc.

summary(lbw)

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




ggplot(lbw) +
  geom_point(aes(x = lwt, y = bwt)) +
  theme_classic()

ggplot(lbw, aes(x=lwt))+
  geom_histogram(color="black", fill="grey")+
  theme_classic()+
  scale_y_continuous(expand = c(0,0.1))

cor.test(lbw$lwt, lbw$bwt, method= "spearman")





lbw$race <- as.factor(lbw$race)

boxplot(data=lbw, bwt~race)

lbw_aov <- aov(data = lbw, bwt~race)
summary(lbw_aov)
kruskal.test(data = lbw, bwt~race)

boxplot(data=lbw, bwt~smoke)
t.test(data=lbw, bwt~smoke)

boxplot(data=lbw, bwt~ptl)
kruskal.test(data = lbw, bwt~ptl)

boxplot(data=lbw, bwt~ht)
t.test(data=lbw, bwt~ht)

boxplot(data=lbw, bwt~ui)
t.test(data=lbw, bwt~ui)

boxplot(data=lbw, bwt~ftv)
lbw$ftv <- as.factor(lbw$ftv)
aov_ftv <- aov(data = lbw, bwt~ftv)
summary(aov_ftv)
kruskal.test(data = lbw, bwt~ftv)


#### 2 #####
ggplot(lbw,aes(x = age, y = bwt)) +
  geom_point() +
  geom_smooth (method = "lm", formula = lbw$bwt~lbw$age)+
  theme_classic()

cor.test(lbw$age, lbw$bwt, method= "spearman")
cor.test(lbw$age, lbw$bwt, method= "pearson")








