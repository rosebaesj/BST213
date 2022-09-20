# Julie Lee
# Sept 7, 2022
# BST 213 HW2
# make these packages and their associated functions
# available to use in this script
library(readxl)
library(broom)
library(tidyverse)
library(rstatix)
library(dplyr)
library(mvtnorm)
library(sasLM) # for running like SAS
# clear R's brain
rm(list = ls())
lbw <- read_excel("Desktop/BST 213/lbw.xls")
view(lbw)
summary(lbw)

#tidy data
#race 1 = White, race 2 = Black, race 3 = Other
#history of hypertension 0 = no, 1 = yes
lbw <- lbw |> mutate(ht_des = case_when(ht == 0~0,
                                        ht == 1~1),
                     race_des = case_when(race == 1~0, race == 2~1, race ==3~2))
lbw$ht_des <- as.factor(lbw$ht_des)
lbw$race_des <- as.factor(lbw$race_des)
#make White as reference (race_des) -> biggest sample size

#1.	Use PROC REG to run a model that includes maternal weight as 
#a continuous predictor of birth weight.  
#Interpret the regression portion (i.e., the bottom) 
#of the printout.   Compare the regression results to a
#Pearson correlation coefficient. 
REG(bwt~lwt, Data = lbw) #how SAS runs it
weightmodel <- lm(bwt~lwt, data=lbw)
summary(weightmodel)
fit <- lm(bwt~lwt, data=lbw)
tidy(fit, conf.int=TRUE)
cor.test(lbw$lwt,lbw$bwt)
#Use PROC GLM to run the same model.  
#Interpret and compare the results to the regression results 
#from above.
GLM(bwt~lwt, Data = lbw, BETA = TRUE, EMEAN = TRUE) # for SAS as GLM
fit2 <- glm(bwt~lwt, data=lbw)
summary(fit2)
tidy(fit2, conf.int=TRUE)

#2.	Use PROC REG to run a model that includes history of 
#hypertension as a binary predictor of birth weight. 
#Interpret the regression portion of the printout. 
#Compare the regression results to a 2-sample t-test.
REG(bwt ~ ht_des, Data = lbw) # run as SAS
fit_ht <- lm(bwt~ht_des, data=lbw)
summary(fit_ht)
tidy(fit_ht, conf.int=TRUE)
t.test(bwt~ht_des, data=lbw, var.equal = TRUE) # need to specify equal variance
#Use PROC GLM to run the same model.  Interpret and 
#compare the results to the regression results from above.
GLM(bwt~ht_des, Data = lbw, BETA = TRUE, EMEAN = TRUE, conf.level = 0.95)
fit_ht2 <- glm(bwt~ht_des, data=lbw)
summary(fit_ht2)
tidy(fit_ht2, conf.int=TRUE)

#33.	Use PROC REG to run a model that includes race 
#(in 3 categories) as a categorical predictor of birth weight.  
#Interpret the regression portion of the printout. 
#Compare the regression results to a standard ANOVA analysis. 
REG(bwt~race_des, Data = lbw) # run as SAS
fit_race <- lm(bwt~race_des, data=lbw)
summary(fit_race)
#changing the reference race to Other Race (bigger sample compare to black)
#White = 1, Black = 2, Other = 0
lbw <- lbw |> mutate(race_des2 = case_when(race == 1~1, race == 2~2, race ==3~0))
lbw$race_des2 <- as.factor(lbw$race_des2)
#run the regression again
REG(bwt~race_des2, Data = lbw) # run as SAS
fit_race2 <- lm(bwt~race_des2, data=lbw)
summary(fit_race2)
tidy(fit_race, conf.int=TRUE)
racebwt <- aov(bwt ~ race_des, data = lbw)
summary(racebwt)
#Based on an F-statistic of 4.978 corresponding to a p-value of 0.00783, I reject
#the null hypothesis that there is no difference in mean birth weight among 
#infants in different racial groups. 
#To identify which groups may be different from one another, I can perform 
#pair-wise t-tests. 
pairwise.t.test(lbw$bwt, lbw$race_des, p.adjust.method="bonferroni")
#Conclusions: Based on pair-wise t-tests of the three racial categories 
#compared, there are differences in mean birth weight between infants born 
#to white and black mothers, and white and other mothers. 

#Use PROC GLM to run the same model.  Interpret 
#and compare the results to the regression results from above.
GLM(bwt~race_des, Data = lbw, BETA = TRUE, EMEAN = TRUE)
GLM(bwt~race_des2, Data = lbw, BETA = TRUE, EMEAN = TRUE) # run it with other = 0
fit_race2 <- glm(bwt~race_des, data=lbw)
tidy(fit_race2, conf.int=TRUE)