# ============================================================================
# MIXED MODEL ANALYSIS OF TYR WM DATA STRESS - ACCURACY and then RT
# ============================================================================

#read data into RM
setwd("M:/C_PhD/Stress/scripts/R") 
MyData <- read.csv(file="WMStress_MixedModel_IGUP.csv", header=TRUE, sep=",")
#MyData <- read.csv(file="WMStress_MixedModel_IG_UP.csv", header=TRUE, sep=",")

summary(MyData)
# load necessary libraries
library(lattice)
library(lme4)
library(ggplot2)
library(psych) # for the command describeBy()
library(car) # for the command scatterplot()
library(boot) # for the command boot.ci()
library(coefplot2) #for plotting effects
library(effects)
library(pbkrtest)
library(doBy) # for esticon.
#coding variables
# set contrasts to sum-to-zero (for unordered factors) and polynomial (for ordered factors)
options(contrasts = c("contr.sum", "contr.poly"))

#predictors as factors? Scaling or centering doesn't seem to make sense here;treat as conditions
MyData$Stress_f <- as.factor(MyData$Stress)
MyData$Day_f <- as.factor(MyData$Day)
MyData$Sex_f <- as.factor(MyData$Sex)
MyData$Condition_f <- as.factor(MyData$Condition)
#check contrasts -> 
contrasts(MyData$Stress_f) #Stress: 1 = no stress; -1 = stress
contrasts(MyData$Day_f) #Day: 1 = 1; -1 = 2
contrasts(MyData$Sex_f) #Sex: 0 =1; 1 = -1
contrasts(MyData$Condition_f) #Condition: -1 =  UPD; 1 = IGN

# PSS (perceived stress scale), lSpan, Cortisol diff of diff (log) % first I mean-centered; but scaling helps with model convergence
MyData$PSS_s <- scale(MyData$PSS, center = FALSE, scale = TRUE)
MyData$lSpan_s <- scale(MyData$lSpan, center = FALSE, scale = TRUE)
MyData$Cortisol_s <- scale(MyData$Cortisol, center = FALSE, scale = TRUE)
summary(MyData$Cortisol_s)

## first DV: here ACC
MyData$ACC_f <- as.factor(MyData$ACC)
#check contrast
contrasts(MyData$ACC_f)  #ACC: 1 = wrong, -1 = correct

# Model syntax
MyModel_ACC_big <- glmer(ACC_f ~ Stress_f * Condition_f * (Cortisol_s + PSS_s + Sex_f + lSpan_s) + (1 + Stress_f * Condition_f * Cortisol_s | SubNo),
                         family = binomial, data = MyData, control = glmerControl(optCtrl = list(maxfun = 100000)))

MyModel_ACC_small <- glmer(ACC_f ~ Stress_f * Condition_f + (1 + Stress_f*Condition_f | SubNo),
                                family = binomial, data = MyData, control = glmerControl(optCtrl = list(maxfun = 100000)))

MyModel_ACC_PSS_gender <- glmer(ACC_f ~ Stress_f * Condition_f * (PSS_s + Sex_f) + (1 + Stress_f * Condition_f | SubNo),
                            family = binomial, data = MyData, control = glmerControl(optCtrl = list(maxfun = 100000)))

#confint(cp_g_model2, level = 0.95, method = c('Wald', 'profile', 'boot'))
ConfInt_Wald95_small<- confint(MyModel_ACC_small, level = 0.95, method = 'Wald')
ConfInt_Wald99_small <- confint(MyModel_ACC_small, level = 0.99, method = 'Wald')

ConfInt_Wald95_PSS_gender <- confint(MyModel_ACC_PSS_gender, level = 0.95, method = 'Wald')
ConfInt_Wald99_PSS_gender <- confint(MyModel_ACC_PSS_gender, level = 0.99, method = 'Wald')

ConfInt_Wald95_big<- confint(MyModel_ACC_big, level = 0.95, method = 'Wald')
ConfInt_Wald99_big <- confint(MyModel_ACC_big, level = 0.99, method = 'Wald')

# save 
save(MyData,MyModel_ACC_small,file = "MPH_ACC_small.RData")
save(MyData,MyModel_ACC_PSS_gender,file = "MPH_ACC_PSS_gender.RData")
save(MyData,MyModel_ACC_big,file = "MPH_ACC_big.RData")

## second DV: here RT

#look at distribution of DV
densityplot(MyData$RTs)
MyData$RTs_log <- log(MyData$RTs)
densityplot(MyData$RTs_log)

MyModel_RT_big <- lmer(RTs_log ~ Stress_f * Condition_f * (Cortisol_s + PSS_s + Sex_f + lSpan_s) + (1 + Stress_f * Condition_f * Cortisol_s | SubNo),
                         data = MyData, control = lmerControl(optCtrl = list(maxfun = 100000)))
MyModel_RT_small <- lmer(RTs_log ~ Stress_f * Condition_f  + (1 + Stress_f * Condition_f | SubNo),
                         data = MyData, control = lmerControl(optCtrl = list(maxfun = 100000)))

ConfInt_Wald95_RTbig <- confint(MyModel_RT_big, level = 0.95, method = 'Wald')
ConfInt_Wald99_RTbig <- confint(MyModel_RT_big, level = 0.99, method = 'Wald')

ConfInt_Wald95_RTsmall <- confint(MyModel_RT_small, level = 0.95, method = 'Wald')
ConfInt_Wald99_RTsmall <- confint(MyModel_RT_small, level = 0.99, method = 'Wald')

# save 
save(MyData,MyModel_RT_small,file = "MPH_RT_small.RData")
save(MyData,MyModel_RT_big,file = "MPH_RT_big.RData")