# Michelle Green
# 04.29.15
# michellegreentx@gmail.com
# Initial script to explore Survey Data

setwd("C:/Users/Michelle/Dropbox/Research/Repos/Survey/Analyses")

#read in data
data <- read.csv("../Data/Survey Master Data CSV.csv")

#load mixed model and multilevel package
library(lme4)

#load arm package
library(arm)

#attaches variables so I can just name them
attach(data)

#basic linear model test
fitlm<-lm(asin(leaf.pct.herb) ~ soil.no3.n + dbh.cm + soil.no3.n * dbh.cm)

#genearl linear model
fitglm<-glm(asin(leaf.pct.herb) ~ soil.no3.n + dbh.cm + soil.no3.n * dbh.cm)
fitglm2<-glm(asin(leaf.pct.herb) ~ soil.no3.n + dbh.cm)

#tells me the important info about the model
summary(fitlm)
summary(fitglm)

#how to compare models, tells you if more complicated model is sig diff from simpler;
#F=stat you can look up, Pr(>F)= probability that you'll get something greater than F
anova(fitglm, fitglm2, test="F")
#Results: Model 1: asin(leaf.pct.herb) ~ soil.no3.n + dbh.cm + soil.no3.n * dbh.cm
#Model 2: asin(leaf.pct.herb) ~ soil.no3.n + dbh.cm
#Resid. Df Resid. Dev Df   Deviance      F Pr(>F)
#1        49    0.32289                            
#2        50    0.32521 -1 -0.0023126 0.3509 0.5563

#lmer practice
lmer1<-lmer(asin(leaf.pct.herb) ~ soil.no3.n + (1|site))
summary(lmer1)


lmer3<-lmer(asin(leaf.pct.herb) ~ leaf.pct.n + asin(pct.urban) + (1|site))
summary(lmer3)
# 6% chance pct urban would have an effect on herbivory assuming that % urban has no effect

#install.packages("coefplot2", repos="http://www.math.mcmaster.ca/bolker/R", type="source")

#plotting lmer3
library(coefplot2)
coefplot2(lmer3, var.idx=2:4)
help(coefplot2)
