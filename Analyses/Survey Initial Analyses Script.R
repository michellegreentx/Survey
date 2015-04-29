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

lmer2<-lmer(asin(leaf.pct.herb) ~ leaf.pct.n + (1|site))
summary(lmer2)

#looking at pct herbivory as fxn of leaf n and urbanness
lmer3<-lmer(asin(leaf.pct.herb) ~ leaf.pct.n + asin(pct.urban) + (1|site))
summary(lmer3)
# 6% chance pct urban would have an effect on herbivory assuming that % urban has no effect

#install.packages("coefplot2", repos="http://www.math.mcmaster.ca/bolker/R", type="source")
#plotting lmer3, not sure how to do this, need to look into it more
library(coefplot2)
coefplot2(lmer3, var.idx=2:4)
help(coefplot2)

#lmer for looking at effect of leaf soil no3 on leaf pct n with site as random factor
lmer.leaf.n<-lmer(asin(leaf.pct.n) ~ soil.no3.n + (1|site))
summary(lmer.leaf.n)

#comparing models maybe? need to check with ashley. does having leaf pct n tell you 
  #anything sig diff than without?
lmer.leaf.n2<-lmer(asin(leaf.pct.n) ~ (1|site))
summary(lmer.leaf.n2)
anova(lmer.leaf.n, lmer.leaf.n2, test="F")

#NOx have an effect on leaf n?
lmer.leaf.n4<-lmer(asin(leaf.pct.n) ~ nox.yr.2013 + (1|site))
summary(lmer.leaf.n4)

#Does everything have an effect on leaf n?
lmer.leaf.n5<-lmer(asin(leaf.pct.n) ~ pct.urban + soil.total.n + dbh.cm + leaf.pct.herb + (1|site))
summary(lmer.leaf.n5)
