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

#plot the results
library(lattice)
xyplot(fitted(lmer1)~soil.no3.n | site, groups=site, data=data, type=c('p','r'), auto.key=F)

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

#don't know what the hell i was doing here. don't pay attention to this.
#comparing models maybe? does having leaf pct n tell you anything sig diff than without?
lmer.leaf.n2<-lmer(asin(leaf.pct.n) ~ (1|site))
summary(lmer.leaf.n2)
anova(lmer.leaf.n, lmer.leaf.n2, test="F")


#below is a series of simple lmers looking at the effect of one factor on leaf N, 
  #with site as a random factor
  #i don't think this is really relevant to the big picture, 
  # but just want to get a handle on doing these

#NOx have an effect on leaf n?
lmer.leaf.n4<-lmer(asin(leaf.pct.n) ~ nox.yr.2013 + (1|site))
summary(lmer.leaf.n4)

#NO3-N have an effect on leaf n?
lmer.leaf.n8<-lmer(asin(leaf.pct.n) ~ soil.no3.n + (1|site))
summary(lmer.leaf.n8)

#NH4-N have an effect on leaf n?
lmer.leaf.n9<-lmer(asin(leaf.pct.n) ~ soil.nh4.n + (1|site))
summary(lmer.leaf.n9)

#percent urban have an effect on leaf n?
lmer.leaf.n10<-lmer(asin(leaf.pct.n) ~ pct.urban + (1|site))
summary(lmer.leaf.n10)

#NOx december have an effect on leaf n?
lmer.leaf.n11<-lmer(asin(leaf.pct.n) ~ nox.dec_2013 + (1|site))
summary(lmer.leaf.n11)

#percent herbivory have an effect on leaf n? {adding data doesn't matter}
lmer.leaf.n12<-lmer(asin(leaf.pct.n) ~ leaf.pct.herb + (1|site), data=data) 
summary(lmer.leaf.n12)

#dbh have an effect on leaf n?
lmer.leaf.n13<-lmer(asin(leaf.pct.n) ~ dbh.cm + (1|site))
summary(lmer.leaf.n13)


#this is looking at the effects of soil n, dbh, and the interaction of soil N and dbh
lmer.leaf.n5<- lmer(asin(leaf.pct.n) ~ dbh.cm * soil.total.n + (1|site), data=data)
summary(lmer.leaf.n5)
anova(lmer.leaf.n5)

# change plus signs to * 
#look at interactions, apply step to get rid of nonsignificant interactions first.
  #then it will continue with 

#Does everything have an effect on leaf n?
lmer.leaf.n6<-lmer(asin(leaf.pct.n) ~ pct.urban + soil.total.n + dbh.cm + leaf.pct.herb + (1|site))
summary(lmer.leaf.n6)
anova(lmer.leaf.n6)

#this adds in the p values
library(lmerTest)

#look up step further, the script below did not work 
stepeq<-step(lmer.leaf.n6)
stepeq

#investigating step further
step1<-step(lmer.leaf.n6, ddf = "Satterthwaite", type = 3, alpha.random = 0.1, alpha.fixed = 0.05, 
     reduce.fixed = TRUE, reduce.random = TRUE, fixed.calc = TRUE, lsmeans.calc = TRUE,
     difflsmeans.calc = TRUE, test.effs = NULL,  keep.effs = NULL, data=data)
summary(step1)
#seems I need to remove NA values in order for it to work
na.rm=TRUE

#attempted to use step, which performs backward elimination of 
#non-significant effects of linear mixed effects model
#seeems to have not worked because the random effect of site was not significant.

#05.03.15
#read in data with new column
data <- read.csv("../Data/survey.master.data.csv")
attach(data)

#subset data based on category urban or rural
urbandata <- subset (data, urban.rural->"1")

ruraldata <- subset (data, urban.rural=="2")
#why did I have to do an == in this line, but the arrow worked for 1?
attach(urbandata)

#correlation of vars
cor(pct.urban, leaf.pct.n)
cor(soil.no3.n, leaf.pct.n, use="complete")  #need to do use=complete so that it only
  #uses the values and not the NAs
na.rm=TRUE
as.factor(data$urban.rural)

#some dotcharts using all data splitting the vars by urban/rural category
dotchart(data$leaf.pct.n,
         groups=factor(data$urban.rural),
         ylab="Soil NO3-N", xlab="Leaf N",
         main="Cleveland dotplot", pch=data$soil.no3.n)

attach(data)
boxplot(leaf.pct.n∼factor(urban.rural),
        varwidth=TRUE, xlab="Urban / Rural",
        main="Boxplot of concentration conditional on\
        urban.rural", ylab="Leaf N", data=data)

boxplot(soil.no3.n∼factor(urban.rural),
        varwidth=TRUE, xlab="Urban / Rural",
        main="Boxplot of concentration conditional on\
        urban.rural", ylab="Soil NO3-N", data=data)

boxplot(soil.nh4.n∼factor(urban.rural),
        varwidth=TRUE, xlab="Urban / Rural",
        main="Boxplot of concentration conditional on\
        urban.rural", ylab="Soil NH4-N", data=data)

boxplot(leaf.pct.herb∼factor(urban.rural),
        varwidth=TRUE, xlab="Urban / Rural",
        main="Boxplot of concentration conditional on\
        urban.rural", ylab="Leaf Herbivory", data=data)


