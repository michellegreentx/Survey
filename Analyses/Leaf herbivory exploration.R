# Michelle Green
#5.4.15
#michellegreentx@gmail.com

#Investigating relationships between leaf herbivory other variables

all.data <- read.csv("../Data/survey.master.data.csv")
attach(all.data)

#no missing data
nomissing.all.data<-(na.omit(all.data))

library(car)

# Assessing Outliers
outlierTest(leaf.herb.1) # Bonferonni p-value for most extreme obs
qqPlot(leaf.herb.16, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(leaf.herb.1) # leverage plots

# Normality of Residuals
# qq plot for studentized resid
qqPlot(leaf.herb.1, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(leaf.herb.1) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

#taking subset of important variables
myvars = c("leaf.pct.n", "leaf.pct.herb", "dbh.cm", "pct.urban", "nox.yr.2013", "soil.no3.n", "soil.nh4.n")
imp.data = all.data[myvars]

#plot pairs of vars
plot(imp.data)


#messing around with doing stepwise regression manually. so confusing.
#soil.p doesn't seem to be significant, but adding it gives a better adjusted R-square,
#but it also changes what's significant.
leaf.herb<-lm(asin(leaf.pct.herb) ~ soil.no3.n + asin(pct.urban), data=all.data)
summary(leaf.herb)

leaf.herb.1<-lm(asin(leaf.pct.herb) ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + asin(pct.urban) + 
                    dbh.cm + asin(leaf.pct.n) + soil.p, data=all.data)
summary(leaf.herb.1)

leaf.herb.12<-lm(asin(leaf.pct.herb) ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + asin(pct.urban) + 
                  dbh.cm + asin(leaf.pct.n), data=all.data)
summary(leaf.herb.12)

anova(leaf.herb.1, leaf.herb.12)

anova(leaf.herb.12, leaf.herb.1)
regr<-lm(leaf.pct.herb~leaf.pct.n)
summary(regr)
#same as previous model but without dbh.cm
leaf.herb.13=update(leaf.herb.12, .~.-dbh.cm)
summary(leaf.herb.13)

#plot residuals of leaf.herb.1
plot(fitted(leaf.herb.1), residuals(leaf.herb.1))
#residuals not great

#having R do backward stepwise regression for me
step(leaf.herb.1, direction="backward")

#wasn't working bc step was masked by the lmer function. 
#had to do "stats::" in order to make it go back to stats step
stats::step(leaf.herb.1, direction="forward")

stats::step(leaf.herb.1, direction="both")

#should be doing this instead of lm b/c i do expect there to be differences between the sites.
#lmer lets the intercept vary while keeping the slopes the same.
leaf.herb.14<-lmer(asin(leaf.pct.herb) ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + asin(pct.urban) + 
                   dbh.cm + asin(leaf.pct.n) + (1|site), data=all.data)
summary(leaf.herb.14)
# everything is significant except leaf.pct.n, but it's almost significant
# warning says that some predictor variables are on very different scales: consider rescaling

#attempting same thing without arcsine transforming didn't change anything of note

#tried using numbers instead of pcts for all % vars  - removed scale warnings.
leaf.herb.16<-lmer(qnorm(leaf.pct.herb) ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + pct.urban + 
                     dbh.cm + qnorm(leaf.pct.n) + (1|site), data=all.data)
summary(leaf.herb.16)

plot(fitted(leaf.herb.16), residuals(leaf.herb.16))
hist(leaf.pct.herb.num)

qnorm(leaf.pct.herb)

#needed to replace 0 with 0.00001 in order to do inverse transformation 
(leaf.pct.herb)->tleaf.pct.herb
all.data[53,10]<- 0.00001


sort(leaf.pct.herb)
qnorm(leaf.pct.herb)->tleaf.pct.herb

qpct.urban<-qnorm(pct.urban)
qleaf.pct.n<-qnorm(leaf.pct.n)
leaf.herb.17<-lmer(tleaf.pct.herb ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + qpct.urban+ 
                     dbh.cm + qleaf.pct.n + (1|site), data=all.data)
summary(leaf.herb.17)
plot(fitted(leaf.herb.17), residuals(leaf.herb.17))


#do qq plot of lmer
qqnorm(resid(leaf.herb.17)); qqline(resid(leaf.herb.17))


############
#influence analysis
############


all.data <- read.csv("../Data/survey.master.data.csv")
attach(all.data)
all.data[53,10]<- 0.00001

#transforming two columns so that it will run in line smoothly in the lmer
all.data[,15] <- log(all.data[,15])
all.data[,13] <- log(all.data[,13])
all.data[,10] <- log(all.data[,10])

#inverse transform leaf.pct.herb
all.data$leaf.pct.herb <- qnorm(all.data$leaf.pct.herb)

#install.packages("influence.ME")
library(influence.ME)
# initial model without any data points removed


plot(influence(leaf.herb.i0, obs=T), which="cook")

#to drop observations:
# Cook's rule of thumb = if it's beyond 4/#observations, so in this case it's 0.075
#there are 4 points beyond this, so start with removing the whackest (tree)

#code to remove one row - new data <- old data [-c(row to remove),]
all.data.out1<-all.data[-51,]

#now rerun lmer without that one row
leaf.herb.i1<-lmer(leaf.pct.herb ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + pct.urban+ 
                     dbh.cm + leaf.pct.n + (1|site), data=all.data.out1)

plot(influence(leaf.herb.i1, obs=T), which="cook")
#one far outlier(#46)

all.data.out2<-all.data.out1[-46,]
leaf.herb.i2<-lmer(leaf.pct.herb ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + pct.urban+ 
                     dbh.cm + leaf.pct.n + (1|site), data=all.data.out2)
plot(influence(leaf.herb.i2, obs=T), which="cook") 


all.data.out3<-all.data.out2[-46,]
leaf.herb.i3<-lmer(leaf.pct.herb ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + pct.urban+ 
                     dbh.cm + leaf.pct.n + (1|site), data=all.data.out3)
plot(influence(leaf.herb.i3, obs=T), which="cook") 

all.data.out4<-all.data.out3[-44,]
leaf.herb.i4<-lmer(leaf.pct.herb ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + pct.urban+ 
                     dbh.cm + leaf.pct.n + (1|site), data=all.data.out4)
plot(influence(leaf.herb.i4, obs=T), which="cook") 

all.data.out5<-all.data.out4[-45,]
leaf.herb.i5<-lmer(leaf.pct.herb ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + pct.urban+ 
                     dbh.cm + leaf.pct.n + (1|site), data=all.data.out5)
plot(influence(leaf.herb.i5, obs=T), which="cook") 


library(coefplot2)

#does this change coefficients?
coefplot2(c(leaf.herb.i0, leaf.herb.i1, leaf.herb.i2, leaf.herb.i3, leaf.herb.i4, leaf.herb.i5), 
            legend=TRUE) 
#no, keep outliers in.
 
summary(leaf.herb.i0)
summary(leaf.herb.i5)

###################
###################
###################


#05.20.15
#Leaf Herbivory lmer and Leaf N lmer without any data transformations,and using percents x 100 to get
#rid of scale warnings

all.data <- read.csv("../Data/survey.master.data.csv")
attach(all.data)
all.data[53,10]<- 0.00001

#initial working model
leafherb.nums<- lmer(leaf.pct.herb.num ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + pct.urban.num +
                       dbh.cm + leaf.pct.n.num + (1 | site))
summary(leafherb.nums)

plot(fitted(leafherb.nums), residuals(leafherb.nums))

#how correlated are NOx and urbanness?
nox.urban.lm <- lm(nox.yr.2013 ~ pct.urban)
summary(nox.urban.lm)
#R-squared = .677, highly significant

#removed pct.urban.num b/c it's highly correlated with nox.yr.2013
leafherb.nums1<- lmer(leaf.pct.herb.num ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + 
                       dbh.cm + leaf.pct.n.num + (1 | site))
summary(leafherb.nums1)

plot(fitted(leafherb.nums1), residuals(leafherb.nums1))

#removed nox.yr.2013 
leafherb.nums2<- lmer(leaf.pct.herb.num ~ pct.urban.num + soil.no3.n + soil.nh4.n + 
                       dbh.cm + leaf.pct.n.num + (1 | site))
summary(leafherb.nums2)

plot(fitted(leafherb.nums1), residuals(leafherb.nums1))



################################
################################
# Michelle Green
# 06.05.15
# Similar investigations with pcts asin(sqrt(x)) transformed

leafherb.1 <- lmer((asin(sqrt(leaf.pct.herb))) ~ (asin(sqrt(pct.urban))) + (asin(sqrt(leaf.pct.n))) + 
                          soil.no3.n + soil.nh4.n + soil.ca + (1|site))
summary(leafherb.1)
# soil.no3.n, soil.nh4.n, and soil.ca are all significant


###############################
###############################
# Michelle Green
# 06.10.15
# Looking into ANCOVA; leaf N as a fxn of Nox with pH/Ca as covariate
# using this to help me: http://www.uk.sagepub.com/dsur/study/DSUR%20Smart%20Alex-Labcoat%20Leni-Self%20Test%20Answers/DSUR%20Chapter%2011%20Web%20Material.pdf

# read in data
all.data <- read.csv("../Data/survey.master.data.csv")
attach(all.data)

all.data[53,11]<- 0.00001

all.data[,15] <- asin(sqrt(all.data[,16]))
all.data[,13] <- asin(sqrt(all.data[,14]))
all.data[,10] <- asin(sqrt(all.data[,11]))

library(ggplot2)

# run anova to see whether the groups differ in their levels of leaf herbivory
# without the covariate included
anovamodel1 <- aov(leaf.pct.herb ~ nox.yr.2013, data = all.data)
summary(anovamodel1)
# p value 0.119, no they do not differ

#boxplot - this is insane b/c each value of Ca is getting it's own plot
boxplot <- ggplot(all.data, aes(nox.yr.2013, leaf.pct.herb))
boxplot + geom_boxplot() + facet_wrap(~soil.ca) + labs(x="NOx", y="Leaf Herbivory")

#convert Ca into factor with 4 groups based on quartiles
soil.ca.gp<-factor(soil.ca.gp,levels = c(1:4), labels = c("1Q", "2Q", "3Q", "4Q"))

# try boxplot now
boxplot <- ggplot(all.data, aes(nox.yr.2013, leaf.pct.herb))
boxplot + geom_boxplot() + facet_wrap(~soil.ca.gp) + labs(x="NOx", y="Leaf Herbivory")
#not sure if this is a good plot or not? maybe converting nox into groups is good too?

# convert nox.yr.2013 into factor with 4 groups based on quartiles
nox.yr.2013.gp<-factor(nox.yr.2013.gp,levels = c(1:4), labels = c("Nox1Q", "Nox2Q", "Nox3Q", "Nox4Q"))

# try boxplot again
boxplot <- ggplot(all.data, aes(nox.yr.2013.gp, leaf.pct.herb))
boxplot + geom_boxplot() + facet_wrap(~soil.ca.gp) + labs(x="NOx", y="Leaf Herbivory")

library(car)
 
# doing Levene's test to see whether the variance in leaf herb varies across the
# interaction of different groups experiencing different Ca levels and the
# level of NOx 
leveneTest(leaf.pct.herb, interaction(soil.ca.gp, nox.yr.2013), center=median)
# p value = 0.1178, which means that the assumption of homogeneity of variance is NOT violated

# conduct anova to test whether Ca (covariate) is independent of NOx (indep var)
checkindepmodel<- aov(soil.ca.gp ~ nox.yr.2013, data=all.data)
summary(checkindepmodel)
# p value = 0.048, which means that the soil.ca.gp is significantly different among nox values
# this means it's inappropriate to use soil.ca.gp as a covariate ??

# but if I use the nox.yr.2013.gp, then it's not significant
checkindepmodel2<- aov(soil.ca.gp ~ nox.yr.2013.gp, data=all.data)
summary(checkindepmodel2)
# p value = 0.066

#trying ancova anyway with soil.ca.gp
contrasts(soil.ca.gp)<-(-1,1)
leafherbancova.1<-aov(leaf.pct.herb ~ nox.yr.2013.gp + soil.ca.gp, data=all.data)
Anova(leafherbancova.1, type = "III")
#Response: leaf.pct.herb
#Sum Sq Df F value  Pr(>F)   
#(Intercept)    0.001222  1  0.2272 0.63558   
#nox.yr.2013.gp 0.002545  1  0.4734 0.49450   
#soil.ca.gp     0.049812  1  9.2647 0.00366 **
#  Residuals      0.279584 52
# Sooo.... nox is not important but soil.ca is?

#trying without soil.ca in groups, even more significant
contrasts(soil.ca)<-(-1,1)
leafherbancova.2<-aov(leaf.pct.herb ~ nox.yr.2013 + soil.ca, data=all.data)
Anova(leafherbancova.2, type = "III")
#Response: leaf.pct.herb
#Sum Sq Df F value    Pr(>F)    
#(Intercept) 0.001242  1  0.2787    0.5998    
#nox.yr.2013 0.003951  1  0.8869    0.3507    
#soil.ca     0.094684  1 21.2556 2.648e-05 ***
  #Residuals   0.231636 52 

# Soooooo.... go back and add Ca into my original mixed model equation for herbivory?
leafherb.2 <- lmer(leaf.pct.herb ~ pct.urban + leaf.pct.n + 
                     soil.no3.n + soil.nh4.n + soil.ca + (1|site))
summary(leafherb.2)

#scatter plot of leaf herb as a function of soil ca
scatter <- ggplot(all.data, aes(soil.ca, leaf.pct.herb))
scatter + geom_point(size=3) + geom_smooth(method = "lm", alpha=0.1) +
  labs(x = "Soil Ca", y = "Leaf Herbivory")

#scatter plot of soil no3-n as a function of soil ca
scatter2 <- ggplot(all.data, aes(soil.ca, soil.no3.n))
scatter2 + geom_point(size=3) + geom_smooth(method = "lm", alpha=0.1) +
  labs(x = "Soil Ca", y = "Soil NO3-N")