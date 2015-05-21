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
leaf.herb.i0<-lmer(leaf.pct.herb ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + pct.urban+ 
                     dbh.cm + leaf.pct.n + (1|site), data=all.data)
summary(leaf.herb.i0)

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
