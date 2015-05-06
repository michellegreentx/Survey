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
qqPlot(leaf.herb.1, main="QQ Plot") #qq plot for studentized resid 
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
leaf.herb.16<-lmer(leaf.pct.herb.num ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + pct.urban.num + 
                     dbh.cm + leaf.pct.n.num + (1|site), data=all.data)
summary(leaf.herb.16)
 