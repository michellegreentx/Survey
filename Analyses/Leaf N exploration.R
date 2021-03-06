#########################
# Michelle Green
# 05.06.15
# Investigation into effects on leaf pct N
# michellegreentx@gmail.com

all.data <- read.csv("../Data/survey.master.data.csv")
attach(all.data)

all.data[53,10]<- 0.00001
#these column numbers need to be changed b/c i added a couple of columns
all.data[,15] <- log(all.data[,15])
all.data[,13] <- log(all.data[,13])
all.data[,10] <- log(all.data[,10])


leaf.n.1<-lmer(leaf.pct.n ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + pct.urban + 
                     dbh.cm + leaf.pct.herb + (1|site), data=all.data)
summary(leaf.n.1)

##########################
# Did at some point in between above and below

#####
#initial working model for leaf n
leafn.nums<- lmer(leaf.pct.n.num ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + leaf.pct.herb.num +
                    dbh.cm + pct.urban.num + (1|site))
summary(leafn.nums)
plot(fitted(leafn.nums), residuals(leafn.nums))

#with pct.urban.num removed
leafn.nums1<- lmer(leaf.pct.n.num ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + leaf.pct.herb.num +
                     dbh.cm + (1|site))
summary(leafn.nums1)
plot(fitted(leafn.nums1), residuals(leafn.nums1))

#with nox.yr.2013 removed
leafn.nums2<- lmer(leaf.pct.n.num ~ pct.urban.num + soil.no3.n + soil.nh4.n + leaf.pct.herb.num +
                     dbh.cm + (1|site))
summary(leafn.nums2)
plot(fitted(leafn.nums2), residuals(leafn.nums2))



########################
# Michelle green
# 06.03.15
# Further investigation into effects on Leaf N

# Use values less than one for all percents, but arcsin(sqrt(x)) transform them.


all.data <- read.csv("../Data/survey.master.data.csv")
attach(all.data)

all.data[53,10]<- 0.00001

all.data[,15] <- asin(sqrt(all.data[,15]))
all.data[,13] <- asin(sqrt(all.data[,13]))
all.data[,10] <- asin(sqrt(all.data[,10]))

#mixed effects model with site as random factor
leaf.n.1 <- lmer(leaf.pct.n ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + pct.urban + dbh.cm + leaf.pct.herb +
                   soil.ca + (1|site))
#get warning messages about variables being on vary different scales
summary(leaf.n.1)
# nothing significantly contributes to leaf N

#attempt with pcts >1, untransformed to see if it gets rid of warnings about variables on diff scales
all.data.nums <- read.csv("../Data/survey.master.data.csv")

#mixed effects model with site as random factor
leaf.n.1.num <- lmer(all.data.nums$leaf.pct.n.num ~ all.data.nums$nox.yr.2013 + all.data.nums$soil.no3.n + 
                       all.data.nums$soil.nh4.n + all.data.nums$pct.urban.num + all.data.nums$dbh.cm + 
                       all.data.nums$leaf.pct.herb.num + all.data.nums$soil.ca + (1|site))
summary(leaf.n.1.num)
#still nothing significant and still get warning messages about variables