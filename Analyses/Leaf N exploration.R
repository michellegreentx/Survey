#########################
# Michelle Green
# 05.06.15
# Investigation into effects on leaf pct N
# michellegreentx@gmail.com

all.data <- read.csv("../Data/survey.master.data.csv")
attach(all.data)

all.data[53,10]<- 0.00001

all.data[,15] <- log(all.data[,15])
all.data[,13] <- log(all.data[,13])
all.data[,10] <- log(all.data[,10])


leaf.n.1<-lmer(leaf.pct.n ~ nox.yr.2013 + soil.no3.n + soil.nh4.n + pct.urban + 
                     dbh.cm + leaf.pct.herb + (1|site), data=all.data)
summary(leaf.n.1)
