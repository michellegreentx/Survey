## Michelle Green
# 05.02.15
#michellegreentx@gmail.com
#script to investigate site average survey data

#attach site avg data
site.data <-read.csv("../Data/survey.master.site.avg.csv")
attach(site.data)

summary(site.data)

#set urban.rural as a factor
as.factor(urban.rural)

shapiro.test(dbh.cm)

shapiro.test(nox.dec_2013)

shapiro.test(leaf.pct.herb)
shapiro.test(asin(leaf.pct.herb))

shapiro.test(leaf.pct.n)
shapiro.test(asin(leaf.pct.n))

shapiro.test(pct.urban)
shapiro.test(asin(pct.urban))

shapiro.test(soil.no3.n)

shapiro.test(soil.nh4.n)

#attempting multiple regression with site avg data
mlr.site<-lm(leaf.pct.n ~ dbh.cm + nox.yr.2013 + soil.nh4.n + soil.no3.n, data=site.data)
summary(mlr.site)

mlr.site2<-lm(leaf.pct.n ~ dbh.cm + nox.yr.2013 + soil.nh4.n + soil.no3.n, groups=urban.rural, data=site.data)
summary(mlr.site2)

> xyplot(leaf.pct.n ~ soil.no3.n, data = site.data, group = urban.rural,
         +        auto.key = list(title = "Class", columns = 5)
         + )

mlr.site<-lm(leaf.pct.n ~ dbh.cm + nox.yr.2013 + soil.nh4.n + soil.no3.n, data=site.data)
summary(mlr.site)


#plot pairs of vars
plot(imp.site.data)
