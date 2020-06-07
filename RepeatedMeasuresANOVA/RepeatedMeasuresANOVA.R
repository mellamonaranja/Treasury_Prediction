#Repeated Measures ANOVA(Analysis of Variance)#

#Import the data
distance=read.csv("C:/dis3years2.csv",header = TRUE, na.strings = '.')

View(distance)
str(distance)

attach(distance)

#Change factor to vector
distance$year=factor(distance$year,levels = c(1,2,3),labels=c('2015','2016','2017'))

#Get the Descriptive Statistic
describeBy(year,Distance,mat=T)

#Create the graph(histogram, boxplot)
boxplot(Distance~year,data=distance,ylap='Distance',xlap='Year',col=topo.colors(5))
hist(distance$Distance,breaks = 10,col=topo.colors(5))

#Verify ANOVA(Analysis of Variance)
distanceResult=aov(Distance~year+Error(id/year),data=distance)

#Get the ANOVA result easily
summary(distanceResult)

detach(distance)

#Appendix, Multicomparision Test : TukeyHSD
TukeyResult=aov(Distance~year,data=distance)
TukeyHSD(TukeyResult)
plot(TukeyHSD(TukeyResult))
